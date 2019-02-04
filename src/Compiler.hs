{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Compiler
  ( someFunc
  ) where

-- https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html

import           Control.Applicative   hiding (many, (<|>))
import           Control.Monad         (void, when)
import           Data.Char
import           Data.Bits
import           Data.List (foldl')
import           Data.Maybe
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Void
import           Data.Word             (Word8, Word32)
import           Text.Megaparsec
import           Text.Megaparsec.Char
-- import Text.Megaparsec.Combinator
import qualified Text.Megaparsec.Lexer as L

type Parser = Parsec () String

class ToBits a where
  toBits :: a -> Word32

data Instruction = Instruction
  { aMux         :: Maybe Bool
  , cond         :: Maybe Cond
  , alu          :: Maybe Alu
  , shifter      :: Maybe Shifter
  , mbr          :: Maybe Bool
  , mar          :: Maybe Bool
  , memoryOp     :: Maybe MemoryOp
  , enableSBus   :: Maybe Bool
  , sBus         :: Maybe WritableReg
  , bBus         :: Maybe Reg
  , aBus         :: Maybe Reg
  , address      :: Maybe Addr
  , iLabel       :: Maybe String
  , comment      :: Maybe String
  } deriving (Eq, Show)

instance ToBits a => ToBits (Maybe a) where
  toBits (Just a) = toBits a
  toBits Nothing = 0

instance ToBits Instruction where
  toBits Instruction{..} = foldl' (.|.) 0
    [ bAMux alu `shift` 31
    , toBits cond
    , toBits alu
    , toBits shifter
    , b mbr 24
    , b mar 23
    , toBits memoryOp `shift` 21
    , b enableSBus 20
    , toBits sBus `shift` 16
    , toBits bBus `shift` 12
    , toBits aBus `shift` 8
    , aluBusBits alu `shift` 8 -- TODO remove?
    , toBits address
    ]
    where
      b :: Maybe Bool -> Int -> Word32
      b (Just True) n = 1 `shift` n
      b _ _ = 0

      aluBusBits :: Maybe Alu -> Word32
      aluBusBits (Just (Id r)) = toBits r
      aluBusBits (Just (Not r)) = toBits r
      aluBusBits (Just (Plus a b)) = toBits b `shift` 4 .|. toBits a
      aluBusBits (Just (And a b)) = toBits b `shift` 4 .|. toBits a
      aluBusBits _ = 0

      bAMux :: Maybe Alu -> Word32
      bAMux (Just (Id MBR)) = 1
      bAMux (Just (Not MBR)) = 1
      bAMux (Just (Plus MBR _)) = 1
      bAMux (Just (And MBR _)) = 1
      bAMux _ = 0


emptyInstruction :: Instruction
emptyInstruction = Instruction Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


data Alu
  = Id Reg
  | Plus Reg Reg
  | And Reg Reg
  | Not Reg
  deriving (Eq, Show)

instance ToBits Alu where
  toBits (Id _) = 0
  toBits (Plus _ _) = 1 `shift` 27
  toBits (And _ _) = 2 `shift` 27
  toBits (Not _) = 3 `shift` 27

pAlu :: Parser Alu
pAlu = tryChoicesP
  [ Not <$> (symbol "~" *> pReg)
  , binary Plus "+"
  , binary And "&"
  , Id <$> pReg
  ]
  where
    binary alu s = do
      a <- pReg
      op <- symbol s
      b <- pReg
      return $ alu a b

data Shifter
  = ShiftLeft
  | ShiftRight
  | ShiftNothing
  deriving (Eq, Show)

instance ToBits Shifter where
  toBits ShiftLeft = 1 `shift` 25
  toBits ShiftRight = 2 `shift` 25
  toBits ShiftNothing = 0

data Cond
  = JumpNegative
  | JumpZero
  | Jump
  deriving (Eq, Show)

instance ToBits Cond where
  toBits JumpNegative = 1 `shift` 29
  toBits JumpZero = 2 `shift` 29
  toBits Jump = 3 `shift` 29

parseCond :: Parser Cond
parseCond = try pJump <|> try pJumpZero <|> pJumpNegative
  where
    goto = symbol "goto"
    sIf = symbol "if"
    pJump = goto >> return Jump
    pJumpZero = sIf >> symbol "Z" >> goto >> return JumpZero
    pJumpNegative = sIf >> symbol "N" >> goto >> return JumpNegative

data Addr
  = LineAddr Int
  | LabelAddr String
  deriving (Eq, Show)

instance ToBits Addr where
  toBits (LineAddr i) = fromIntegral i
  toBits (LabelAddr _) = 0 -- TODO

parseAddr :: Parser Addr
parseAddr = try pLineAddr <|> pLabelAddr
  where
    pLineAddr = LineAddr <$> decimal
    pLabelAddr = LabelAddr <$> (char '.' *> alphaNum)


-- | If this is present the memory select bit must be set
newtype MemoryOp = MemoryOp
  Bool -- ^ True read, False write
  deriving (Eq, Show)

instance ToBits MemoryOp where
  toBits (MemoryOp True) = 3
  toBits (MemoryOp False) = 1

data Reg
  = Zero
  | One
  | MinusOne
  | MAR -- TODO remove?
  | MBR -- TODO remove?
  | WR WritableReg
  deriving (Eq, Show)

instance ToBits Reg where
  toBits Zero = 0
  toBits One = 1
  toBits MinusOne = 2
  toBits MAR = 0 -- TODO
  toBits MBR = 0 -- TODO
  toBits (WR wr) = toBits wr

pReg :: Parser Reg
pReg = tryChoicesP $ fmap optionalParens
  [ symbol "0" >> return Zero
  , symbol "1" >> return One
  , symbol "-1" >> return MinusOne
  , symbol "MAR" >> return MAR
  , symbol "MBR" >> return MBR
  , WR <$> pWritableReg
  ]

data WritableReg
  = PC
  | R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  | R9
  | R10
  | AC
  deriving (Eq, Show)

instance ToBits WritableReg where
  toBits PC = 3
  toBits R0 = 4
  toBits R1 = 5
  toBits R2 = 6
  toBits R3 = 7
  toBits R4 = 8
  toBits R5 = 9
  toBits R6 = 10
  toBits R7 = 11
  toBits R8 = 12
  toBits R9 = 13
  toBits R10 = 14
  toBits AC = 15

pWritableReg :: Parser WritableReg
pWritableReg = tryChoicesP
  [ symbol "PC" >> return PC
  , symbol "R0" >> return R0
  , symbol "R1" >> return R1
  , symbol "R2" >> return R2
  , symbol "R3" >> return R3
  , symbol "R4" >> return R4
  , symbol "R5" >> return R5
  , symbol "R6" >> return R6
  , symbol "R7" >> return R7
  , symbol "R8" >> return R8
  , symbol "R9" >> return R9
  , symbol "R10" >> return R10
  , symbol "AC" >> return AC
  ]

{- Lexer -}

spaceConsumer :: Parser ()
spaceConsumer = L.space (void $ char ' ') empty empty

lexeme  :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = lexeme . between (symbol "(") (symbol ")")

optionalParens :: Parser a -> Parser a
optionalParens a = try (parens a) <|> try a

alphaNum :: Parser String
alphaNum = lexeme $ many (satisfy isAlphaNum)

decimal :: Parser Int
decimal = fromInteger <$> lexeme L.decimal

semicolon :: Parser ()
semicolon = void $ symbol ";"

restOfLine :: Parser String
restOfLine = many $ noneOf "\r\n"

tryChoicesP :: [Parser a] -> Parser a
tryChoicesP = lexeme . choice . fmap optionalParens

{- Parser -}
m16Parser :: Parser [Instruction]
m16Parser = between space eof (many instruction)

instruction :: Parser Instruction
instruction = instruction' emptyInstruction
  where
    instruction' :: Instruction -> Parser Instruction
    instruction' i = (eol >> return i) <|> (partOfInstruction i >>= instruction')

-- | An instruction consists of multiple parts that may be separated by a semicolon.
-- | This is an example for a complicated instruction:
-- |
-- | R4 <- lsh (1 + R3); MBR <- lsh (1 + R3); MAR <- 1; wr      ; if Z goto .label1; :label2 # this is a comment
-- | assignSBus        ; assignMBR          ; setMAR  ; memoryOp; jump             ; label   comment
-- |
-- |
-- | Notes:
-- |  - assignSBus and assignMBR must assign the result of the same operation
-- \  - setMAR writes the B bus into MAR
-- |  - memoryOp sets the read/write bit and enables the memory select bit
-- |  - jumps can depend on flags from the ALU, the shifter does not affect them!
-- |  - everything after a '#' is a comment
-- |  - the :label can only be followed by whitespace or a comment
-- |
partOfInstruction :: Instruction -> Parser Instruction
-- partOfInstruction i = foldl1 (<|>) $ fmap i [pComment, pMemoryOp]
partOfInstruction i = pComment i
                    <|> try (pSkipSemicolon i)
                    <|> try (pAssignMAR i)
                    <|> try (pAssignMBR i)
                    <|> try (pAssignReg i)
                    <|> try (pNoAssign i)
                    <|> try (pMemoryOp i)
                    <|> try (pJump i)
                    <|> try (pLabel i)
                    <|> fail "Unable to parse instrction"


pComment :: Instruction -> Parser Instruction
pComment = \case
  Instruction { comment = Just _ } -> fail "Duplicate comment"
  i -> do
    c <- char '#' *> restOfLine
    return i { comment = Just c }

pSkipSemicolon :: Instruction -> Parser Instruction
pSkipSemicolon i = do
  lexeme $ void (char ';')
  return i

pAssignMAR :: Instruction -> Parser Instruction
pAssignMAR i@Instruction {alu} = do
  void $ symbol "MAR"
  void $ symbol "<-"
  reg <- pReg
  return i { mar = Just True, bBus = Just reg }


pAssignMBR :: Instruction -> Parser Instruction
pAssignMBR i@Instruction {sBus} = do
  void $ symbol "MBR" >> symbol "<-"
  pAluShifter i { mbr = Just True }


pAssignReg :: Instruction -> Parser Instruction
pAssignReg i@Instruction {sBus} = do
  reg <- pWritableReg <* symbol "<-"
  case sBus of
    Just r | r /= reg -> fail "sBus already set"
    _ -> pAluShifter i { sBus = Just reg, enableSBus = Just True }

pNoAssign :: Instruction -> Parser Instruction
pNoAssign i@Instruction {alu} = do
  pa <- pAlu
  case alu of
    Just a | a /= pa -> fail "alu already set"
    _ -> return i {alu = Just pa}


pMemoryOp :: Instruction -> Parser Instruction
pMemoryOp = \case
  Instruction { memoryOp = Just _ } -> fail "Duplicate memory operation"
  i -> rd <|> wr
    where
      result rw = return i { memoryOp = Just $ MemoryOp rw }
      rd = try $ symbol "rd" >> result True
      wr = try $ symbol "wr" >> result False


pJump :: Instruction -> Parser Instruction
pJump = \case
  Instruction { address = Just _ } -> fail "there can only be one jump per instruction"
  Instruction { cond = Just _ } -> fail "there can only be one jump per instruction"
  i -> do
    c <- parseCond
    a <- parseAddr
    return i {cond = Just c, address = Just a }


pLabel :: Instruction -> Parser Instruction
pLabel = \case
  Instruction {iLabel = Just _} -> fail "label already set"
  i -> do
    l <- try (symbol ":" *> alphaNum)
    return i {iLabel = Just l}


----------------------------------------------------------------------------------------


pAluShifter :: Instruction -> Parser Instruction
pAluShifter i@Instruction {alu} = do
  s <- pShifter (shifter i)
  pa <- pAlu
  case alu of
    Just a | a /= pa -> fail "alu already set"
    _ -> return i {shifter = Just s, alu = Just pa}

pShifter :: Maybe Shifter -> Parser Shifter
pShifter shifter = do
  s <- try lsh <|> try rsh <|> return ShiftNothing
  case shifter of
    Just action | action /= s -> fail "shifter already set"
    _ -> return s
  where
    lsh = symbol "lsh" >> return ShiftLeft
    rsh = symbol "rsh" >> return ShiftRight


someFunc :: IO ()
someFunc = do
  let fileName = "code.txt"
  putStrLn $ "File: " ++ fileName
  code <- readFile fileName
--   print code
  case parse m16Parser fileName code of
    Right x -> do
      let labels = collectLabels x
      let replacedAddrs = fmap (replaceAddr labels) x
      writeFile "compiled-by-haskell.txt" $ unlines $ fmap (showBin . toBits) replacedAddrs
      putStrLn $ unlines $ fmap printInstr replacedAddrs
    Left x  -> print x
  where
    printInstr i = showBin (toBits i) ++ " " ++ show i

showBin :: Word32 -> String
showBin w = foldl' (flip $ (:) . bitChar . testBit w) "" [0..31]
  where
    bitChar False = '0'
    bitChar True = '1'

collectLabels :: [Instruction] -> Map String Int
collectLabels = foldl' addLabel Map.empty . zip [0..]
  where
    addLabel map (line, Instruction{iLabel}) = case iLabel of
      Just s -> Map.insert s line map
      Nothing -> map

replaceAddr :: Map String Int -> Instruction -> Instruction
replaceAddr map i@Instruction{address} = case address of
  (Just (LabelAddr s)) -> i {address = Just (LineAddr (map Map.! s))}
  _ -> i
