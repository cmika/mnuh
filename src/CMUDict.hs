{-# LANGUAGE OverloadedStrings #-}

module CMUDict where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.ByteString (Parser)
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.ByteString.Char8 as APC
import Data.Trie (Trie)
import qualified Data.Trie as Trie
import Data.Either (rights)

data Articulation = 
    Vowel
  | RColored
  | Stop 
  | Affricate
  | Fricative
  | Nasal
  | Liquid
  | Semivowel
  deriving (Eq, Show)

data Phone = 
   -- Vowels
      AO | AA | IY | UW | EH
    | IH | UH | AH | AX | AE
    | EY | AY | OW | AW | OY
    -- RColored
    | ER | AXR
    -- Stops
    | P  | B  | T  | D  | K  | G
    -- Affricates
    | CH | JH
    -- Fricatives
    | F  | V  | TH | DH | S  | Z  | SH | ZH | HH
    -- Nasals
    | M | EM | N | EN | NG | ENG
    -- Liquids
    | L | EL | R | DX | NX
    -- Semivowels
    | Y | W | Q
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Stress = None | Primary | Secondary
  deriving(Eq, Show)

type Phone_Str = (Phone, Maybe Stress)

classifyPhone :: Phone -> Articulation
classifyPhone x
  | x <= OY   = Vowel
  | x <= AXR  = RColored
  | x <= G    = Stop
  | x <= JH   = Affricate
  | x <= HH   = Fricative
  | x <= ENG  = Nasal
  | x <= NX   = Liquid
  | otherwise = Semivowel

-- load entire cmudict file into memory
loadDict :: FilePath -> IO (Trie [Phone_Str])
loadDict path = do
  file <- BS.readFile path
  pure $ buildTrie file

buildTrie :: ByteString -> Trie [Phone_Str]
buildTrie input = Trie.fromList (either (\_ -> []) id $ AP.parseOnly dictFile input)


-- consume then discard comments
comment :: Parser ()
comment = AP.string ";;;" *> AP.takeTill APC.isEndOfLine *> APC.endOfLine

-- parse the word
word :: Parser ByteString
word = AP.takeTill APC.isSpace_w8 <* APC.skipSpace

intToStress :: Int -> Maybe Stress
intToStress (-1) = Nothing
intToStress 1  = Just Primary
intToStress 2  = Just Secondary
intToStress _  = Just None


bsToPhone :: ByteString -> Phone
bsToPhone = read . BS.unpack

-- parse a single pronunciation symbol
symbol :: Parser Phone_Str
symbol = do
  p <- bsToPhone <$> AP.takeTill (\x -> APC.isDigit_w8 x || APC.isSpace_w8 x)
  s <- intToStress <$> AP.option (-1) APC.decimal
  pure(p, s)


-- parse the entire pronunciation for a word
pronunciation :: Parser [Phone_Str]
pronunciation = AP.sepBy symbol $ AP.word8 32

-- parse both the word and its pronunciation
entry :: Parser (ByteString, [Phone_Str])
entry = do
  w <- word
  p <- pronunciation
  APC.endOfLine
  pure (w, p)

--parse either an entry or a comment untill all input is consumed
dictFile :: Parser [(ByteString, [Phone_Str])]
dictFile = rights <$> AP.manyTill (AP.eitherP comment entry) AP.endOfInput
