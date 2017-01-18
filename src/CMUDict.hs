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

-- load entire cmudict file into memory
loadDict :: FilePath -> IO (Trie [String])
loadDict path = do
  file <- BS.readFile path
  pure $ buildTrie file

buildTrie :: ByteString -> Trie [String]
buildTrie input = Trie.fromList (either (\_ -> []) id $ AP.parseOnly dictFile input)

-- consume then discard comments
comment :: Parser ()
comment = AP.string ";;;" *> AP.takeTill APC.isEndOfLine *> APC.endOfLine

-- parse the word
word :: Parser ByteString
word = AP.takeTill APC.isSpace_w8 <* APC.skipSpace

-- parse a single pronunciation symbol
symbol :: Parser String
symbol = BS.unpack <$> AP.takeTill APC.isSpace_w8

-- parse the entire pronunciation for a word
pronunciation :: Parser [String]
pronunciation = AP.sepBy symbol $ AP.word8 32

-- parse both the word and its pronunciation
entry :: Parser (ByteString, [String])
entry = do
  w <- word
  p <- pronunciation
  APC.endOfLine
  pure (w, p)

--parse either an entry or a comment untill all input is consumed
dictFile :: Parser [(ByteString, [String])]
dictFile = rights <$> AP.manyTill (AP.eitherP comment entry) AP.endOfInput
