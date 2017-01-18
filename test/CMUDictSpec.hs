{-# LANGUAGE OverloadedStrings #-}

module CMUDictSpec (main, spec) where

import Test.Hspec
import qualified CMUDict
import qualified Data.Attoparsec.ByteString as Atto
import Data.ByteString (ByteString)
import qualified Data.Trie as Trie

main :: IO ()
main = putStr "main not implemented"

finiteParse :: Atto.Parser a -> ByteString -> Maybe (ByteString, a)
finiteParse parser input = case flip Atto.feed "" $ Atto.parse parser input of
  Atto.Done rest result -> Just (rest, result)
  _                     -> Nothing

spec :: Spec
spec = do
  describe "CMUDict file parsing" $ do
    it "parses comments" $
      let input = ";;; this is the comment\n this is not the comment"
      in finiteParse CMUDict.comment input `shouldBe` Just (" this is not the comment" :: ByteString, ())

    it "parses words" $
      finiteParse CMUDict.word "Something    or other\n" `shouldBe` Just ("or other\n" :: ByteString, "Something" :: ByteString)

    it "parses symbols" $
      finiteParse CMUDict.symbol "HH AA HH AA" `shouldBe` Just (" AA HH AA" :: ByteString, "HH" :: String)

    it "parses pronunciations" $
      finiteParse CMUDict.pronunciation "HH AA HH AA" `shouldBe` Just ("" :: ByteString, ["HH","AA","HH","AA"]:: [String])

    it "parses entries" $
      finiteParse CMUDict.entry "HAHA  HH AA HH AA\n" `shouldBe` Just ("" :: ByteString, ("HAHA", ["HH", "AA", "HH", "AA"]))

    it "parses both entries and comments" $
      let input  = ";;; first comment\nHAHA  HH AA HH AA\n;;; second comment yea \nHA  HH AA\n"
          output = Just ("" :: ByteString, [("HAHA", ["HH", "AA", "HH", "AA"]), ("HA", ["HH", "AA"])])
      in finiteParse CMUDict.dictFile input `shouldBe` output
