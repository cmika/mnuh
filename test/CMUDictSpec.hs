{-# LANGUAGE OverloadedStrings #-}

module CMUDictSpec (main, spec) where

import Test.Hspec
import CMUDict
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
      let input  = "Something    or other\n" :: ByteString
          output = ("or other\n" :: ByteString, "Something" :: ByteString)
      in finiteParse CMUDict.word input `shouldBe` Just output

    it "parses symbols" $
      let input  = "HH AA1 HH AA0"
          output = (" AA1 HH AA0" :: ByteString, (HH, Nothing))
      in finiteParse CMUDict.symbol input `shouldBe` Just output

    it "parses pronunciations" $
      let input  = "HH AA1 HH AA0"
          output = ("" :: ByteString, [(HH,Nothing),(AA,Just Primary),(HH,Nothing),(AA,Just None)])
      in finiteParse CMUDict.pronunciation input `shouldBe` Just output

    it "parses entries" $
      let input  = "HAHA  HH AA1 HH AA0\n"
          output = ("" :: ByteString, ("HAHA", [(HH,Nothing),(AA,Just Primary),(HH,Nothing),(AA,Just None)]))
      in finiteParse CMUDict.entry input `shouldBe` Just output

    it "parses both entries and comments" $
      let input  = ";;; first comment\nHAHA  HH AA1 HH AA0\n;;; second comment yea \nHA  HH AA1\n"
          output = ("" :: ByteString,
                    [("HAHA", [(HH,Nothing),(AA,Just Primary),(HH,Nothing),(AA,Just None)])
                    ,("HA", [(HH,Nothing),(AA,Just Primary)])])
      in finiteParse CMUDict.dictFile input `shouldBe` Just output
