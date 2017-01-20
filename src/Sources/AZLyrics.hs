{-# LANGUAGE OverloadedStrings #-}

module Sources.AZLyrics where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Simple
import Text.HTML.TagSoup ((~/=))
import qualified Text.HTML.TagSoup as TS 
import Debug.Trace

type Artist = ByteString
type Link   = ByteString
type Song = [ByteString]

searchArtist :: String -> IO [(Artist, Link)]
searchArtist artist = do
  request <- parseRequest $ "GET http://search.azlyrics.com/search.php?q=" ++ artist ++ "&p=0&w=artists"
  response <- httpLBS request
  pure $ (takeArtists . TS.parseTags . BSL.toStrict . getResponseBody) response

takeArtists :: [TS.Tag ByteString] -> [(Artist, Link)]
takeArtists tags =
  case drop 2 . dropWhile (~/= ("<td class=\"text-left visitedlyr\">" :: String)) $ tags of
    []    -> []
    tags' -> 
      let TS.TagOpen _ ((_, link):_) = head tags'
          TS.TagText artist = head $ drop 2 tags'
      in (artist, link) : (takeArtists $ drop 3 tags')

getDiscog :: Link -> IO [Song]
getDiscog link = do
  songLinks <- getSongs link
  sequence . fmap getLyrics $ songLinks

getSongs :: Link -> IO [Link]
getSongs link = do
  request <- parseRequest $ "GET " ++ BSC.unpack link
  response <- httpLBS request
  pure $ (takeSongs . TS.parseTags . BSL.toStrict . getResponseBody) response

takeSongs :: [TS.Tag ByteString] -> [Link]
takeSongs tags = 
  case dropWhile (not . isAlbumLink) tags of
    []    -> []
    tags' ->
      let TS.TagOpen _ ((_,link):_) = head tags'
      in trace (BSC.unpack link) $ ("http://www.azlyrics.com/" `BSC.append` BSC.drop 2 link) : (takeSongs $ tail tags')

isAlbumLink :: TS.Tag ByteString -> Bool
isAlbumLink (TS.TagOpen _ (_:(_,"_blank"):_)) = True
isAlbumLink _ = False

getLyrics :: Link -> IO Song
getLyrics link = do
  request <- parseRequest $ "GET " ++ BSC.unpack link
  response <- httpLBS request
  pure $ (takeLyrics . TS.parseTags . BSL.toStrict . getResponseBody) response

startOfLyrics :: String
startOfLyrics = "<!-- Usage of azlyrics.com content by any third-party lyrics provider is prohibited by our licensing agreement. Sorry about that. -->"
 

takeLyrics :: [TS.Tag ByteString] -> Song
takeLyrics t = go (dropWhile (~/= startOfLyrics) t)
  where
    go ((TS.TagClose "div"):_) = []
    go ((TS.TagText lyric):xs) = lyric : go xs
    go (_:xs)                  = go xs
    go _                       = []
