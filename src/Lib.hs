{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Lib where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import Text.HTML.TagSoup
import Text.Parsec
import Text.StringLike

instance TagRep Text where
  toTagRep x =
    case parseTags (T.unpack x) of
          [a] -> toTagRep a
          _ -> error $ "When using a TagRep it must be exactly one tag, you gave: " ++ (T.unpack x)

getShowDescriptionsUrls :: [Tag Text] -> [((Int,Int,Int), (Text, Text))]
getShowDescriptionsUrls tags =
  map (\(ts, media) ->  fix (getRes (sections (~== ("<a>" :: Text)) ts), media)) showTags
  where fix (((a,b,c),d), e) = ((a,b,c), (d,e))
        getRes ts = let url = fromAttrib "href" (head (head ts)) :: Text
                        desc = T.intercalate "; " (map (innerText .
                                                        takeWhile (~/= ("</a>" :: Text)) .
                                                        tail) (tail ts))
                    in case parse dateParser "" (T.unpack url) of
                         Left _err -> error "Could not parse date from url"
                         Right res -> (res, desc)
        dateParser = do string "http://www.democracynow.org/"
                        year <- count 4 digit
                        char '/'
                        month <- many1 digit
                        char '/'
                        day <- many1 digit
                        return (read year, read month, read day)
        showTags :: [([Tag Text], Text)]
        showTags = map getVAndS $ sections (~== ("<ul class='recent_shows'>" :: Text)) tags
        getVAndS tags = (takeWhile (~/= ("</ul>" :: Text)) tags,
                         getMedia tags)
        getMedia tags = let vid = tail (dropWhile (~/= ("<span class='videoLink'>" :: Text)) tags)
                        in if head vid ~== ("</span>" :: Text)
                              then let aud = head $ tail $ dropWhile (~/= ("<span class='audioLink'>" :: Text)) vid
                                   in if aud ~== ("</span>" :: Text)
                                         then ""
                                         else fromAttrib "href" aud
                              else fromAttrib "href" $ head vid
