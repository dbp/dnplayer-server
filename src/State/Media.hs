{-# LANGUAGE OverloadedStrings, GADTs, TemplateHaskell,
             QuasiQuotes, FlexibleInstances, TypeFamilies #-}

module State.Media where

import           Data.Text (Text)
import           Database.Groundhog.TH
import           Snap.Snaplet.Groundhog.Postgresql
import           Application
import           Helpers

data Media = Media { year :: Int
                   , month :: Int
                   , day :: Int
                   , media :: Text
                   } deriving Show

mkPersist defaultCodegenConfig { namingStyle = lowerCaseSuffixNamingStyle} [groundhog|
- entity: Media
  dbName: media
  constructors:
    - name: Media
      fields:
        - name: media
          dbName: url
|]


getMedia :: Int -> Int -> Int -> AppHandler (Maybe Text)
getMedia yr mn dy = one $ project MediaField ((YearField ==. yr) &&. MonthField ==. mn &&. DayField ==. dy)

newMedia :: Int -> Int -> Int -> Text -> AppHandler ()
newMedia yr mn dy url = void $ insert $ Media yr mn dy url
