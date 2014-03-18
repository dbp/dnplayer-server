{-# LANGUAGE OverloadedStrings, GADTs, TemplateHaskell,
             QuasiQuotes, FlexibleInstances, TypeFamilies #-}

module State.Desc where

import           Data.Text (Text)
import           Database.Groundhog.TH
import           Snap.Snaplet.Groundhog.Postgresql
import           Application
import           Helpers

data Desc = Desc { year :: Int
                 , month :: Int
                 , day :: Int
                 , desc :: Text
                 } deriving Show

mkPersist defaultCodegenConfig { namingStyle = lowerCaseSuffixNamingStyle} [groundhog|
- entity: Desc
  dbName: descriptions
  constructors:
    - name: Desc
      fields:
        - name: desc
          dbName: description
|]

getDesc :: Int -> Int -> Int -> AppHandler (Maybe Text)
getDesc yr mn dy = one $ project DescField (YearField ==. yr &&. MonthField ==. mn &&. DayField ==. dy)

newDesc :: Int -> Int -> Int -> Text -> AppHandler ()
newDesc yr mn dy desc = void $ insert $ Desc yr mn dy desc
