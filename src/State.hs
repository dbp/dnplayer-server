{-# LANGUAGE OverloadedStrings #-}

module State where

import Data.Text (Text)
import Snap.Snaplet.PostgresqlSimple
import Application
import Helpers

getDesc :: Int -> Int -> Int -> AppHandler (Maybe Text)
getDesc yr mn dy =
  fieldQuery "select description from descriptions where year = ? and month = ? and day = ?" (yr, mn, dy)

newDesc :: Int -> Int -> Int -> Text -> AppHandler ()
newDesc yr mn dy desc =
  void $ execute "insert into descriptions (year, month, day, description) values (?,?,?,?)" (yr,mn,dy,desc)
