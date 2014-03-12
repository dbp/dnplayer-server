{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Helpers (void
               , io
               , singleQuery
               , singleQuery'
               , numberQuery
               , numberQuery'
               , fieldQuery
               , fieldQuery'
               , idQuery
               , tshow
               , tNotNull
               , readSafe
               , getId
               , require
               , getCurrentPath
               ) where

import Snap.Core
import "mtl" Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad (void, join)
import Data.Maybe
import Snap.Snaplet.PostgresqlSimple
import Database.PostgreSQL.Simple.FromField
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B8

io :: MonadIO m => IO a -> m a
io = liftIO

singleQuery :: (HasPostgres m, Functor m, ToRow q, FromRow r) => Query -> q -> m (Maybe r)
singleQuery stmt attrs = fmap listToMaybe $ query stmt attrs

singleQuery' :: (HasPostgres m, Functor m, FromRow r) => Query -> m (Maybe r)
singleQuery' stmt = fmap listToMaybe $ query_ stmt

idQuery :: (HasPostgres m, Functor m, ToRow q) => Query -> q -> m (Maybe Int)
idQuery = fieldQuery

numberQuery :: (HasPostgres m, Functor m, ToRow q) => Query -> q -> m Int
numberQuery q attrs = fmap (head.fromJust) $ singleQuery q attrs

numberQuery' :: (HasPostgres m, Functor m) => Query -> m Int
numberQuery' q = fmap (head.fromJust) $ singleQuery' q

fieldQuery :: (HasPostgres m, Functor m, ToRow q, FromField r) => Query -> q -> m (Maybe r)
fieldQuery q attrs = fmap (join . fmap listToMaybe . listToMaybe) $ query q attrs

fieldQuery' :: (HasPostgres m, Functor m, FromField r) => Query -> m (Maybe r)
fieldQuery' q = fmap (join . fmap listToMaybe . listToMaybe) $ query_ q

tshow :: Show a => a -> Text
tshow = T.pack . show

tNotNull :: Text -> Bool
tNotNull = not.T.null

readSafe :: Read a => String -> Maybe a
readSafe = fmap fst . listToMaybe . reads

getId :: MonadSnap m => m Int
getId = do mi <- getParam "id"
           case fmap B8.unpack mi >>= readSafe of
             Nothing -> pass
             Just i -> return i

require :: MonadSnap m => m (Maybe a) -> m a
require ma = do a' <- ma
                case a' of
                  Nothing -> pass
                  Just a -> return a

getCurrentPath :: MonadSnap m => m Text
getCurrentPath = fmap ( T.decodeUtf8 . urlEncode . T.encodeUtf8
                      . fst . T.breakOn "?" . T.decodeUtf8 . rqURI) getRequest
