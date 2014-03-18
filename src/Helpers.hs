{-# LANGUAGE OverloadedStrings, PackageImports #-}

module Helpers (void
               , io
               , one
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
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B8

one :: Functor f => f [a] -> f (Maybe a)
one = fmap listToMaybe

io :: MonadIO m => IO a -> m a
io = liftIO

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
