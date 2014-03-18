{-# LANGUAGE OverloadedStrings, ViewPatterns,
             PackageImports, ScopedTypeVariables, FlexibleContexts #-}

module Snap.Snaplet.Groundhog.Postgresql
        ( initGroundhogPostgres
        , GroundhogPostgres
        , HasGroundhogPostgres(..)
        , insert
        , project
        , (G.==.)
        , (G.&&.)
        )
  where

import           Prelude hiding ((++))
import           Control.Applicative
import "MonadCatchIO-transformers" Control.Monad.CatchIO (MonadCatchIO)
import           Snap
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import           Data.Pool
import           Data.Monoid
import           Data.Ratio
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy.Builder.RealFloat as TB
import qualified Database.Groundhog as G
import qualified Database.Groundhog.Core as G
import qualified Database.Groundhog.Postgresql as G

(++) :: Monoid a => a -> a -> a
(++) = mappend
infixr 5 ++

data GroundhogPostgres = GroundhogPostgres
      { pgPool :: Pool G.Postgresql
      }

-- Taken from snaplet-postgresql-simple
getConnectionString :: C.Config -> IO ByteString
getConnectionString config = do
    let params =
            [ ["host"]
            , ["hostaddr"]
            , ["port"]
            , ["dbname","db"]
            , ["user"]
            , ["password","pass"]
            , ["connection_timeout"]
            , ["client_encoding"]
            , ["options"]
            , ["application_name"]
            , ["fallback_application_name"]
            , ["keepalives"]
            , ["keepalives_idle"]
            , ["keepalives_interval"]
            , ["keepalives_count"]
            , ["sslmode"]
            , ["sslcompression"]
            , ["sslcert"]
            , ["sslkey"]
            , ["sslrootcert"]
            , ["sslcrl"]
            , ["requirepeer"]
            , ["krbsrvname"]
            , ["gsslib"]
            , ["service"]
            ]
    connstr <- mconcat <$> mapM showParam params
    extra <- TB.fromText <$> C.lookupDefault "" config "connectionString"
    return $! T.encodeUtf8 (TL.toStrict (TB.toLazyText (connstr ++ extra)))
  where
    qt = TB.singleton '\''
    bs = TB.singleton '\\'
    sp = TB.singleton ' '
    eq = TB.singleton '='

    lookupConfig = foldr (\name names -> do
                            mval <- C.lookup config name
                            case mval of
                              Nothing -> names
                              Just _ -> return mval)
                         (return Nothing)

    showParam [] = undefined
    showParam names@(name:_) = do
      mval :: Maybe C.Value <- lookupConfig names
      let key = TB.fromText name ++ eq
      case mval of
        Nothing -> return mempty
        Just (C.Bool x) -> return (key ++ showBool x ++ sp)
        Just (C.String x) -> return (key ++ showText x ++ sp)
        Just (C.Number x) -> return (key ++ showNum x ++ sp)
        Just (C.List _) -> return mempty

    showBool x = TB.decimal (fromEnum x)

    showNum x = TB.formatRealFloat TB.Fixed Nothing
                   ( fromIntegral (numerator x)
                   / fromIntegral (denominator x) :: Double )

    showText x = qt ++ loop x
      where
        loop (T.break escapeNeeded -> (a,b))
          = TB.fromText a ++
              case T.uncons b of
                Nothing -> qt
                Just (c,b') -> escapeChar c ++ loop b'

    escapeNeeded c = c == '\'' || c == '\\'

    escapeChar c = case c of
                     '\'' -> bs ++ qt
                     '\\' -> bs ++ bs
                     _ -> TB.singleton c

class (MonadCatchIO m) => HasGroundhogPostgres m where
    getGroundhogPostgresState :: m GroundhogPostgres

description :: T.Text
description = "PostgreSQL abstraction"


initGroundhogPostgres :: SnapletInit b GroundhogPostgres
initGroundhogPostgres = makeSnaplet "postgresql-simple" description Nothing $ do
    config <- getSnapletUserConfig
    connstr <- liftIO $ getConnectionString config
    pool <- G.createPostgresqlPool (B8.unpack connstr) 5
    return $ GroundhogPostgres pool


insert :: (HasGroundhogPostgres m, G.PersistEntity v, MonadSnap m) => v -> m (G.AutoKey v)
insert v = do s <- fmap pgPool getGroundhogPostgresState
              liftIO $ G.runDbConn (G.insert v) s

project :: ( HasGroundhogPostgres n
           , MonadSnap n
           , G.PersistEntity v
           , G.EntityConstr v c
           , G.Projection p G.Postgresql (G.RestrictionHolder v c) a
           , G.HasSelectOptions opts G.Postgresql (G.RestrictionHolder v c))
        => p -> opts -> n [a]
project p opts = do s <- fmap pgPool getGroundhogPostgresState
                    liftIO $ G.runDbConn (G.project p opts) s
