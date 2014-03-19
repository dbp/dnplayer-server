{-# LANGUAGE TemplateHaskell, PackageImports, FlexibleInstances #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Groundhog.Postgresql (HasGroundhogPostgres(..), GroundhogPostgres)
import "mtl" Control.Monad.State (get)
import Network.HTTP.Client
------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _db :: Snaplet GroundhogPostgres
    , _man :: Manager
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasGroundhogPostgres (Handler b App) where
  getGroundhogPostgresState = with db get

------------------------------------------------------------------------------
type AppHandler = Handler App App
