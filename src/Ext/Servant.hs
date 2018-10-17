{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}

-- | This module exports types and functions used to implement applications on Servant web server.
module Ext.Servant (
    ResourceConfigurable(..)
  , ActionContext
  , configureResourceApp
  , createResourceApp
  , module Data.IORef
  , module Control.Monad.Logger
  , module System.Log.FastLogger
  , module Data.Proxy
  , module Servant.API
  , module Servant.Server
  , module Data.Resource
  , module Ext.Servant.Action
  , module Ext.Servant.Context
  , module Ext.Servant.Combinators
  , module Ext.Servant.Template
) where

import Data.IORef
import Control.Monad.Logger
import System.Log.FastLogger
import Data.Proxy
import Servant.API
import Servant.Server
import Network.Wai (Application)
import Data.Resource
import Ext.Servant.Action
import Ext.Servant.Context
import Ext.Servant.Combinators
import Ext.Servant.Template

class ResourceConfigurable settings where
    type RC'Resources settings :: [*]
    type RC'Contexts settings :: [*]
    getResources :: settings -> IO (Resources (Refs (RC'Resources settings)))
    getContexts :: settings -> IO (Context (RC'Contexts settings))

type ActionContext settings (ks :: [*]) = RequestContext ks (Refs (ContextTypes (Refs (RC'Resources settings))))

type Configurable rs contexts keys api allAPI = (
    HasServer api contexts
  , HasServer allAPI contexts
  , GetContextLogger (Refs rs)
  , GenerateKeys keys
  )

configureResourceApp :: forall (keys :: [*]) resAPI allAPI rs cs settings contexts api. (
                          ResourceConfigurable settings
                        , rs ~ RC'Resources settings
                        , cs ~ RC'Contexts settings
                        , contexts ~ (RequestContextEntry keys rs : cs)
                        , api ~ ((rs @> keys) :> resAPI)
                        , Configurable rs contexts keys api allAPI
                        )
                     => settings
                     -> ServerT api Action
                     -> (ServerT api Handler -> ServerT allAPI Handler)
                     -> IO Application
configureResourceApp settings server f = do
    resources <- getResources settings
    contexts <- getContexts settings
    createResourceApp @keys @resAPI @allAPI resources contexts server f

-- | Creates @Application@ using @Resources@.
createResourceApp :: forall (keys :: [*]) resAPI allAPI rs cs contexts api. (
                       contexts ~ (RequestContextEntry keys rs : cs)
                     , api ~ ((rs @> keys) :> resAPI)
                     , Configurable rs contexts keys api allAPI 
                     )
                  => Resources (Refs rs) -- ^ @Resources@ used in the @Application@.
                  -> Context cs -- ^ @Contexts@ of the Servant application.
                  -> ServerT api Action -- ^ Composite of serving function under the resource combinator.
                  -> (ServerT api Handler -> ServerT allAPI Handler) -- ^ Converter from server under resource combinator to server for whole application.
                  -> IO Application -- ^ Runnable @Application@.
createResourceApp resources contexts server f = do
    let resourceServer = hoistServerWithContext (Proxy :: Proxy api)
                                                (Proxy :: Proxy contexts)
                                                (actionHandler resources)
                                                server

    return $ resourceApp (Proxy :: Proxy allAPI)
                         resources
                         (Proxy :: Proxy keys)
                         contexts
                         (f resourceServer)