{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Ext.Servant.Action where

import GHC.TypeLits
import Data.Proxy
import Data.Functor
import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Control.Exception.Safe hiding (Handler)
import Servant.Server
import Data.Resource

type Action = IO

actionHandler :: (GetContextLogger rs)
              => Resources rs
              -> Action a
              -> Handler a
actionHandler resources action = liftIO $ do
    --let name = actionName action
    contexts <- generateContexts @'[] resources
    $(logQD' "Ext.Servant") contexts $ "Start action"
    r <- action `catchAny` \e -> do
        $(logQE' "Ext.Servant") contexts $ "Exception in action: " ++ show e
        throw e
    $(logQD' "Ext.Servant") contexts $ "Complete action"
    return r