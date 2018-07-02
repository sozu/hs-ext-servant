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
import Control.Monad.Error
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import qualified Control.Exception.Safe as E
import Servant.Server
import Data.Resource

type Action = IO

actionHandler :: (GetContextLogger rs)
              => Resources rs
              -> Action a
              -> Handler a
--actionHandler resources action = liftIO $ do
-- runInBase :: Handler a -> IO (Either ServantErr a)
actionHandler resources action = control $ \runInBase -> do
    --let name = actionName action
    contexts <- generateContexts @'[] resources
    $(logQD' "Ext.Servant") contexts $ "Start action"
    --r <- action `catchAny` \e -> do
    --    $(logQE' "Ext.Servant") contexts $ "Exception in action: " ++ show e
    --    throw e
    r <- (Right <$> action) `E.catches` [ E.Handler $ \e@(ServantErr _ _ _  _) -> do
                                            $(logQE' "Ext.Servant") contexts $ "Error in action: " ++ show e
                                            runInBase $ throwError e
                                        , E.Handler $ \(e :: E.SomeException) -> do
                                            $(logQE' "Ext.Servant") contexts $ "Unexpected exception in action: " ++ show e
                                            E.throw e
                                        ]
    $(logQD' "Ext.Servant") contexts $ "Complete action"
    return $ r
