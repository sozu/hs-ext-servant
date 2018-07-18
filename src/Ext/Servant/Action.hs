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
import Control.Monad.Except
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import qualified Control.Exception.Safe as E
import Servant.API.ContentTypes (AllMime)
import Servant.Server
import Data.Resource
import Ext.Servant.Context
import Ext.Servant.Validation

type Action = IO

-- | Converts @Action@ to @Handler@ to be applied to @hoistServerWithContext@ like below.
--
-- > let rs = hoistServerWithContext (Proxy :: Proxy ResourceAPI)
-- >                                 contextTypes
-- >                                 (actionHandler resources)
-- >                                 resourceServer
--
-- This function also provides logging function to @Handler@ monad by means of give resources.
actionHandler :: (GetContextLogger rs)
              => Resources rs
              -> Action a
              -> Handler a
actionHandler resources action = control $ \runInBase -> do
    contexts <- generateContexts @'[] resources
    $(logQD' "Ext.Servant") contexts $ "Start action"
    r <- (Right <$> action) `E.catches` [ E.Handler $ \e@(ServantErr _ _ _  _) -> do
                                            $(logQE' "Ext.Servant") contexts $ "Error in action: " ++ show e
                                            runInBase $ throwError e
                                        , E.Handler $ \(e :: E.SomeException) -> do
                                            $(logQE' "Ext.Servant") contexts $ "Unexpected exception in action: " ++ show e
                                            E.throw e
                                        ]
    $(logQD' "Ext.Servant") contexts $ "Complete action"
    return $ r

-- | Validates @v@ and returns value of @ValidationTarget v@ if it succeeds, otherwise throws custom error.
--
-- You have to give the instance implementation of @[ValidationError]@ for @Errorneous@ in some module.
-- If no implementation are found, the compilation of source code using this function fails.
validateOr400 :: forall m v ks cs. (
                   E.MonadThrow m
                 , Validatable v
                 , Erroneous [ValidationError]
                 , AllMime (ErroneousTypes [ValidationError])
                 )
              => RequestContext ks cs -- ^ Request context.
              -> v -- ^ Validatable object.
              -> m (ValidationTarget v) -- ^ Unrapped value if validation succeeds.
validateOr400 rc v = do
    case validate v of
        Nothing -> E.throw $ errorFor err400 (errorsOf v) rc
        Just v' -> return v'