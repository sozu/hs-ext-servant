{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Servant.Utils.StaticFiles (serveDirectoryWebApp)
import Ext.Servant

type AppResources = '[LoggingResource]
type AppContexts = '[]
type AppKeys = '[]

data Settings = Settings

instance ResourceConfigurable Settings where
    type RC'Resources Settings = AppResources
    type RC'Contexts Settings = AppContexts
    getResources settings = do
        lr <- newLoggingResource [(anyTag, LevelDebug, LogStdout defaultBufSize, Nothing)] >>= newIORef
        return $ lr `RCons` RNil
    getContexts settings = return EmptyContext

type API'Resource = "api" :> Get '[PlainText] String
resourceServer sc = api sc

type API'All = (AppResources @> AppKeys :> API'Resource) :<|> "public" :> Raw 
staticServer = serveDirectoryWebApp "public"

main :: IO ()
main = do
    let s = Settings

    app <- configureResourceApp @AppKeys @API'Resource @API'All s resourceServer (:<|> staticServer)

    run 8002 app

api :: ActionContext Settings AppKeys -> Action String
api sc = return "Hello Ext-Servant"