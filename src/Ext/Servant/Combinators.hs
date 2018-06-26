{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Ext.Servant.Combinators where

import GHC.TypeLits
import Data.Proxy
import Control.Monad
import qualified Data.List as L
import Data.String
import qualified Data.ByteString as B
import Network.Wai
import Servant.API
import Servant.Server
import Servant.Server.Internal.RoutingApplication

-- | A combinator to allow cross domain access for specified HTTP methods.
data CrossDomain (methods :: [StdMethod])

newtype CrossDomainOrigin = CrossDomainOrigin { getOrigin :: String }

instance ( HasServer api context
         , HasContextEntry context CrossDomainOrigin
         , ShowMethods methods
         ) => HasServer (CrossDomain methods :> api) context where
    type ServerT (CrossDomain methods :> api) m = ServerT api m

    route p context (Delayed {..}) = tweakResponse (fmap addCrossDomain) $ route (Proxy :: Proxy api) context (Delayed { .. })
        where
            origin' :: B.ByteString
            origin' = fromString $ getOrigin (getContextEntry context :: CrossDomainOrigin)

            methods' :: B.ByteString
            methods' = fromString $ L.intercalate "," $ showMethods (Proxy :: Proxy methods)

            addCrossDomain :: Response -> Response
            addCrossDomain = mapResponseHeaders ((("Access-Control-Allow-Origin", origin') :) . (("Access-Control-Allow-Methods", methods') :))

    hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt s

class ShowMethods (ms :: [StdMethod]) where
    showMethods :: Proxy ms -> [String]

instance ShowMethods '[] where
    showMethods _ = []
instance (ShowMethods ms) => ShowMethods ('GET ': ms) where
    showMethods _ = "GET" : showMethods (Proxy :: Proxy ms)
instance (ShowMethods ms) => ShowMethods ('POST ': ms) where
    showMethods _ = "POST" : showMethods (Proxy :: Proxy ms)
instance (ShowMethods ms) => ShowMethods ('HEAD ': ms) where
    showMethods _ = "HEAD" : showMethods (Proxy :: Proxy ms)
instance (ShowMethods ms) => ShowMethods ('PUT ': ms) where
    showMethods _ = "PUT" : showMethods (Proxy :: Proxy ms)
instance (ShowMethods ms) => ShowMethods ('DELETE ': ms) where
    showMethods _ = "DELETE" : showMethods (Proxy :: Proxy ms)
instance (ShowMethods ms) => ShowMethods ('OPTIONS ': ms) where
    showMethods _ = "OPTIONS" : showMethods (Proxy :: Proxy ms)