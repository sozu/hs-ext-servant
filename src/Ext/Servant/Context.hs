{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Ext.Servant.Context where

import GHC.TypeLits
import Data.Proxy
import Data.Maybe (fromJust, maybe)
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Logger
import Control.Exception.Safe hiding (Handler)
import qualified Data.Vault.Lazy as V
import qualified Data.ByteString.Lazy as B
import Data.String
import Network.Wai
import Network.HTTP.Media
import Network.HTTP.Types
import Servant.API
import Servant.API.ContentTypes
import Servant.Server
import Servant.Server.Internal
import Servant.Foreign
import Data.Resource
import Debug.Trace

-- ------------------------------------------------------------
-- Request context and types/functions for it
-- ------------------------------------------------------------

{- | This data type holds values generated and used in each request context.

    - @Request@ has information of HTTP request.
    - @Contexts@ is an accessor to @ResourceContext@s available in the request context.
    - @Keys@ has keys to get or set values from or into vault.

    They are available via functions defined for each purpose.

    - Values stored with keys are obtained by @contextValue@.
    - The values can be modified by @setContextValue@.
    - @withContext@ runs the action using resource contexts.
-}
data RequestContext ks cs = RequestContext Request (Contexts cs) (Keys ks)

-- | Custom error to be rendered according to media type specified by Accept header.
--
-- To use custom error response,
--
-- 1. Declare data type holding error information.
-- 2. Make the type an instance of @Errorneous@. 
-- 3. Throw @ServantErr@ built via @errorFor@ with the custom error as @e@ in your @Handler@s.
class Erroneous e where
    -- | List of media types. The custom error is used only when Accept header specifies one of these types.
    type ErroneousTypes e :: [*]

    -- | Generates @ServantErr@ by customizing base @ServantErr@ object with the information of custome error.
    --
    -- The implementation of this method should change @errBody@ of given @ServantErr@ according to media type
    -- to represent the error information in custom error.
    buildError :: ServantErr -- ^ Base @ServantErr@ object.
               -> e -- ^ Custom error.
               -> MediaType -- ^ Media type to determine the format of response body.
               -> ServantErr -- ^ Customized @ServantErr@ object.

-- | Generates @ServantErr@ containing custom error information.
--
-- This function is designed to be invoked in @Handler@s.
-- Its convenient to set first argument to @ServantErr@ created by functions such as err400, err500.
errorFor :: forall e ks cs. (Erroneous e, AllMime (ErroneousTypes e))
         => ServantErr -- ^ Base @ServantErr@ object.
         -> e -- ^ Custom error.
         -> RequestContext ks cs -- ^ Request context of this invocation.
         -> ServantErr -- ^ Customized @ServantErr@ object to be thrown.
errorFor org e rc = maybe org (buildError org e) mt
    where
        mt = lookup hAccept (requestHeaders $ requestOf rc)
                >>= matchAccept (allMime (Proxy :: Proxy (ErroneousTypes e)))

-- | Returns a request of the @RequestContext@.
requestOf :: forall ks cs. RequestContext ks cs -- ^ @RequestContext@.
          -> Request -- ^ Request.
requestOf (RequestContext r _ _) = r

-- | Returns a value of specified type from @RequestContext@.
contextValue :: forall k ks cs. (GetContextKey k ks)
             => RequestContext ks cs -- ^ @RequestContext@.
             -> IO k -- ^ Obtained value.
contextValue (RequestContext r _ keys) = readIORef $ fromJust $ V.lookup (getContextKey @k keys) (vault r)

-- | Sets a value of @RequestContext@.
-- This function modify @IORef@ to the value of @k@, thus inner state of @RequestContext@ will change.
setContextValue :: forall k ks cs. (GetContextKey k ks)
                => RequestContext ks cs -- ^ @RequestContext@.
                -> k -- ^ A value to set.
                -> IO () -- ^ Returns nothing.
setContextValue (RequestContext r _ keys) v = writeIORef (fromJust $ V.lookup (getContextKey @k keys) (vault r)) v

-- | Alias to convert resource types to context reference types.
type family R2C (rs :: [*]) :: [*]
type instance R2C rs = (Refs (ContextTypes (Refs rs)))

-- | Hetero typed keys used for vault.
data Keys (ks :: [*]) where
    -- | Generate empty key list.
    KNil :: Keys '[] -- ^ Empty key list.

    -- | Prepend a key to existing keys.
    KCons :: V.Key (IORef k) -- ^ Key to prepend.
          -> Keys ks -- ^ Existing keys.
          -> Keys (k ': ks) -- ^ Prepended keys.

-- | Declares a method to generate keys by their types.
class GenerateKeys (ks :: [*]) where
    -- | Generates keys by their types.
    generateKeys :: Proxy ks -- ^ Proxy to specify types.
                 -> IO (Keys ks) -- ^ Generated keys.

instance GenerateKeys '[] where
    generateKeys _ = return KNil

instance (GenerateKeys ks) => GenerateKeys (k ': ks) where
    generateKeys _ = KCons <$> V.newKey @(IORef k) <*> generateKeys (Proxy :: Proxy ks)

-- | Declares a method to get a key by its type.
class GetContextKey k (ks :: [*]) where
    -- | Returns a key of a type specified by a signature or type application syntax.
    getContextKey :: Keys ks -- ^ Keys.
                  -> V.Key (IORef k) -- ^ Key of the type.

instance {-# OVERLAPPING #-} GetContextKey k (k ': ks) where
    getContextKey (k `KCons` _) = k

instance (GetContextKey k ks) => GetContextKey k (x ': ks) where
    getContextKey (_ `KCons` keys) = getContextKey @k keys

-- | Inserts undefined values of each key type into a vault.
prepareVault :: Keys ks -- ^ Keys for values to insert into a vault.
             -> Vault -- ^ A vault.
             -> IO Vault -- ^ Modified vault.
prepareVault KNil v = return v
prepareVault (k `KCons` ks) v = newIORef undefined >>= \ref -> prepareVault ks (V.insert k ref v)

-- | This data type holds keys for underlying values of @RequestContext@.
data RequestContextKeys ks rs = RequestContextKeys (V.Key (IORef (Contexts (R2C rs)))) (Keys ks)

-- | This instance declaration enables to apply @withContext@ to @RequestContext@ directly.
instance (SelectContexts (Refs ds) cs cs) => WithContext ds (RequestContext ks cs) where
    withContext (RequestContext _ cxt _) f = withContext cxt f

-- | Context entry for resources and keys of contextual values.
data RequestContextEntry ks rs = RequestContextEntry (Resources (Refs rs)) (V.Key (RequestContextKeys ks rs))

-- ------------------------------------------------------------
-- Combinator to use request context
-- ------------------------------------------------------------

{- | This combinator enables downstream handlers to access resource contexts of @rs@.

    This combinator provides @ResourceContext ks rs@ as an argument taken by downstream handler.
-}
data (@>) (rs :: [*]) (ks :: [*])

{- | Generates an application which stores keys of resource contexts and other user data into request vault.

    In returned application, spaces to store contextual values are prepared in each @Request@.
    You should always create an application by this function when using @(@>)@ combinator.

    > import qualified Data.Vault.Lazy as V
    > import Servant.API
    > import Servant.Server
    > import qualified Network.Wai.Handler.Warp as Warp
    > import Data.Resource
    > import Database.ORM
    > import Database.ORM.Dialect.PostgreSQL
    > import Ext.Servant.Runner
    >
    > type KS = '[UserData1, UserData2, ...]
    >
    > type API = (@>) '[DBResoure PostgreSql] :> ...
    >
    > server :: Server API
    > server = s1
    >     where
    >         s1 :: Contexts '[IORef DBContext PostgreSql]
    >            -> String
    >         s1 contexts = ...
    > 
    > main = do
    >     r <- newResource $ PostgreSql "postgresql://user:pass@host:5432/dbname" 10
    >     let resources = r `RCons` RNil
    >
    >     Warp.run 8001 $ resourceApp (Proxy :: Proxy API)
    >                                 resources
    >                                 (Proxy :: Proxy KS)
    >                                 EmptyContext 
    >                                 server
-}
resourceApp :: forall api rs ks context. (HasServer api (RequestContextEntry ks rs : context), GenerateKeys ks)
            => Proxy api -- ^ Type of API.
            -> Resources (Refs rs) -- ^ Resources managed in the application.
            -> Proxy ks -- ^ Types of user data.
            -> Context context -- ^ Server context.
            -> Server api -- ^ Server for the API.
            -> Application -- ^ Generated application.
resourceApp p resources pks cxt = \a -> 
    \r h -> do
        -- Prepare vault space for resource contexts.
        ckey <- V.newKey @(IORef (Contexts (R2C rs)))
        cref <- newIORef undefined
        let r1 = r { vault = V.insert ckey cref (vault r) }

        -- Prepare vault spaces for other keyed values.
        keys <- generateKeys pks
        v' <- prepareVault keys (vault r1)
        let r2 = r1 { vault = v' }

        -- Create RequestContextKeys.
        rckey <- V.newKey @(RequestContextKeys ks rs)
        let r3 = r2 { vault = V.insert rckey (RequestContextKeys ckey keys) (vault r2) }

        serveWithContext p (RequestContextEntry resources rckey :. cxt) a r3 h

lhs :: (a :<|> b) -> a
lhs (a :<|> _) = a

rhs :: (a :<|> b) -> b
rhs (_ :<|> b) = b

-- | Declares a method to invoke given handler by providing @RequestContext@.
class WrapHandler h (ks :: [*]) (rs :: [*]) where
    -- | Invokes a handler taking @RequestContext@ as its first argument.
    wrapHandler :: forall context m a cs contentType. (
                  cs ~ ContextTypes (Refs rs)
                , HasContextEntry context (RequestContextEntry ks rs)
                , ContextResources (Refs cs) (Refs rs))
                => Proxy rs -- ^ Proxy to specify resource types.
                -> Proxy ks -- ^ Proxy to specify types of contextual values.
                -> Context context -- ^ Server context.
                -> Request -- ^ HTTP Request.
                -> (RequestContext ks (Refs cs) -> h) -- ^ Downstream handler.
                -> h -- ^ Wrapped handler.

instance {-# OVERLAPPABLE #-} (MonadBaseControl IO m) => WrapHandler (m a) ks rs where
    wrapHandler _ _ context r h = (liftBaseWith $ \runInBase -> do
            let (RequestContextEntry resources key) = getContextEntry context :: RequestContextEntry ks rs
            fst <$> withContext @(ContextTypes (Refs rs)) resources (execute r key h runInBase)
        ) >>= restoreM
        where
            execute :: (With cs, cs ~ ContextTypes (Refs rs))
                    => Request
                    -> V.Key (RequestContextKeys ks rs)
                    -> (RequestContext ks (Refs cs) -> m a)
                    -> RunInBase m IO
                    -> IO (StM m a)
            execute r key h runInBase = do
                let rck@(RequestContextKeys ckey keys) = fromJust $ V.lookup key (vault r)
                let cref = fromJust $ V.lookup ckey (vault r)
                writeIORef cref ?cxt
                runInBase $ h (RequestContext r ?cxt keys)

-- RunInBase m b = forall a. m a -> b (StM m a)
-- liftBaseWith :: (RunInBase m b -> b a) -> m a
-- restoreM :: StM m a -> m a

instance {-# OVERLAPPING #-} (WrapHandler a ks rs, WrapHandler b ks rs) => WrapHandler (a :<|> b) ks rs where
    --wrapHandler :: Proxy rs -> Proxy ks -> Context context -> Request -> (RequestContext ks (Refs cs) -> (a :<|> b)) -> (a :<|> b)
    wrapHandler pr pk context r h = wrapHandler pr pk context r (lhs . h) :<|> wrapHandler pr pk context r (rhs . h)

instance {-# OVERLAPPING #-} (WrapHandler b ks rs) => WrapHandler ((->) a b) ks rs where
    --wrapHandler :: Proxy rs -> Proxy ks -> Context context -> Request -> (RequestContext ks (Refs cs) -> (a -> b)) -> (a -> b)
    wrapHandler pr pk context r h = \a -> wrapHandler pr pk context r (\c -> h c a)

instance ( WrapHandler (Server api) ks rs
         , cs ~ ContextTypes (Refs rs)
         , HasServer api context
         , HasContextEntry context (RequestContextEntry ks rs)
         , ContextResources (Refs cs) (Refs rs)) => HasServer ((@>) (rs :: [*]) (ks :: [*]) :> api) context where
    type ServerT ((@>) rs ks :> api) m = RequestContext ks (R2C rs) -> ServerT api m

    route p context (Delayed {..}) = route (Proxy :: Proxy api) context delayed'
        where
            serverD' c p h a b r = wrapHandler (Proxy :: Proxy rs) (Proxy :: Proxy ks) context r <$> serverD c p h a b r
            delayed' = Delayed { serverD = serverD'
                               --, bodyD = \ct -> (,) <$> bodyD ct <*> return ct
                               , ..
                               }

    hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

instance (HasForeign lang ftype api) => HasForeign lang ftype ((@>) (rs :: [*]) (ks :: [*]) :> api) where
    type Foreign ftype ((@>) (rs :: [*]) (ks :: [*]) :> api) = Foreign ftype api
    foreignFor lang ftype p req = foreignFor lang ftype (Proxy :: Proxy api) req

-- ------------------------------------------------------------
-- Handler filter combinator
-- ------------------------------------------------------------

{- | A cominator which shows that a HandleFilter wraps downstream handlers.

    @f@ denotes the type of the instance of HandleFilter which should be registered in server context.

    > type API = Filter SomeFilter :> ...
    >
    > data SomeFilter = SomeFilter String
    > instance HandlerFilter SomeFilter where
    >     ...
    >
    > main = do
    >     let context = SomeFilter "foo" :. EmptyContext
    >     ...

    As shown in above exmple, a filter object must be generated beforehand and stored in server context.
    When request comes, it is selected from the server context only by its type.
    Therefore, if you intend to use another @SomeFilter@ having another @String@ parameter,
    you have to define another type (newtype of @SomeFilter@ ?) to distinguish them.
-}
data Filter f

{- | Represents a handler filter.

    Handler filter is used with @Filter@ combinator and handles a downstream handler. Each filter can

    - Execute some IO actions around the invocation of the handler.
    - Provide arguments to the handler. Types of the arguments must be defined by @FilterArgs@.
    - Use resource contexts given by implicit parameter. Types of resource contexts must be specified by @ContextsForFilter@. 

    Each filter has to define 3 kinds of type families.
    - @ContextsForFilter@ has types of resource contexts which will be passed to @doFilter@ as an implicit parameter.
        - They may be a subset of resource context types used in the application.
    - @KeysForFilter@ has types of contextual values stored in @RequestContext@.
        - TODO: Currently, they MUST be the same type list as the type list given to @(@>)@.
    - @FilterArgs@ defines the types of arguments which this filter provides to the downstream handler.
        - For esample, the filter whose @FilterArgs@ is @'[String, Int]@ accepts downstream handler whose type is @String -> Int -> Handler a@.
-}
class HandlerFilter f where
    -- | Types of resource contexts.
    type ContextsForFilter f :: [*]

    -- | Types of contextual values.
    type KeysForFilter f :: [*]

    -- | Types of arguments this filter provides.
    type FilterArgs f :: [*]

    -- | Executes something around downstream handler and returns the result.
    doFilter :: (With (ContextsForFilter f), MonadIO m, MonadBaseControl IO m)
             => f -- ^ Filter object.
             -> RequestContext (KeysForFilter f) (Refs (ContextsForFilter f)) -- ^ @RequestContext@.
             -> Apply (FilterArgs f) (m a) -- ^ Downstream handler.
             -> m a -- ^ Result of this filter, in most cases, the result of downstream handler.

-- | Declares a method to apply filter function to downstream handler.
--
-- Because @doFilter@ is a IO monad, it can't be applied to any type of downstream handler.
-- This class provides a way to resolve the problem in type safe method.
class (HandlerFilter f) => FilterHandler f h where
    -- | Applies filter function to downstrem handler.
    filterHandler :: (SelectContexts (Refs (ContextsForFilter f)) (R2C rs) (R2C rs))
                  => f -- ^ Filter object.
                  -> Request -- ^ HTTP request.
                  -> V.Key (RequestContextKeys (KeysForFilter f) rs) -- ^ Vault key to get keys of contextual values.
                  -> Apply (FilterArgs f) h -- ^ Downstream handler.
                  -> h -- ^ Wrapped handler.

instance {-# OVERLAPPABLE #-} (HandlerFilter f) => FilterHandler f (Handler a) where
    filterHandler filter r key h = do
        case V.lookup key (vault r) of
            Just rck@(RequestContextKeys ckey keys) -> do
                let cref = fromJust $ V.lookup ckey (vault r)
                cxt <- liftIO $ readIORef cref
                let ?cxt = selectContexts @(Refs (ContextsForFilter f)) cxt cxt
                doFilter filter (RequestContext r ?cxt keys) h
            Nothing -> fail "No resource context found in request"

instance {-# OVERLAPPING #-} (
            HandlerFilter f
          , Distribute (Apply (FilterArgs f) (a :<|> b)) (Apply (FilterArgs f) a) (Apply (FilterArgs f) b)
          , FilterHandler f a
          , FilterHandler f b) => FilterHandler f (a :<|> b) where
    -- filterHandler :: f -> Request -> V.Key (RequestContextKeys (KeysForFilter f) rs) -> Apply (FilterArgs f) (a :<|> b) -> (a :<|> b)
    filterHandler filter r key h = 
        let (ha :<|> hb) = distribute h
        in filterHandler filter r key ha :<|> filterHandler filter r key hb

instance {-# OVERLAPPING #-} (HandlerFilter f, FilterHandler f b, FilterOnFunc (FilterArgs f)) => FilterHandler f ((->) a b) where
    -- filterHandler :: f -> Request -> V.Key (RequestContextKeys (KeysForFilter f) rs) -> Apply (FilterArgs f) (a -> b) -> (a -> b)
    filterHandler filter r key h = \a -> filterHandler filter r key (h' a)
        where
            h' = filterOnFunc (Proxy :: Proxy (FilterArgs f)) (Proxy :: Proxy b) h

-- | Declares a method to move original argument of downstream handler to the top of the arguments.
--
-- The signature can be regarded as @Proxy xs -> Proxy b -> Apply xs (a -> b) -> (a -> Apply xs b)@.
-- What this function does is moving @a@ from the end of arguments to the top.
class FilterOnFunc xs where
    filterOnFunc :: Proxy xs -> Proxy b -> Apply xs (a -> b) -> a -> Apply xs b

instance FilterOnFunc '[] where
    filterOnFunc _ _ f a = f a

instance (FilterOnFunc xs) => FilterOnFunc (x ': xs) where
    filterOnFunc _ p f a = \x -> filterOnFunc (Proxy :: Proxy xs) p (f x) a

-- | Declares a method to apply a function to each side of @(:<|>)@.
class Distribute h a b where
    distribute :: h -> (a :<|> b)

instance {-# OVERLAPPING #-} Distribute (x -> (a :<|> b)) (x -> a) (x -> b) where
    distribute h = (\x -> lhs (h x)) :<|> (\x -> rhs (h x))

instance (Distribute y a b) => Distribute (x -> y) (x -> a) (x -> b) where
    distribute h = (\x -> lhs $ (distribute (h x) :: (a :<|> b))) :<|> (\x -> rhs $ (distribute (h x) :: (a :<|> b)))

instance ( FilterHandler f (ServerT api Handler)
         , HasServer api context
         , HandlerFilter f
         , HasContextEntry context f
         , HasContextEntry context (RequestContextEntry (KeysForFilter f) (ResourcesInContext context))
         , cs ~ (ContextTypes (Refs (ResourcesInContext context)))
         , SelectContexts (Refs (ContextsForFilter f)) (Refs cs) (Refs cs)
         , Merge (FilterArgs f)
         ) => HasServer (Filter f :> api) context where
    type ServerT (Filter f :> api) m = Apply (FilterArgs f) (ServerT api m)

    route p context (Delayed {..}) = route (Proxy :: Proxy api) context (Delayed { serverD = serverD', .. })
        where
            serverD' c p h a b r = do
                let filter = getContextEntry context :: f
                let (RequestContextEntry resources key) = getContextEntry context :: RequestContextEntry (KeysForFilter f) (ResourcesInContext context)
                serverD c p h a b r >>= \h -> Route (filterHandler filter r key h)

    -- hoistServerWithContext :: Proxy (Filter f :> api) -> Proxy context -> (m x -> n x) -> ServerT (Filter f :> api) m -> ServerT (Filter f :> api) n
    -- s :: ServerT (Filter f :> api) m
    --   == Apply (FilterArgs f) (ServerT api m)
    --   == a1 -> ... -> an -> ServerT api m
    hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt `merge'` s
        where
            merge' :: (b -> c) -> Apply (FilterArgs f) b -> Apply (FilterArgs f) c
            merge' = merge (Proxy :: Proxy (FilterArgs f))

instance (HasForeign lang ftype api) => HasForeign lang ftype (Filter f :> api) where
    type Foreign ftype (Filter f :> api) = Foreign ftype api
    foreignFor lang ftype p req = foreignFor lang ftype (Proxy :: Proxy api) req

-- ------------------------------------------------------------
-- Other combinators
-- ------------------------------------------------------------

-- | 
data Use (a :: *)

instance ( HasServer api context 
         , HasContextEntry context a
         ) => HasServer (Use a :> api) context where
    type ServerT (Use a :> api) m = a -> ServerT api m

    route p context (Delayed {..}) = route (Proxy :: Proxy api) context (Delayed { serverD = serverD', .. })
        where
            serverD' c p h a b r = ($ (getContextEntry context :: a)) <$> serverD c p h a b r

    hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

instance (HasForeign lang ftype api) => HasForeign lang ftype (Use a :> api) where
    type Foreign ftype (Use a :> api) = Foreign ftype api
    foreignFor lang ftype p req = foreignFor lang ftype (Proxy :: Proxy api) req

-- ------------------------------------------------------------
-- Utility type level functions.
-- ------------------------------------------------------------

type family HasElems (as :: [*]) (bs :: [*]) (cs :: [*]) :: [*] where
    HasElems (a ': as) (a ': bs) cs = a ': HasElems as cs cs
    HasElems (a ': as) (b ': bs) cs = HasElems (a ': as) bs cs

type family ResourcesInContext (context :: [*]) :: [*] where
    ResourcesInContext (RequestContextEntry ks rs ': cs) = rs
    ResourcesInContext (c ': cs) = ResourcesInContext cs

type family Apply (as :: [*]) (f :: *) :: *
type instance Apply '[] f = f
type instance Apply (a ': as) f = a -> Apply as f

class Merge (as :: [*])  where
    merge :: forall b c. Proxy as -> (b -> c) -> Apply as b -> Apply as c

instance Merge '[] where
    merge _ f g = f g

instance (Merge as) => Merge (a ': as) where
    merge _ f g = \(x :: a) -> (merge (Proxy :: Proxy as) f (g x))