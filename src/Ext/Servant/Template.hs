{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE OverloadedStrings #-}

module Ext.Servant.Template where

import GHC.Base
import System.IO.Unsafe
import Data.IORef
import Data.Proxy
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.ByteString.Char8 as C8
import Network.HTTP.Media ((//))
import Servant.API
import Data.Template
import Data.Template.Expr
import qualified Data.Template.Exp as EXP

{- | Quasi-quote function.

    This quotation can only appear in place of top-level declaration.
    The string inside it should 3 string of (i)type name (ii)template file path (iii)type expression of handler result which are separated by spaces.
    (i) is a new name of the response type used in API type declarations.
    The response type is declared as a data type including no data constructors by splicing this quotation.
    Therefore, after the quotation, you can use the name as the existing type name. (ii) is a template file path.
    (iii) is a Haskell expression of the type which will be passed from the handler to the template.
    In the template, the passed value can be access as the variable whose name is 'it'.

    Typically, the use of this quotation and the declaration of an API are as follows.

    > data Account = ...
    > 
    > [tmpl| Dashboard template/dashboard.html Account |]
    > 
    > type DashboardAPI = "dashboard" :> Get '[Dashboard] Account
    > 
    > dashboard :: Server DashboardAPI
    > dashboard = return Account {...}
    > 
    > app :: Application
    > app = serve (Proxy :: Proxy DashboardAPI) dashboard
-}
tmpl = QuasiQuoter {
    quoteExp = undefined
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = \s -> let n:f:t = words s in declareTemplate (mkName n) f (unwords t) True
  }

{- | Runtime compilation version of @tmpl@.

    The same syntax as @tmpl@ is available.
    Difference of this and @tmpl@ is that this quote compiles the template in each evaluation.
    Therefore, modification of the template after the application is compiled will be reflected to rendering result without re-compilation.
    This feature is convenient especially in development phase.
    However, because runtime compilcation gives horribly bad impact on the latency, this should not be used in production.
-}
tmpld = QuasiQuoter {
    quoteExp = undefined
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = \s -> let n:f:t = words s
                     in declareTemplate (mkName n) f (unwords t) False
  }

unIO' c = let (# _, v #) = unIO c realWorld# in v

{- | Generate declarations to use a file as an HTML template in Servant.

    This function generates type implementing @MimeRender@ for each HTML template.

    @tmpl@ generates code which compiles template at compile time.
    > instance MimeRender [TemplateName] [arg_type] where
    >     mimeRender _ it = fromString $(render path)

    @tmpld@ implements @MimeRender@ to compile template for each time @mimeRender@ is invoked.
    > cache_[TemplateName] = unsafePerformIO $ newIORef Nothing
    > instance MimeRender [TemplateName] [arg_type] where
    >     mimeRender _ it = fromString
    >                           $ unsafePerformIO
    >                           $ renderRuntime path [import_list] (it, "it", [arg_type]) cache_[TemplateName]
-}
declareTemplate :: Name -- ^ A name of a type used for the response for the template.
                -> FilePath -- ^ File path of the template.
                -> String -- ^ An expression of the type which will be passed from the handler to the template.
                -> Bool -- ^ Specifies the template will be rendered in compile time or runtime.
                -> Q [Dec] -- ^ Generated declarations.
declareTemplate n path t statically = do
    --dt <- dataD (cxt []) n [] Nothing [] (cxt []) -- template-haskell-2.11.0.0
    dt <- dataD (cxt []) n [] Nothing [] [] -- template-haskell-2.12.0.0
    let imports = thisModule >>= importedModules
    exts <- extsEnabled
    (mr, decs) <- if statically
                then return $ (funD 'mimeRender [clause [wildP, varP $ mkName "it"] (normalB (appE (varE 'UTF8.fromString) (render path))) []], [])
                else do
                    let cacheName = mkName $ "cache_" ++ nameBase n 
                    cache <- funD cacheName [clause [] (normalB (appE (varE 'unsafePerformIO) (appE (varE 'newIORef) (conE 'Nothing)))) []]
                    -- Without NOINLINE pragma,
                    -- GHC seems to consider that all cache_XXX functions are identical and only an IORef object is created and shared.
                    -- This causes a bug the expression generated from newest template is always used.
                    noinline <- pragInlD cacheName NoInline FunLike AllPhases
                    let f = appE (appE (appE [| renderRuntime newContext path |] imports) [| (it, "it", fromJust $ EXP.getType t) |]) (varE cacheName)
                    return $ ( funD 'mimeRender [clause [wildP, varP $ mkName "it"]
                                                      (normalB (appE (varE 'UTF8.fromString) (appE (varE 'unsafePerformIO) f)))
                                                      []]
                             , [noinline, cache]
                             )
    --mime <- instanceD (cxt []) (appT (appT (conT ''MimeRender) (conT n)) (getType t)) [mr]
    -- Instance declaration uses type variable like
    -- > instance (a ~ ArgType) => MimeRender TemplateName a where
    -- >     ...
    -- because direct instantiation of ArgType can cause "Illegal type synonym family application in instance" error.
    -- Use of the type variable brings the restriction that no other instances can be declared for the template.
    -- Is this restriction allowable? Maybe so because one template should be written to render variable on just one type.
    let alias = mkName "a"
    mime <- instanceD (cxt [appT (appT equalityT (varT alias)) (getType t)]) (appT (appT (conT ''MimeRender) (conT n)) (varT alias)) [mr]
    let bs v = Just (appE (varE 'C8.pack) (litE $ stringL v))
    let ct = funD 'contentType [clause [wildP] (normalB (infixE (bs "text") (varE '(//)) (bs "html"))) []]
    accept <- instanceD (cxt []) (appT (conT ''Accept) (conT n)) [ct]
    return $ decs ++ [dt, mime, accept]

-- | Content type for text/html.
--
-- This content type can be used with @Handler@ function returning @Renderer@.
-- You can select the template in @Handler@ function
-- by declaring this type as the content type of the API and implementing @Handler@ function to return @Handler Renderer@.
--
-- > [tmpl| Page1 template/page1.html String |]
-- > [tmpl| Page2 template/page2.html String |]
-- > type API = "path" :> Capture "x" Int :> Get '[HTML] Renderer
-- > someHandler :: Int -> Handler Renderer
-- > someHandler v = return $ if v > 0 then Renderer (Proxy :: Proxy Page1) "abc" else Renderer (Proxy :: Proxy Page2) "def"
data HTML

-- | Holds a content type and a value to be rendered.
--
-- This type is designed to be used as the returned type of a @Handler@ to control content type by each request.
data Renderer = forall t a. (MimeRender t a)
    => Renderer { renderType :: Proxy t -- ^ Type of the content type.
                , renderValue :: a -- ^ Value to be rendered.
                }
     | EmptyContent

instance Accept HTML where
    contentType _ = C8.pack "text" // C8.pack "html"

instance (Accept a) => MimeRender a Renderer where
    mimeRender _ (Renderer t v) = mimeRender t v
    mimeRender _ EmptyContent = ""