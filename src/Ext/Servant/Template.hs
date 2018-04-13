{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Ext.Servant.Template where

import System.IO.Unsafe
import Data.IORef
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

{- | Generate declarations to use a file as an HTML template in Servant.

    This function generates type implementing @MimeRender@ for each HTML template.

    @tmpl@ generates code which compiles template at compile time.
    > instance MimeRender [TemplateName] where
    >     mimeRender _ it = fromString $(render path)

    @tmpld@ implements @MimeRender@ to compile template for each time @mimeRender@ is invoked.
    > cache_[TemplateName] = unsafePerformIO $ newIORef Nothing
    > instance MimeRender [TemplateName] where
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
    mr <- if statically
                then return $ funD 'mimeRender [clause [wildP, varP $ mkName "it"] (normalB (appE (varE 'UTF8.fromString) (render path))) []]
                else do
                    cacheName <- newName $ "cache_" ++ showName n 
                    -- cache_Name = unsafePerformIO $ newIORef Nothing
                    cache <- funD cacheName [clause [] (normalB (appE (varE 'unsafePerformIO) (appE (varE 'newIORef) (conE 'Nothing)))) []]
                    addTopDecls [cache]
                    let f = appE (appE (appE [| renderRuntime newContext path |] imports) [| (it, "it", fromJust $ EXP.getType t) |]) (varE cacheName)
                    return $ funD 'mimeRender [clause [wildP, varP $ mkName "it"]
                                                      (normalB (appE (varE 'UTF8.fromString) (appE (varE 'unsafePerformIO) f)))
                                                      []]
    mime <- instanceD (cxt []) (appT (appT (conT ''MimeRender) (conT n)) (getType t)) [mr]
    let bs v = Just (appE (varE 'C8.pack) (litE $ stringL v))
    let ct = funD 'contentType [clause [wildP] (normalB (infixE (bs "text") (varE '(//)) (bs "html"))) []]
    accept <- instanceD (cxt []) (appT (conT ''Accept) (conT n)) [ct]
    return $ [dt, mime, accept]