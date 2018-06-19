{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Ext.Servant.Validation where

import GHC.Generics
import Control.Applicative
import Control.Monad
import Control.Monad.Except (catchError)
import Data.Proxy
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.List as L
import Language.Haskell.TH
import qualified Data.Text as T
import Data.Maybe (maybe, catMaybes)
import Data.Aeson as J
import qualified Data.Aeson.BetterErrors as JB
import Data.Aeson.Types
import Web.FormUrlEncoded as F
import Web.HttpApiData

data Pointer = RawPointer
             | IndexPointer Int
             | KeyPointer String
             deriving (Show, Eq)

-- TODO
-- needs extensible design.
data Source = StringValidatable String
            | ByteStringValidatable B.ByteString
            | TextValidatable T.Text
            | EmptyValidatable
            deriving (Show, Eq, Generic)

class ToSource a where
    toSource :: a -> Source

instance ToSource Value where
    toSource (Object v) = ByteStringValidatable (encode v)
    toSource (Array v) = ByteStringValidatable (encode v)
    toSource (String v) = TextValidatable (v)
    toSource (Number v) = StringValidatable (show v)
    toSource (Bool v) = StringValidatable (if v then "true" else "false")
    toSource Null = StringValidatable "null"

instance ToSource T.Text where
    toSource = TextValidatable

-- TODO
-- needs more flexible formatting library than Text.Printf or Formatting.
type ValidationError = String

data F a = F { value :: Maybe a
             , alternative :: Maybe a
             , source :: Source
             , cause :: Maybe ValidationError
             } deriving (Generic)

--instance {-# OVERLAPPABLE #-} (FromJSON a) => FromJSON (F a) where
--    parseJSON v =
--        (parseJSON v :: Parser a) >>= \v' -> return (F (Just v') Nothing (toSource v) Nothing)
--            <|> return (F Nothing Nothing (toSource v) (Just "Validation Error"))

instance {-# OVERLAPPING #-} (FromJSON a, AllVerifiable vs a) => FromJSON (F (a :? vs)) where
    parseJSON v =
        let source = toSource v
            pvs = Proxy :: Proxy vs
        in (parseJSON v :: Parser a) >>= \v' -> return (case verifyAll pvs v' of
                                                            Left e -> F Nothing Nothing source (Just e)
                                                            Right v'' -> F (Just $ SafeData v'' pvs) Nothing source Nothing)
            <|> return (F Nothing Nothing source (Just "Validation Error"))

instance (FromHttpApiData a) => FromHttpApiData (F a) where
    parseUrlPiece t =
        case (parseUrlPiece t :: Either T.Text a) of
            Right v -> Right (F (Just v) Nothing (toSource t) Nothing)
            Left _ -> Right (F Nothing Nothing (toSource t) (Just "Validation Error"))

class Validatable v a where
    validate :: v -> Maybe a
    errors :: Proxy a -> v -> [ValidationError]

jsonOptions = defaultOptions { J.fieldLabelModifier = stripSuffix, J.omitNothingFields = True }
formOptions = defaultFormOptions { F.fieldLabelModifier = stripSuffix }

stripSuffix :: String -> String
stripSuffix = reverse . strip . reverse
    where
        strip ('\'':cs) = cs
        strip cs = cs

{- | Declares new data type which has equivalent fields to given type.

    Type of each field is @F a@ where @a@ is the type of the equivalent field of sou type.

    > data A = A { f1 :: String, f2 :: Int } deriving (Generic)
    >
    > -- $(validatable [''A]) generates code below.
    >
    > data A' = A' { f1' :: F String, f2' :: F Int } deriving (Generic)
    >
    > instance FromJSONBetterErrors A' where
    >     fromJSONBetterErrors = A' <$> asField "f1" (Proxy :: Proxy (F String))
    >                               <*> asField "f2" (Proxy :: Proxy (F Int))
    >
    > instance FromForm A'
    >
    > instance Validatable A' A where
    >      validate v = A <$> value (f1' v) <*> value (f2' v)
    >      errors _ v = catMaybes [cause (f1' v), cause (f2' v)]
-}
validatable :: [Name]
            -> Q [Dec]
validatable ns = concat <$> mapM conv ns
    where
        conv :: Name -> Q [Dec]
        conv name = do
            TyConI (DataD _ _ tvs kind (c@(RecC cn recs):_) drvs) <- reify name
            let (f0:fs) = map (\(rn, _, _) -> fn' rn) recs
            let vn = mkName "v"
            let c' = con' n' c
            bfj <- deriveBetterFromJSON n' c'
            return [
                DataD [] n' [] Nothing [con' n' c] [DerivClause Nothing [(ConT ''Generic)]]
              , bfj
              , InstanceD Nothing [] (AppT (ConT ''FromForm) (ConT n')) [
                  FunD 'fromForm [Clause [] (NormalB $ AppE (VarE 'genericFromForm) (VarE 'formOptions)) []]
                ]
              , InstanceD Nothing [] (AppT (AppT (ConT ''Validatable) (ConT n')) (ConT name)) [
                  FunD 'validate [Clause
                                    [VarP vn]
                                    (NormalB $ v' vn (InfixE (Just $ ConE cn) (VarE '(<$>)) (Just $ val f0 vn)) fs)
                                    []
                                    ]
                , FunD 'errors [Clause
                                    [WildP, VarP vn]
                                    (NormalB $ AppE (VarE 'catMaybes) (fs' vn fs))
                                    []
                                    ]
                ]
              ]
            where
                -- Name of validatable data type.
                n' = tn' name
                -- Generates new type name.
                tn' n = mkName $ nameBase n ++ "'"
                -- Generates new field (wrapped by @F@) name.
                fn' n = mkName $ nameBase n ++ "'"
                -- Applies another expression with (<*>), namely, app [| x |] [| y |] == [| x <*> y |].
                app :: Exp -> Exp -> Exp
                app x y = InfixE (Just x) (VarE '(<*>)) (Just y)
                -- Generates expression where @value@ is applied to a field (fn) in an object (vn).
                val :: Name -> Name -> Exp
                val fn vn = AppE (VarE 'value) (AppE (VarE fn) (SigE (VarE vn) (ConT n')))
                fs' :: Name -> [Name] -> Exp
                fs' vn fs = ListE (map (\f -> AppE (VarE 'cause) (AppE (VarE f) (VarE vn))) fs)
                -- Generates body of @validate@ implementation by concatenating [| value (f1 v) |] expressions with (<*>).
                v' :: Name -> Exp -> [Name] -> Exp
                v' vn acc [] = acc
                v' vn acc [fn] = app acc (val fn vn)
                v' vn acc (fn:fns) = v' vn (app acc (val fn vn)) fns
                -- Generates data constructor by wrapping all types of fields with @F@.
                con' :: Name -> Con -> Con
                con' cn (RecC _ recs) = RecC cn (map (\(rn, bang, ft) -> (fn' rn, bang, AppT (ConT ''F) (ft' ft))) recs)
                -- Obtains a type wrapped by @F@. Original type is returned when it is @Validatable@ type.
                ft' :: Type -> Type
                ft' t = maybe t id (isValidatable t)
                -- Checks the type is @Validatable@ and returns its name if so.
                -- A type is considered to be @Validatable@
                -- only when its name is included in the argument of the same invocation of this function,
                -- or it is the list (or Maybe applied type in future) of @Validatable@ type.
                isValidatable :: Type -> Maybe Type
                isValidatable t@(ConT n) = n `L.elemIndex` ns >> return (ConT $ tn' n)
                isValidatable (AppT ListT t) = isValidatable t >>= Just . (AppT ListT)
                isValidatable _ = Nothing


----

class Verifier v a where
    verify :: Proxy v -> a -> Either ValidationError a

--instance Verifier (MinLen n) String where
--    verify p s = Right s
--instance Verifier (MaxLen n) String where
--    verify p s = Right s
--
--type PersonName = String :? '[MinLen 10, MaxLen 20]

--data (:?) a (vs :: [*]) = forall a vs. (AllVerifiable vs a) => SafeData a (Proxy vs)
data (:?) a (vs :: [*]) = SafeData a (Proxy vs)

safeData :: (a :? vs)
         -> a
safeData (SafeData v _) = v

class AllVerifiable (vs :: [*]) a where
    verifyAll :: Proxy vs -> a -> Either ValidationError a

instance AllVerifiable '[] a where
    verifyAll _ a = Right a

instance (Verifier v a, AllVerifiable vs a) => AllVerifiable (v ': vs) a where
    verifyAll _ a = verify (Proxy :: Proxy v) a >>= verifyAll (Proxy :: Proxy vs)

class FromJSONBetterErrors a where
    fromJSONBetterErrors :: JB.Parse' a

instance (FromJSONBetterErrors a) => FromJSON a where
    parseJSON = JB.toAesonParser' fromJSONBetterErrors

{- | Generates declaration of function to parse given type in aeson-better-errors style.

    > data A = A { f1 :: String, f2 :: Maybe Int }
    > $(deriveBetterFromJSON ''A)
    >
    > instance FromJSONBetterErrors A where
    >     fromJSONBetterErrors = A <$> key "f1" asString <*> keyMay "f2" asIntegral
-}
deriveBetterFromJSON :: Name
                     -> Con
                     -> DecQ
deriveBetterFromJSON n c@(RecC cn recs) = do
    --TyConI (DataD _ _ _ _ (RecC cn recs:_) _) <- reify n
    let fjbe = funD 'fromJSONBetterErrors [clause [] (normalB $ asFields cn recs) []]
    instanceD (cxt []) (appT (conT ''FromJSONBetterErrors) (conT n)) [fjbe]
    where
        asFields :: Name -> [(Name, Bang, Type)] -> ExpQ
        asFields cn rs = do
            recs <- mapM (\(rn, _, rt) -> [| asField (Proxy :: Proxy $(return rt)) (stripSuffix $ show rn) |]) rs
            return $ applicativeCon cn recs

        applicativeCon :: Name -> [Exp] -> Exp
        applicativeCon cn [] = ConE cn
        applicativeCon cn (a:as) = applicativeAcc (InfixE (Just $ ConE cn) (VarE '(<$>)) (Just a)) as

        applicativeAcc :: Exp -> [Exp] -> Exp
        applicativeAcc b [] = b
        applicativeAcc b (e:es) = applicativeAcc (InfixE (Just b) (VarE '(<*>)) (Just e)) es

class AsType a where
    asType :: Proxy a -> JB.Parse err a

instance AsType [Char] where
    asType _ = JB.asString
instance {-# OVERLAPPABLE #-} (Integral a) => AsType a where
    asType _ = JB.asIntegral
instance AsType Bool where
    asType _ = JB.asBool
instance {-# OVERLAPPABLE #-} (AsType a) => AsType [a] where
    asType _ = JB.eachInArray $ asType (Proxy :: Proxy a)

class AsField a where
    asField :: Proxy a -> String -> JB.Parse err a
instance {-# OVERLAPPABLE #-} (AsType a) => AsField a where
    asField p n = JB.key (T.pack n) (asType p)
instance (AsType a) => AsField (Maybe a) where
    asField _ n = JB.keyMay (T.pack n) (asType (Proxy :: Proxy a))
instance {-# OVERLAPPABLE #-} (AsField a) => AsField (F a) where
    -- FIXME alternativeが適当。
    asField _ n = asField (Proxy :: Proxy (F (a :? '[]))) n >>= \(F v a s e) -> return (F (v >>= return . safeData) Nothing s e)
instance (AsField a, AllVerifiable vs a) => AsField (F (a :? vs)) where
    asField _ n =
        let pvs = Proxy :: Proxy vs
        in (asField (Proxy :: Proxy a) n >>= \v -> return $
                case verifyAll pvs v of
                    Left e -> F Nothing Nothing EmptyValidatable (Just e)
                    Right v' -> F (Just $ SafeData v' pvs) Nothing EmptyValidatable Nothing
           ) `catchError` \e -> return $ case e of
                JB.InvalidJSON s -> F Nothing Nothing (StringValidatable s) (Just $ "invalid JSON string")
                JB.BadSchema _ es -> case es of
                    JB.KeyMissing _ -> F Nothing Nothing EmptyValidatable (Just $ "not exist: " ++ n)
                    JB.WrongType _ v -> F Nothing Nothing (toSource v) (Just $ "wrong type: " ++ n)
                    _ -> F Nothing Nothing EmptyValidatable (Just $ "unknown error")
