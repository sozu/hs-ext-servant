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

-- | Data source from which each field value is assigned.
--
-- TODO: Currently unavailable.
data Source = StringValidatable String
            | ByteStringValidatable B.ByteString
            | TextValidatable T.Text
            | EmptyValidatable
            deriving (Show, Eq, Generic)

-- | Declares a method to convert a value to Source.
class ToSource a where
    -- | Converts a value to Source.
    toSource :: a -- ^ A value.
             -> Source -- ^ Converted Source value.

-- | Declares conversions from JSON component to Source.
instance ToSource Value where
    toSource (Object v) = ByteStringValidatable (encode v)
    toSource (Array v) = ByteStringValidatable (encode v)
    toSource (String v) = TextValidatable (v)
    toSource (Number v) = StringValidatable (show v)
    toSource (Bool v) = StringValidatable (if v then "true" else "false")
    toSource Null = StringValidatable "null"

instance ToSource T.Text where
    toSource = TextValidatable

-- | Declares types of causes for validation errors.
--
-- TODO: Data constructors corresponding to various errors will be defined.
type ValidationError = String

-- | Wrapper of @a@ holding the value or error cause according to the validation result of a field.
data F a = F { value :: Maybe a
             , alternative :: Maybe a
             , source :: Source
             , cause :: Maybe ValidationError
             } deriving (Show, Generic)

-- | Instances of @Validatable@ supplies way to convert from some data source to valid object of @a@.
-- @v@ is a data type which can hold values enough to construct @a@ object,
-- and errors happened in conversion from data source to each field of @a@.
-- @validatable ''A@, which is a TH function, generates another type @A'@ which implements @Validatable A' A@.
class Validatable v a where
    -- | Returns valid object of @a@ if the validation succeeded.
    validate :: v -- ^ Object holding field values of @a@ and validation errors.
             -> Maybe a -- ^ @Just a@ if validation succeeded, otherwise Nothing.

    -- | Returns a list of validation errors.
    errors :: Proxy a -- ^ Proxy to determine which instance to be used.
           -> v -- ^ Object holding field values of @a@ and validation errors.
           -> [ValidationError] -- ^ List of validation errors.

-- | Same as @errors@ but TypeApplication is available instead of @Proxy@.
--
-- > data A = ...
-- > instance Validatable A' A where
-- >     ...
-- > v :: A'
-- > v = ...
-- >
-- > errors (Proxy :: Proxy A) v == errors @A v
errorsOf :: forall a v. (Validatable v a)
         => v -- ^ Object holding field values of @a@ and validation errors.
         -> [ValidationError] -- ^ List of validation errors.
errorsOf v = errors (Proxy :: Proxy a) v

jsonOptions = defaultOptions { J.fieldLabelModifier = stripSuffix, J.omitNothingFields = True }
formOptions = defaultFormOptions { F.fieldLabelModifier = stripSuffix }

stripSuffix :: String -> String
stripSuffix = reverse . strip . reverse
    where
        strip ('\'':cs) = cs
        strip cs = cs

{- | Declares new data type which has equivalent fields to given type.

    The name of the type and its fields are determined by appending @'@ to the origina name.
    In the new type, type of every field is converted from @a@ to @F a@.

    Besides, several instances of new type is generated for the validation.
    Validations from JSON or HTTP form are available currently.

    > data A = A { f1 :: String, f2 :: Int } deriving (Generic)
    > data B = B { f1 :: String, f2 :: Int , f3 :: A } deriving (Generic)
    >
    > -- $(validatable [''A, ''B]) generates code below.
    >
    > data A' = A' { f1' :: F String, f2' :: F Int } deriving (Generic)
    > data B' = B' { f1' :: F String, f2' :: F Int, f3' :: F A' } deriving (Generic)
    >
    > instance FromJSONBetterErrors A' where
    >     fromJSONBetterErrors = A' <$> asField "f1" (Proxy :: Proxy (F String))
    >                               <*> asField "f2" (Proxy :: Proxy (F Int))
    > instance FromJSONBetterErrors B' where
    >     ...
    >
    > instance FromForm A' where
    >     fromForm f = A' <$> asFormField "f1" f
    >                     <*> asFormField "f2" f
    > instance FromForm B' where
    >     ...
    >
    > instance AsType A' where
    >     asType _ = fromJSONBetterErrors
    > instance AsType B' where
    >     ...
    >
    > instance AsFormField A' where
    >     asFormField _ _ _ = Left (T.pack "Nested type is not available as a form field")
    > instance AsFormField B' where
    >     ...
    >
    > instance Validatable A' A where
    >      validate v = A <$> value (f1' v) <*> value (f2' v)
    >      errors _ v = catMaybes [cause (f1' v), cause (f2' v)]
    >
    > instance Validatable B' B where
    >      validate v = B <$> value (f1' v) <*> value (f2' v) <*> (value >=> validate) (f3' v)
    >      errors _ v = catMaybes [cause (f1' v), cause (f2' v), cause (f3' v)]

    TODO: @errors@ does not work correctly in nested validatable types.
-}
validatable :: [Name]
            -> Q [Dec]
validatable ns = concat <$> mapM conv ns
    where
        conv :: Name -> Q [Dec]
        conv name = do
            TyConI (DataD _ _ tvs kind (c@(RecC cn recs):_) drvs) <- reify name
            let (f0:fs) = map (\(rn, _, rt) -> (fn' rn, rt)) recs
            let vn = mkName "v"
            let c' = con' n' c
            bfj <- deriveBetterFromJSON n' c'
            ff <- deriveFromForm n' c'
            return [
                DataD [] n' [] Nothing [con' n' c] [DerivClause Nothing [(ConT ''Generic)]]
              , bfj
              , ff
              , InstanceD Nothing [] (AppT (AppT (ConT ''Validatable) (ConT n')) (ConT name)) [
                  FunD 'validate [Clause
                                    [VarP vn]
                                    (NormalB $ v' vn (InfixE (Just $ ConE cn) (VarE '(<$>)) (Just $ val f0 vn)) fs)
                                    []
                                    ]
                , FunD 'errors [Clause
                                    [WildP, VarP vn]
                                    (NormalB $ AppE (VarE 'catMaybes) (fs' vn (f0:fs)))
                                    []
                                    ]
                ]
              , InstanceD Nothing [] (AppT (ConT ''AsType) (ConT n')) [
                  FunD 'asType [Clause
                                    [WildP]
                                    (NormalB $ VarE 'fromJSONBetterErrors)
                                    []
                                    ]
                ]
              , InstanceD Nothing [] (AppT (ConT ''AsFormField) (ConT n')) [
                  FunD 'asFormField [Clause
                                    [WildP, WildP, WildP]
                                    (NormalB $ AppE (ConE 'Left)
                                                    (AppE (VarE 'T.pack) (LitE $ StringL "Nested type is not available as a form field")))
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
                val :: (Name, Type) -> Name -> Exp
                val (fn, ft) vn =
                    let fe t = SigE (VarE fn) (AppT (AppT ArrowT (ConT n')) (AppT (ConT ''F) t))
                    in case isValidatable ft of
                        Nothing -> AppE (VarE 'value) (AppE (fe ft) (SigE (VarE vn) (ConT n')))
                        Just t' -> AppE (InfixE (Just $ VarE 'value) (VarE '(>=>)) (Just $ VarE 'validate)) (AppE (fe t') (SigE (VarE vn) (ConT n')))
                fs' :: Name -> [(Name, Type)] -> Exp
                fs' vn fs =
                    let fe fn t = SigE (VarE fn) (AppT (AppT ArrowT (ConT n')) (AppT (ConT ''F) t))
                    in ListE $ map (\(f, ft) -> case isValidatable ft of
                                                    Nothing -> AppE (VarE 'cause) (AppE (fe f ft) (VarE vn))
                                                    Just t' -> AppE (VarE 'cause) (AppE (fe f t') (VarE vn))
                                   ) fs
                -- Generates body of @validate@ implementation by concatenating [| value (f1 v) |] expressions with (<*>).
                v' :: Name -> Exp -> [(Name, Type)] -> Exp
                v' vn acc [] = acc
                v' vn acc [f] = app acc (val f vn)
                v' vn acc (f:fs) = v' vn (app acc (val f vn)) fs
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

-- | Instances should provide a verification with @v@ which determines the given @a@ is valid or not.
--
-- TODO: Because just Proxy is given, any data used in verification must be a type. Is it enough?
class Verifier v a where
    -- | Determines the @a@ value is valid or not.
    verify :: Proxy v -- ^ Verifier type.
           -> a -- ^ Value to verify.
           -> Either ValidationError a -- ^ If valid, @Right@, otherwise @Left@.

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
    fromJSONBetterErrors :: JB.Parse err a

instance {-# OVERLAPPABLE #-} (FromJSONBetterErrors a) => FromJSON a where
    parseJSON = JB.toAesonParser (\e -> T.pack "Validation feiled") fromJSONBetterErrors

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
instance AsType Object where
    asType _ = JB.asObject

class AsField a where
    asField :: Proxy a -> String -> JB.Parse err a
instance {-# OVERLAPPABLE #-} (AsType a) => AsField a where
    asField p n = JB.key (T.pack n) (asType p)
instance (AsType a) => AsField (Maybe a) where
    --asField _ n = JB.keyMay (T.pack n) (asType (Proxy :: Proxy a))
    asField _ n = JB.keyOrDefault (T.pack n) Nothing (JB.perhaps $ asType (Proxy :: Proxy a))
instance {-# OVERLAPPABLE #-} (AsField a) => AsField (F a) where
    -- FIXME alternativeが適当。
    asField _ n = asField (Proxy :: Proxy (F (a :? '[]))) n
                    >>= \(F v a s e) -> return (F (v >>= return . safeData) Nothing s e)
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

deriveFromForm :: Name
               -> Con
               -> DecQ
deriveFromForm n c@(RecC cn recs) = do
    let ff = funD 'fromForm [clause [varP $ mkName "f"] (normalB $ asFields cn recs) []]
    instanceD (cxt []) (appT (conT ''FromForm) (conT n)) [ff]
    where
        asFields :: Name -> [(Name, Bang, Type)] -> ExpQ
        asFields cn rs = do
            recs <- mapM (\(rn, _, rt) -> [| asFormField (Proxy :: Proxy $(return rt)) (stripSuffix $ show rn) f |]) rs
            return $ applicativeCon cn recs

class AsFormField a where
    asFormField :: Proxy a -> String -> Form -> Either T.Text a

instance  FromHttpApiData Object where
    parseUrlPiece _ = Left $ T.pack "Json object type can not be applied to form data"

instance {-# OVERLAPPABLE #-} (FromHttpApiData a) => AsFormField a where
    asFormField _ n f = F.parseUnique (T.pack n) f
instance (FromHttpApiData a) => AsFormField [a] where
    asFormField _ n f = F.parseAll (T.pack n) f
instance {-# OVERLAPPING #-} AsFormField String where
    asFormField _ n f = F.parseUnique (T.pack n) f
instance {-# OVERLAPS #-} (FromHttpApiData a) => AsFormField (Maybe a) where
    asFormField _ n f = F.parseMaybe (T.pack n) f
instance {-# OVERLAPPABLE #-} (AsFormField a) => AsFormField (F a) where
    asFormField _ n f = asFormField (Proxy :: Proxy (F (a :? '[]))) n f
                            >>= \(F v a s e) -> return (F (v >>= return . safeData) Nothing s e)
instance {-# OVERLAPPABLE #-} (AsFormField a, AllVerifiable vs a) => AsFormField (F (a :? vs)) where
    asFormField _ n f =
        let pvs = Proxy :: Proxy vs
        in return $ case asFormField (Proxy :: Proxy a) n f of
                        Right v -> case verifyAll pvs v of
                                    Left e -> F Nothing Nothing EmptyValidatable (Just e)
                                    Right v' -> F (Just $ SafeData v' pvs) Nothing EmptyValidatable Nothing
                        Left e -> F Nothing Nothing EmptyValidatable (Just $ T.unpack e)

applicativeCon :: Name -> [Exp] -> Exp
applicativeCon cn [] = ConE cn
applicativeCon cn (a:as) = applicativeAcc (InfixE (Just $ ConE cn) (VarE '(<$>)) (Just a)) as

applicativeAcc :: Exp -> [Exp] -> Exp
applicativeAcc b [] = b
applicativeAcc b (e:es) = applicativeAcc (InfixE (Just b) (VarE '(<*>)) (Just e)) es