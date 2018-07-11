{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}

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
             deriving (Eq)

type PointerPath = [Pointer]

instance {-# OVERLAPPING #-} Show PointerPath where
    showsPrec 10 [] = showString "(root)"
    showsPrec 10 (RawPointer : []) = showString "(root)"
    showsPrec 10 (IndexPointer i : ps) = \s -> "[" ++ show i ++ "]" ++ showsPrec 9 ps s
    showsPrec 10 (KeyPointer k : ps) = \s -> k ++ showsPrec 9 ps s
    showsPrec d [] = showString $ ""
    showsPrec d (RawPointer:ps) = showsPrec d ps
    showsPrec d (IndexPointer i:ps) = \s -> "[" ++ show i ++ "]" ++ showsPrec d ps s
    showsPrec d (KeyPointer k:ps) = \s -> "." ++ k ++ showsPrec d ps s

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
data ValidationError = ErrorString String
                     | ValueMissing PointerPath
                     | forall t. TypeMismatch PointerPath (Proxy t)
                     | forall v. (Verifier v) => VerificationFailure PointerPath (Proxy v)

instance Eq ValidationError where
    ErrorString s1 == ErrorString s2 = s1 == s2
    ValueMissing p1 == ValueMissing p2 = p1 == p2
    TypeMismatch p1 _ == TypeMismatch p2 _ = p1 == p2
    VerificationFailure p1 v1 == VerificationFailure p2 v2 = p1 == p2 && eqVerifier v1 v2
    _ == _ = False

instance Show ValidationError where
    show (ErrorString s) = s
    show (ValueMissing ps) = "Value is not found at " ++ show ps
    show (TypeMismatch ps _) = "Value at " ++ show ps ++ " is not convertible"
    show (VerificationFailure ps v) = verificationFailure v ps

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
class (Eq (Args (VerifierSpec v))) => Verifier v where
    type VerifiableType v :: *
    type VerifierSpec v :: [*]

    verifierSpec :: Proxy v -> (String, Args (VerifierSpec v))

    -- | Determines the @a@ value is valid or not.
    verify :: Proxy v -- ^ Verifier type.
           -> (VerifiableType v) -- ^ Value to verify.
           -> Either ValidationError (VerifiableType v) -- ^ If valid, @Right@, otherwise @Left@.

    verificationFailure :: Proxy v
                        -> [Pointer]
                        -> String
    verificationFailure _ ps = "Value at " ++ show ps ++ " is invalid"

class EqVerifier v1 v2 where
    eqVerifier :: Proxy v1 -> Proxy v2 -> Bool

instance {-# OVERLAPPING #-} (Verifier v) => EqVerifier v v where
    eqVerifier p1 p2 = verifierSpec p1 == verifierSpec p2
instance (Verifier v1, Verifier v2) => EqVerifier v1 v2 where
    eqVerifier _ _ = False

data Args (args :: [*]) where
    ANil :: Args '[]
    ACons :: (Eq a) => a -> Args as -> Args (a ': as)

infixr 2 `ACons`

instance Eq (Args '[]) where
    (==) _ _ = True
instance (Eq a, Eq (Args as)) => Eq (Args (a ': as)) where
    (==) (a1 `ACons` as1) (a2 `ACons` as2) = a1 == a2 && as1 == as2


--
--class Shift f args where
--    shift :: f -> Args args -> r
--
--instance (Shift r as) => Shift (a -> r) (a ': as) where
--    shift f (a `ACons` as) = shift (f a) as
--
--shiftTest :: (Int, Bool, String)
--shiftTest = (,,) `shift` (1 `ACons` True `ACons` "abc" `ACons` ANil :: Args '[Int, Bool, String])
--

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

instance (Verifier v, VerifiableType v ~ a, AllVerifiable vs a) => AllVerifiable (v ': vs) a where
    verifyAll _ a = verify (Proxy :: Proxy v) a >>= verifyAll (Proxy :: Proxy vs)

class FromJSONBetterErrors a where
    fromJSONBetterErrors :: JB.Parse err a

instance {-# OVERLAPPABLE #-} (FromJSONBetterErrors a) => FromJSON a where
    parseJSON = JB.toAesonParser (\e -> T.pack "Validation failed") fromJSONBetterErrors

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
    asField _ n = do
        let pvs = Proxy :: Proxy vs
        source <- JB.asValue >>= return . toSource
        (asField (Proxy :: Proxy a) n >>= \v -> do
                return $ case verifyAll pvs v of
                    Left e -> F Nothing Nothing source (Just e)
                    Right v' -> F (Just $ SafeData v' pvs) Nothing source Nothing
         ) `catchError` \e -> return $ case e of
                JB.InvalidJSON s -> F Nothing Nothing (StringValidatable s) (Just $ ErrorString "invalid JSON string")
                JB.BadSchema _ es -> case es of
                    JB.KeyMissing _ -> F Nothing Nothing source (Just $ ValueMissing [KeyPointer n])
                    JB.WrongType _ v -> F Nothing Nothing source (Just $ TypeMismatch [KeyPointer n] (Proxy :: Proxy a))
                    JB.FromAeson s -> F Nothing Nothing source (Just $ ErrorString s)
                    JB.OutOfBounds _ -> F Nothing Nothing source (Just $ TypeMismatch [KeyPointer n] (Proxy :: Proxy a))
                    JB.ExpectedIntegral _ -> F Nothing Nothing source (Just $ TypeMismatch [KeyPointer n] (Proxy :: Proxy a))
                    _ -> F Nothing Nothing EmptyValidatable (Just $ ErrorString $ "unknown error")

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
                        Left e -> F Nothing Nothing EmptyValidatable (Just $ ErrorString $ T.unpack e)

applicativeCon :: Name -> [Exp] -> Exp
applicativeCon cn [] = ConE cn
applicativeCon cn (a:as) = applicativeAcc (InfixE (Just $ ConE cn) (VarE '(<$>)) (Just a)) as

applicativeAcc :: Exp -> [Exp] -> Exp
applicativeAcc b [] = b
applicativeAcc b (e:es) = applicativeAcc (InfixE (Just b) (VarE '(<*>)) (Just e)) es