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

import GHC.Exts (toList)
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

-- ----------------------------------------------------------------
-- Types and functions used in validation
-- ----------------------------------------------------------------

-- | This type indicates the location of parsing value in data source.
data Pointer = RawPointer
             | IndexPointer Int
             | KeyPointer String
             deriving (Eq)

type PointerPath = [Pointer]

showPath :: PointerPath
         -> Bool
         -> String
showPath [] True = "(root)"
showPath [] False = ""
showPath (RawPointer : ps) True = showPath ps True
showPath (RawPointer : ps) False = showPath ps False
showPath (IndexPointer i : ps) _ = "[" ++ show i ++ "]" ++ showPath ps False
showPath (KeyPointer k : ps) True = k ++ showPath ps False
showPath (KeyPointer k : ps) False = "." ++ k ++ showPath ps False

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
data ValidationError' = ErrorString String
                      | ValueMissing
                      | forall t. TypeMismatch (Proxy t)
                      | forall v. (Verifier v) => VerificationFailure (Proxy v) (FailureHint v)

instance Show ValidationError' where
    show (ErrorString s) = s
    show (ValueMissing) = "Value is not found"
    show (TypeMismatch _) = "Value is not convertible"
    show (VerificationFailure v hint) = "Invalid value"

instance Eq ValidationError' where
    ErrorString s1 == ErrorString s2 = s1 == s2
    ValueMissing == ValueMissing = True
    TypeMismatch _ == TypeMismatch _ = True
    VerificationFailure v1 _ == VerificationFailure v2 _ = eqVerifier v1 v2
    _ == _ = False

data ValidationError = ValidationError PointerPath ValidationError' deriving (Eq)

(!@) :: PointerPath
     -> ValidationError'
     -> ValidationError
(!@) path e = ValidationError path e

instance Show ValidationError where
    show (ValidationError path err) = "[" ++ showPath path True ++ "] "
            ++ case err of
                ErrorString s -> s
                ValueMissing -> "Value is not found"
                TypeMismatch _ -> "Value is not convertible"
                VerificationFailure v hint -> verificationFailure v path hint

-- | Wrapper of @a@ holding the value or error cause according to the validation result of a field.
data F a = F { value :: Maybe a
             , source :: Source
             , cause :: Maybe ValidationError'
             } deriving (Generic)

-- | Instances of @Validatable@ supplies way to convert from some data source to valid object of @ValidationTarget v@.
--
-- @v@ is a data type which can hold values enough to construct @ValidationTarget v@ object,
-- and errors happened in conversion from data source to each field of @ValidationTarget v@.
-- @validatable ''A@ generates another type @A'@ which implements @Validatable A'@ with @ValidationTarget A' = A@.
class Validatable v where
    type ValidationTarget v :: *

    -- | Returns valid object of @a@ if the validation succeeded.
    validate :: v -- ^ Object holding field values of @a@ and validation errors.
             -> Maybe (ValidationTarget v) -- ^ @Just a@ if validation succeeded, otherwise Nothing.

    -- | Returns a list of validation errors.
    errors :: PointerPath -- ^ Base path indexing the location of this value.
           -> v -- ^ Object holding field values of @a@ and validation errors.
           -> [ValidationError] -- ^ List of validation errors.

errorsOf :: (Validatable v)
         => v
         -> [ValidationError]
errorsOf v = errors [] v

jsonOptions = defaultOptions { J.fieldLabelModifier = stripSuffix, J.omitNothingFields = True }
formOptions = defaultFormOptions { F.fieldLabelModifier = stripSuffix }

stripSuffix :: String -> String
stripSuffix = reverse . strip . reverse
    where
        strip ('\'':cs) = cs
        strip cs = cs

data FieldType = NormalScalar Type
               | NormalList Type
               | ValidatableScalar Type Type
               | ValidatableList Type Type
               | ValidatableMaybe Type Type

-- ----------------------------------------------------------------
-- TH function to generate types used in validation.
-- ----------------------------------------------------------------

{- | Declares new data type which has equivalent fields to given type.

    The name of the type and its fields are determined by appending @'@ to the origina name.
    In the new type, type of each field is converted by applying @F@ in following manners.

    - @Maybe a@ is changed to @F (Maybe a)@.
    - @[a]@ is changed to @F [F a]@.
    - Other types are simply applied by @F@, that is, @a@ is changed to @F a@.
    - If the original type @A@ is the type involved in the arguments of the same invocation of this function,
      @A'@ which is the new type of @A@ is used instead.

    Besides, several instances are defined to enable validation from data in the form of JSON or HTTP form.
    Next code is the example of generated code.
    (The implementations of @Validatable@ are omitted because they include elaborate part taken to deal with DuplicateRecordFields.)

    > data A = A { a1 :: Int, a2 :: [Int], a3 :: Maybe Int }
    > data B = B { b1 :: A, b2 :: [A], b3 :: Maybe A }
    >
    > -- $(validatable [''A, ''B]) generates code below.
    >
    > data A' = A' { a1' :: F Int, a2' :: F [F Int], a3' :: F (Maybe Int) } deriving (Generic)
    > data B' = B' { b1' :: F A', b2' :: F [F A'], b3' :: F (Maybe A') } deriving (Generic)
    >
    > instance FromJSONBetterErrors A' where
    >     fromJSONBetterErrors = A' <$> asField (Proxy :: Proxy (F Int)) (KeyPointer "a1")
    >                               <*> asField (Proxy :: Proxy (F [F Int])) (KeyPointer "a2")
    >                               <*> asField (Proxy :: Proxy (F (Maybe Int))) (KeyPointer "a3")
    > instance FromJSONBetterErrors B' where
    >     ...
    >
    > instance FromForm A' where
    >     fromForm f = A' <$> asFormField (Proxy :: Proxy (F Int)) "a1" f
    >                     <*> asFormField (Proxy :: Proxy (F [F Int])) "a2" f
    >                     <*> asFormField (Proxy :: Proxy (F (Maybe Int))) "a3" f
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
    > instance Validatable A' where
    >      type ValidationTarget A' = A
    >      validate v = A <$> ...
    >      errors p v = ...
    >
    > instance Validatable B' where
    >      type ValidationTarget B' = B
    >      validate v = B <$> ...
    >      errors p v = ...

    TODO: should generate instance of FromHttpApiData instead of FromForm ?
-}
validatable :: [Name]
            -> Q [Dec]
validatable ns = concat <$> mapM conv ns
    where
        conv :: Name -> Q [Dec]
        conv name = do
            TyConI (DataD _ _ tvs kind (c@(RecC cn recs):_) drvs) <- reify name
            let fields = map (\(rn, _, rt) -> (genFieldName rn, rt)) recs
            let vn = mkName "v"
            let pn = mkName "p"
            let c' = constructorOf n' c
            dJson <- deriveBetterFromJSON n' c'
            dForm <- deriveFromForm n' c'
            dValidatable <- [d|
                    instance Validatable $(conT n') where
                        type ValidationTarget $(conT n') = $(conT name)
                        validate $(varP vn) = $(validateFor cn vn fields)
                        errors $(varP pn) $(varP vn) = $(errorsFor pn vn fields)
                |]
            dParse <- [d|
                    instance AsType $(conT n') where
                        asType _ = fromJSONBetterErrors
                    instance AsFormField $(conT n') where
                        asFormField _ _ _ = Left $ T.pack "Nested type is not available as a form field."
                |]
            return $ concat [[
                DataD [] n' [] Nothing [constructorOf n' c] [DerivClause Nothing [(ConT ''Generic)]]
              , dJson
              , dForm
              ], dValidatable, dParse]
            where
                -- Name of validatable data type.
                n' = genTypeName name
                -- Generates new type name.
                genTypeName n = mkName $ nameBase n ++ "'"
                -- Generates new field (wrapped by @F@) name.
                genFieldName n = mkName $ nameBase n ++ "'"

                -- Applies another expression with (<*>), namely, app [| x |] [| y |] == [| x <*> y |].
                app :: Exp -> Exp -> Exp
                app x y = InfixE (Just x) (VarE '(<*>)) (Just y)

                -- Get field type of given type.
                -- Validatable type is replaced with its @Validatable@ version (qualified with @'@).
                fieldTypeOf :: Type -> FieldType
                fieldTypeOf t@(ConT n)
                    | n `L.elem` ns = ValidatableScalar (ConT n) (ConT $ genTypeName n)
                    | otherwise = NormalScalar t
                fieldTypeOf (AppT ListT t@(ConT n))
                    | n `L.elem` ns = ValidatableList (ConT n) (ConT $ genTypeName n)
                    | otherwise = NormalList t
                fieldTypeOf t@(AppT (ConT m) (ConT n))
                    | nameBase m == "Maybe" && n `L.elem` ns = ValidatableMaybe (ConT n) (ConT $ genTypeName n)
                    | otherwise = NormalScalar t
                fieldTypeOf t = NormalScalar t

                -- Generates an implementation of @validate@
                validateFor :: Name -> Name -> [(Name, Type)] -> ExpQ
                validateFor cn vn fields = applicativeCon cn <$> (mapM (appValueExpQ vn) fields)

                -- Generates an implementation of @errors@
                errorsFor :: Name -> Name -> [(Name, Type)] -> ExpQ
                errorsFor pn vn fields = appE (varE 'concat) (listE $ map (appCauseExpQ pn vn) fields)

                -- Generates expression obtaining valid data from a field.
                -- Normal type:              [| value                ((f1 :: A' -> F String)     (v :: A')) |]
                -- Validatable type:         [| (value >=> validate) ((f1 :: B' -> F A')         (v :: B')) |]
                -- List of normal type:      [| value                ((f1 :: A' -> F [F String]) (v :: A')) >>= sequence . map value |]
                -- List of validatable type: [| value                ((f1 :: B' -> F [F A'])     (v :: B')) >>= sequence . map (value >=> validate) |]
                -- Maybe of validatable type:[| value                ((f1 :: B' -> F (Maybe A')) (v :: B')) >>= id >>= return . validate |]
                appValueExpQ :: Name -> (Name, Type) -> ExpQ
                appValueExpQ vn (fn, ft) = do
                    let sigV = [| $(varE vn) :: $(conT n') |]
                    let sigF t = [| $(varE fn) :: $(conT n') -> F $(return t) |]
                    let fListT t = AppT ListT (AppT (ConT ''F) t)
                    let maybeT t = AppT (ConT ''Maybe) t
                    case fieldTypeOf ft of
                        NormalScalar t        -> [| value                ($(sigF t) $(sigV)) |]
                        ValidatableScalar _ t -> [| (value >=> validate) ($(sigF t) $(sigV)) |]
                        NormalList t          -> [| value                ($(sigF $ fListT t) $(sigV)) >>= sequence . map value |]
                        ValidatableList _ t   -> [| value                ($(sigF $ fListT t) $(sigV)) >>= sequence . map (value >=> validate) |]
                        ValidatableMaybe _ t  -> [| value                ($(sigF $ maybeT t) $(sigV)) >>= id >>= return . validate |]

                -- Generates expression obtaining list of errors from a field.
                -- f = (f1 :: A' -> F a) (v :: A')
                -- errX :: F x -> Maybe [ValidationError]
                -- errN p (f :: F a) = (:[]) <$> (p !@) <$> (cause f)
                -- errV p (f :: F A') = errN p f <|> errors p <$> (value f)
                -- errI p (i, f) err = err (p ++ [IndexPointer i]) f
                -- errNS p (f :: F [F a]) = errN p f <|> (value f >>= return . concat . catMaybes . map errNI . zip [0..])
                -- errVS p (f :: F [F A']) = errN p f <|> (value f >>= return . concat . catMaybes . map errVI . zip [0..])
                -- errMB p (f :: F (Maybe A')) = errN p f <|> (value f >>= id >>= return . errors p)
                -- maybe [] id (errX f)
                appCauseExpQ :: Name -> Name -> (Name, Type) -> ExpQ
                appCauseExpQ pn vn (fn, ft) = do
                    let f t = [| ($(varE fn) :: $(conT n') -> F $(return t)) ($(varE vn) :: $(conT n')) |]
                    let errN = [| (\p f -> (:[]) . (p !@) <$> cause f) |]
                    let errV t = [| (\p f -> $(errN) p f <|> (errors p <$> value f)) |]
                    let errI p err = map (\(i, f) -> err (p ++ [IndexPointer i]) f) . zip [0..]
                    let fListT t = AppT ListT (AppT (ConT ''F) t)
                    let path = [| $(varE pn) ++ [KeyPointer $ stripSuffix (nameBase fn)] |]
                    let errs = case fieldTypeOf ft of
                            NormalScalar t         -> [| let f' = $(f t) in $(errN) $(path) f' |]
                            ValidatableScalar t' t -> [| let f' = $(f t) in $(errV t') $(path) f' |]
                            NormalList t           -> [|
                                    let f' = $(f $ fListT t)
                                    in $(errN) $(path) f' <|> (value f' >>=
                                            return . concat . catMaybes . map (\(i, f) -> $(errN) ($(path) ++ [IndexPointer i]) f) . zip [0..]
                                        )
                                |]
                            ValidatableList t' t   -> [|
                                    let f' = $(f $ fListT t)
                                    in $(errN) $(path) f' <|> (value f' >>=
                                            return . concat . catMaybes . map (\(i, f) -> $(errV t') ($(path) ++ [IndexPointer i]) f) . zip [0..]
                                        )
                                |]
                            ValidatableMaybe t' t  -> let mt = AppT (ConT ''Maybe) t in [|
                                    let f' = $(f mt)
                                    in $(errN) $(path) f' <|> (value f' >>= id >>= return . errors $(path))
                                |]
                    [| maybe [] id $(errs) |]

                -- Generates data constructor by wrapping all types of fields with @F@.
                constructorOf :: Name -> Con -> Con
                constructorOf cn (RecC _ recs) = RecC cn (map (\(rn, bang, ft) -> (genFieldName rn, bang, fieldType ft)) recs)
                    where
                        fieldType t = case fieldTypeOf t of
                                        NormalScalar t        -> AppT (ConT ''F) t
                                        ValidatableScalar _ t -> AppT (ConT ''F) t
                                        NormalList t          -> AppT (ConT ''F) (AppT ListT (AppT (ConT ''F) t))
                                        ValidatableList _ t   -> AppT (ConT ''F) (AppT ListT (AppT (ConT ''F) t))
                                        ValidatableMaybe _ t  -> AppT (ConT ''F) (AppT (ConT ''Maybe) t)

-- ----------------------------------------------------------------
-- Verifiers
-- ----------------------------------------------------------------

-- | Instances should provide a verification with @v@ which determines the given @a@ is valid or not.
--
-- TODO: Because just Proxy is given, any data used in verification must be a type. Is it enough?
class (Eq (Args (VerifierSpec v))) => Verifier v where
    type VerifiableType v :: *
    type VerifierSpec v :: [*]
    type FailureHint v :: *
    type instance FailureHint v = ()

    verifierSpec :: Proxy v -> (String, Args (VerifierSpec v))

    verifierArgs :: Proxy v -> Args (VerifierSpec v)
    verifierArgs = snd . verifierSpec

    -- | Determines the @a@ value is valid or not.
    verify :: Proxy v -- ^ Verifier type.
           -> (VerifiableType v) -- ^ Value to verify.
           -> Either (FailureHint v) (VerifiableType v) -- ^ If valid, @Right@, otherwise @Left@.

    verify' :: Proxy v
            -> (VerifiableType v)
            -> Either ValidationError' (VerifiableType v)
    verify' p v = case verify p v of
                    Left h -> Left $ VerificationFailure p h
                    Right v' -> Right v'

    verificationFailure :: Proxy v
                        -> [Pointer]
                        -> FailureHint v
                        -> String
    verificationFailure _ ps hint = "Value at " ++ showPath ps True ++ " is invalid"

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

type family Apply (as :: [*]) (f :: *) :: *
type instance Apply '[] f = f
type instance Apply (a ': as) f = a -> Apply as f

-- | Declares an operator to extract arguments of @Verifier@ by applying a function which accepts them in the same order.
--
-- This operator provides shorthand way to obtain @Verifier@ arguments in @verify@ or @verificationFailure@.
-- See an example below which shows the usage of @<-$@ in @verify@.
--
-- > data MinAppear (a :: Symbol) (b :: Nat)
-- >
-- > instance (KnownSymbol a, KnownNat b) => Verifier (MinAppear a b) where
-- >     type VerifiableType (MinAppear a b) = String
-- >     type VerifierSpec (MinAppear a b) = '[a, b]
-- >
-- >     verifierSpec _ = ( "MinAppear"
-- >                      , symbolVal (Proxy :: Proxy a) `ACons` natVal (Proxy :: Proxy b) `ACons` ANil
-- >                      )
-- >
-- >     verify _ v = 
-- >         let (target, minCount) = (,) <-$ verifierArgs (Proxy :: Proxy MinAppear) :: (String, Integer)
-- >         in if minCount <= length (filter (== target) words v) then Right v else Left ()
class ExtractArgs args r where
    (<-$) :: Apply args r -> Args args -> r

instance ExtractArgs '[] r where
    (<-$) f _ = f
instance (ExtractArgs as r) => ExtractArgs (a ': as) r where
    (<-$) f (a `ACons` as) = f a <-$ as

-- | Type operator to qualifying a type with verifiers.
data (:?) a (vs :: [*]) = SafeData a (Proxy vs)

instance (Eq a) => Eq (a :? vs) where
    (==) (SafeData a1 _) (SafeData a2 _) = a1 == a2
instance (Show a) => Show (a :? vs) where
    show (SafeData a _) = show a

-- | Returns verified value.
safeData :: (a :? vs)
         -> a
safeData (SafeData v _) = v

class AllVerifiable (vs :: [*]) a where
    verifyAll :: Proxy vs -> a -> Either ValidationError' a

instance AllVerifiable '[] a where
    verifyAll _ a = Right a
instance (Verifier v, VerifiableType v ~ a, AllVerifiable vs a) => AllVerifiable (v ': vs) a where
    verifyAll _ a = verify' (Proxy :: Proxy v) a >>= verifyAll (Proxy :: Proxy vs)

-- ----------------------------------------------------------------
-- For JSON 
-- ----------------------------------------------------------------

-- | Generates declaration of function to parse given type in aeson-better-errors style.
--
--  > data A = A { f1 :: String, f2 :: Maybe Int }
--  > $(deriveBetterFromJSON ''A)
--  >
--  > instance FromJSONBetterErrors A where
--  >     fromJSONBetterErrors = A <$> asField (Proxy :: Proxy String) (KeyPointer "f1")
--  >                              <*> asField (Proxy :: Proxy (Maybe Int)) (KeyPointer "f2")
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
            recs <- mapM (\(rn, _, rt) -> [| asField (Proxy :: Proxy $(return rt)) (KeyPointer $ stripSuffix $ show rn) |]) rs
            return $ applicativeCon cn recs

class FromJSONBetterErrors a where
    fromJSONBetterErrors :: JB.Parse err a

instance {-# OVERLAPPABLE #-} (FromJSONBetterErrors a) => FromJSON a where
    parseJSON = JB.toAesonParser (\e -> T.pack "Validation failed") fromJSONBetterErrors

-- | Declares a method to get JSON parser by the type of a field.
--
-- Besides following instances, instance definition of every @Validatable@ type is generated by @validatable@.
class AsType a where
    -- | Returns JSON parser by a type.
    asType :: Proxy a -- ^ Type specifier.
           -> JB.Parse err a -- ^ JSON parser.

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

-- | Declares a method to get JSON parser by the type and the pointer to value in current parsing state.
--
-- This method is used to generate the instance of @FromJSONBetterErrors@
-- In the generated code, @Pointer@ given as second argument is always @KeyPointer@.
-- Although, it can be @RawPointer@ when invoked from other instance implementation.
class AsField a where
    -- | Returns JSON parser for a field.
    asField :: Proxy a -- ^ Type specifier for the field.
            -> Pointer -- ^ Pointer indicating target value to parse in current state.
            -> JB.Parse err a -- ^ JSON parser.

instance {-# OVERLAPPABLE #-} (AsType a) => AsField a where
    asField p (KeyPointer n) = JB.key (T.pack n) (asType p)
    asField p RawPointer = asType p
-- Without this instance, 'AsField [a]' has higher priority than '(AsType a) => AsField a'.
instance {-# OVERLAPPING #-} AsField [Char] where
    asField p (KeyPointer n) = JB.key (T.pack n) (asType p)
    asField p RawPointer = asType p
instance (AsType a) => AsField (Maybe a) where
    asField _ (KeyPointer n) = JB.keyOrDefault (T.pack n) Nothing (JB.perhaps $ asType (Proxy :: Proxy a))

instance {-# OVERLAPS #-} (AsField a) => AsField [a] where
    -- FIXME index information is lost.
    asField _ (KeyPointer n) = JB.key (T.pack n) $ JB.eachInArray
                                                 $ asField (Proxy :: Proxy a) RawPointer
instance {-# OVERLAPPABLE #-} (AsField a) => AsField (F a) where
    asField _ n = asField (Proxy :: Proxy (F (a :? '[]))) n
                    >>= \(F v s e) -> return (F (v >>= return . safeData) s e)
instance (AsField a, AllVerifiable vs a) => AsField (F (a :? vs)) where
    asField _ n = do
        let pvs = Proxy :: Proxy vs
        source <- JB.asValue >>= return . toSource
        (asField (Proxy :: Proxy a) n >>= \v -> do
                return $ case verifyAll pvs v of
                    Left e -> F Nothing source (Just e)
                    Right v' -> F (Just $ SafeData v' pvs) source Nothing
         ) `catchError` \e -> return $ case e of
                JB.InvalidJSON s -> F Nothing (StringValidatable s) (Just $ ErrorString "invalid JSON string")
                JB.BadSchema _ es -> case es of
                    JB.KeyMissing _ -> F Nothing source (Just $ ValueMissing)
                    JB.WrongType _ v -> F Nothing source (Just $ TypeMismatch (Proxy :: Proxy a))
                    JB.FromAeson s -> F Nothing source (Just $ ErrorString s)
                    JB.OutOfBounds _ -> F Nothing source (Just $ TypeMismatch (Proxy :: Proxy a))
                    JB.ExpectedIntegral _ -> F Nothing source (Just $ TypeMismatch (Proxy :: Proxy a))
                    _ -> F Nothing EmptyValidatable (Just $ ErrorString $ "unknown error")

-- ----------------------------------------------------------------
-- For HTTP form 
-- ----------------------------------------------------------------

-- | Generates instance of @FromForm@ for given type.
--
--  > data A = A { f1 :: String, f2 :: Maybe Int }
--  > $(deriveFromForm ''A)
--  >
--  > instance FromForm A where
--  >     fromForm f = A <$> asFormField (Proxy :: Proxy String) "f1" f
--  >                    <*> asFormField (Proxy :: Proxy (Maybe Int)) "f2" f
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

-- | Declares a method to parse HTTP form into field value.
class AsFormField a where
    -- | Parses a value in a form into field value of type @a@.
    asFormField :: Proxy a -- ^ Type specifier.
                -> String -- ^ Form key.
                -> Form -- ^ HTTP form.
                -> Either T.Text a -- ^ Parsed value or error message.

instance FromHttpApiData Object where
    parseUrlPiece _ = Left $ T.pack "Json object type can not be parsed by form data"

instance {-# OVERLAPPABLE #-} (FromHttpApiData a) => AsFormField a where
    asFormField _ n f = F.parseUnique (T.pack n) f
instance {-# OVERLAPPING #-} AsFormField String where
    asFormField _ n f = F.parseUnique (T.pack n) f

instance {-# OVERLAPS #-} (AsFormField a) => AsFormField (Maybe a) where
    asFormField _ n f = F.lookupMaybe (T.pack n) f >>= \v' -> 
        case v' of
            Nothing -> return Nothing
            Just v -> Just <$> asFormField (Proxy :: Proxy a) n f

instance {-# OVERLAPS #-} (AsFormField a) => AsFormField [a] where
    -- FIXME index information is lost.
    asFormField _ n f = mapM (\p -> asFormField (Proxy :: Proxy a) n $ F.toForm [(n, p)]) params
        where
            params = F.lookupAll (T.pack n) f

instance {-# OVERLAPPABLE #-} (AsFormField a) => AsFormField (F a) where
    asFormField _ n f = asFormField (Proxy :: Proxy (F (a :? '[]))) n f
                            >>= \(F v s e) -> return (F (v >>= return . safeData) s e)
instance {-# OVERLAPPABLE #-} (AsFormField a, AllVerifiable vs a) => AsFormField (F (a :? vs)) where
    asFormField _ n f =
        let pvs = Proxy :: Proxy vs
        in return $ case asFormField (Proxy :: Proxy a) n f of
                        Right v -> case verifyAll pvs v of
                                    Left e -> F Nothing EmptyValidatable (Just e)
                                    Right v' -> F (Just $ SafeData v' pvs) EmptyValidatable Nothing
                        Left e -> F Nothing EmptyValidatable (Just $ ErrorString $ T.unpack e)

-- ----------------------------------------------------------------
-- Helper functions
-- ----------------------------------------------------------------

-- | Generate an expression applying data constructo to expressions concatenated by @<*>@.
--
-- > applicativeCon 'A [exp1, exp2, exp3] == runQ [| A' <$> $(exp1) <*> $(exp2) <*> $(exp3) |]
applicativeCon :: Name -> [Exp] -> Exp
applicativeCon cn [] = ConE cn
applicativeCon cn (a:as) = applicativeAcc (InfixE (Just $ ConE cn) (VarE '(<$>)) (Just a)) as

-- | Generate an expression concatenating expressions by @<*>@.
--
-- > applicativeAcc exp1 [exp2, exp3] == runQ [| $(exp1) <*> $(exp2) <*> $(exp3) |]
applicativeAcc :: Exp -> [Exp] -> Exp
applicativeAcc b [] = b
applicativeAcc b (e:es) = applicativeAcc (InfixE (Just b) (VarE '(<*>)) (Just e)) es