{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ext.Servant.Validation where

import GHC.Generics
import Control.Applicative
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.List as L
import Language.Haskell.TH
import qualified Data.Text as T
import Data.Maybe (maybe)
import Data.Aeson as J
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
             , error :: Maybe ValidationError
             } deriving (Generic)

instance (FromJSON a) => FromJSON (F a) where
    parseJSON v =
        (parseJSON v :: Parser a) >>= \v' -> return (F (Just v') Nothing (toSource v) Nothing)
            <|> return (F Nothing Nothing (toSource v) (Just "Validation Error"))

instance (FromHttpApiData a) => FromHttpApiData (F a) where
    parseUrlPiece t =
        case (parseUrlPiece t :: Either T.Text a) of
            Right v -> Right (F (Just v) Nothing (toSource t) Nothing)
            Left _ -> Right (F Nothing Nothing (toSource t) (Just "Validation Error"))

class Validatable v a where
    validate :: v -> Maybe a

jsonOptions = defaultOptions { J.fieldLabelModifier = stripSuffix }
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
    > data A' = A' { f1 :: F String, f2 :: F Int } deriving (Generic)
    > instance FromJSON A'
    > instance FromForm A'
    >
    > instance Validatable A' A where
    >      validate v = A <$> value (f1 v) <*> value (f2 v)
-}
validatable :: [Name]
            -> Q [Dec]
validatable ns = concat <$> mapM conv ns
    where
        conv :: Name -> Q [Dec]
        conv n = do
            TyConI (DataD cxt name tvs kind (c@(RecC cn recs):_) drvs) <- reify n
            let (f0:fs) = map (\(rn, _, _) -> fn' rn) recs
            let vn = mkName "v"
            return [
                DataD [] n' [] Nothing [con' n' c] [DerivClause Nothing [(ConT ''Generic)]]
              , InstanceD Nothing [] (AppT (ConT ''FromJSON) (ConT n')) [
                  FunD 'parseJSON [Clause [] (NormalB $ AppE (VarE 'genericParseJSON) (VarE 'jsonOptions)) []]
                ]
              , InstanceD Nothing [] (AppT (ConT ''FromForm) (ConT n')) [
                  FunD 'fromForm [Clause [] (NormalB $ AppE (VarE 'genericFromForm) (VarE 'formOptions)) []]
                ]
              , InstanceD Nothing [] (AppT (AppT (ConT ''Validatable) (ConT n')) (ConT n)) [
                  FunD 'validate [Clause
                                    [VarP vn]
                                    (NormalB $ v' vn (InfixE (Just $ ConE cn) (VarE '(<$>)) (Just $ val f0 vn)) fs)
                                    []
                                    ]
                ]
              ]
            where
                n' = mkName $ nameBase n ++ "'"
                fn' n = mkName $ nameBase n ++ "'"
                app :: Exp -> Exp -> Exp
                app x y = InfixE (Just x) (VarE '(<*>)) (Just y)
                val fn vn = AppE (VarE 'value) (AppE (VarE fn) (SigE (VarE vn) (ConT n')))
                -- (<*> ((<$>) A (f1 v)) (f2 v))
                v' :: Name -> Exp -> [Name] -> Exp
                v' vn acc [] = acc
                v' vn acc [fn] = app acc (val fn vn)
                v' vn acc (fn:fns) = v' vn (app acc (val fn vn)) fns
                con' :: Name -> Con -> Con
                con' cn (RecC n recs) = RecC cn (map (\(rn, bang, ft) -> (fn' rn, bang, AppT (ConT ''F) (ft' ft))) recs)
                ft' :: Type -> Type
                ft' t = maybe t (ConT . fn') (isValidatable t)
                isValidatable :: Type -> Maybe Name
                isValidatable (ConT n) = n `L.elemIndex` ns >> return n
                isValidatable (AppT ListT t) = isValidatable t
                isValidatable _ = Nothing