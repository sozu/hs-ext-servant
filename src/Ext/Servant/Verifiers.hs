{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Ext.Servant.Verifiers where

import GHC.TypeLits
import Data.Proxy
import Data.Char
import Language.Haskell.TH hiding (Range)
import Ext.Servant.Validation

-- ----------------------------------------------------------------
-- Integral verifiers
-- ----------------------------------------------------------------

-- | Verifier applied to @t@ and verify its value falls between @min@ and @max@.
--
-- Any @Integral@ type can be applied to this verifier.
data Range t (min :: Nat) (max :: Nat)

instance (Integral t, KnownNat min, KnownNat max) => Verifier (Range t min max) where
    type VerifiableType (Range t min max) = t
    type VerifierSpec (Range t min max) = '[Integer, Integer]

    verifierSpec _ = ("Range", natVal (Proxy :: Proxy min) `ACons` natVal (Proxy :: Proxy max) `ACons` ANil)

    verify p v = if iv < min || iv > max then Left () else Right v
        where
            iv = toInteger v
            (min, max) = (,) <-$ verifierArgs p

    verificationFailure p path _ = "Value of " ++ showPath path True
                                    ++ " must be larger than " ++ show min
                                    ++ " and smaller than " ++ show max
        where
            (min, max) = (,) <-$ verifierArgs p

-- ----------------------------------------------------------------
-- String verifiers
-- ----------------------------------------------------------------

-- | Verifier applied to @s@ and verify its length falls between @min@ and @max@.
--
-- TODO: Only an instance where @s@ is String is given currently. 
data Length s (min :: Nat) (max :: Nat)

instance (KnownNat min, KnownNat max) => Verifier (Length String min max) where
    type VerifiableType (Length String min max) = String
    type VerifierSpec (Length String min max) = '[Integer, Integer]

    verifierSpec _ = ("Length", natVal (Proxy :: Proxy min) `ACons` natVal (Proxy :: Proxy max) `ACons` ANil)

    verify p v = if len < min || len > max then Left () else Right v
        where
            len = toInteger $ length v
            (min, max) = (,) <-$ verifierArgs p

    verificationFailure p path _ = "Length of " ++ showPath path True
                                    ++ " must be larger than " ++ show min
                                    ++ " and smaller than " ++ show max
        where
            (min, max) = (,) <-$ verifierArgs p

-- | Instances of this class are designed to be used with @CharOf@ verifier.
--
-- With @CharOf@ verifier, input string is verified whether it consists of specified characters only.
class CharFilter f where
    -- | This symbol is used as the first argument of @CharOf@ verifier.
    type CharFilterSpec f :: Symbol
    -- | Filter function of characters in input string.
    filterChar :: Char -- ^ A character.
               -> Bool -- ^ True when this filter pass the character.

-- | Verifier applied to @String@ and verify it consists of characters all of which are passed filter function defined on @f@.
data CharOf f

instance (CharFilter f, KnownSymbol (CharFilterSpec f)) => Verifier (CharOf f) where
    type VerifiableType (CharOf f) = String
    type VerifierSpec (CharOf f) = '[String]

    verifierSpec _ = ("CharOf", symbolVal (Proxy :: Proxy (CharFilterSpec f)) `ACons` ANil)

    verify p [] = Right []
    verify p (c:cs)
        | filterChar @f c = (c:) <$> verify p cs
        | otherwise = Left ()

    verificationFailure p path _ = "Every character must be '" ++ spec ++ "'"
        where
            spec = id <-$ verifierArgs p

-- | TH function declaring @CharacterFilter@ data type.
declareCharFilter :: String -- ^ Name of generating data type.
                  -> String -- ^ Specifier of the type used for the @CharFilterSpec@ symbol.
                  -> Name -- ^ The name of filter function.
                  -> Q [Dec] -- ^ Declarations of data type and @CharFilter@ instance for it.
declareCharFilter n s f = (:) <$> (dataD (cxt []) cn [] Nothing [] []) <*> [d|
        instance CharFilter $(conT cn) where
            type CharFilterSpec $(conT cn) = $(litT $ strTyLit s)
            filterChar = $(varE f)
    |]
    where
        cn = mkName n