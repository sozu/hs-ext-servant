{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Ext.Servant.Verifiers where

import GHC.TypeLits
import Data.Proxy
import Ext.Servant.Validation

-- ----------------------------------------------------------------
-- Integral verifiers
-- ----------------------------------------------------------------

data Range t (min :: Nat) (max :: Nat)

instance (Integral t, KnownNat min, KnownNat max) => Verifier (Range t min max) where
    type VerifiableType (Range t min max) = t
    type VerifierSpec (Range t min max) = '[Integer, Integer]

    verifierSpec _ = ("Range", natVal (Proxy :: Proxy min) `ACons` natVal (Proxy :: Proxy max) `ACons` ANil)

    verify p v = if iv < min || iv > max then Left () else Right v
        where
            iv = toInteger v
            (min, max) = (,) <-$ verifierArgs p

    verificationFailure p path _ = "Value of " ++ show path
                                    ++ " must be larger than " ++ show min
                                    ++ " and smaller than " ++ show max
        where
            (min, max) = (,) <-$ verifierArgs p

-- ----------------------------------------------------------------
-- String verifiers
-- ----------------------------------------------------------------

data Length s (min :: Nat) (max :: Nat)

instance (KnownNat min, KnownNat max) => Verifier (Length String min max) where
    type VerifiableType (Length String min max) = String
    type VerifierSpec (Length String min max) = '[Integer, Integer]

    verifierSpec _ = ("Length", natVal (Proxy :: Proxy min) `ACons` natVal (Proxy :: Proxy max) `ACons` ANil)

    verify p v = if len < min || len > max then Left () else Right v
        where
            len = toInteger $ length v
            (min, max) = (,) <-$ verifierArgs p

    verificationFailure p path _ = "Length of " ++ show path
                                    ++ " must be larger than " ++ show min
                                    ++ " and smaller than " ++ show max
        where
            (min, max) = (,) <-$ verifierArgs p