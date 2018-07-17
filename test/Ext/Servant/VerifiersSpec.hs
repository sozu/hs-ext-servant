{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Ext.Servant.VerifiersSpec where

import Test.Hspec
import Control.Monad
import Data.Proxy
import Data.Maybe (fromJust)
import Data.Aeson
import Ext.Servant.Validation
import Ext.Servant.Verifiers
import Data.Char

$(declareCharFilter "Alphabet" "alphabet" 'isAlpha)

type Length_5_10 = String :? '[Length String 5 10]
type Range_5_10 = Int :? '[Range Int 5 10]
type AlphabetString = String :? '[CharOf Alphabet]

data LengthTest = LengthTest { str :: [Length_5_10] } deriving (Eq, Show)
data RangeTest = RangeTest { int :: [Range_5_10] } deriving (Eq, Show)
data AlphabetTest = AlphabetTest { alphabet :: [AlphabetString] } deriving (Eq, Show)

$(validatable [''LengthTest, ''RangeTest, ''AlphabetTest])

spec :: Spec
spec = do
    describe "Length" $ do
        it "valid" $ do
            let r = fromJSON $ object [("str", toJSON (["01234", "0123456789"] :: [String]))] :: Result LengthTest'
            case r of
                Success r' -> map safeData . str <$> validate r' `shouldBe` Just ["01234", "0123456789"]
                Error e -> expectationFailure e

        it "too short" $ do
            let r = fromJSON $ object [("str", toJSON (["0123", "0123456789"] :: [String]))] :: Result LengthTest'
            case r of
                Success r' -> do
                    validate r' `shouldBe` Nothing
                    map ((return . safeData) <=< value) (fromJust $ value $ str' r') `shouldBe` [Nothing, Just "0123456789"]
                Error e -> expectationFailure e

        it "too long" $ do
            let r = fromJSON $ object [("str", toJSON (["01234", "0123456789a"] :: [String]))] :: Result LengthTest'
            case r of
                Success r' -> do
                    validate r' `shouldBe` Nothing
                    map ((return . safeData) <=< value) (fromJust $ value $ str' r') `shouldBe` [Just "01234", Nothing]
                Error e -> expectationFailure e

    describe "Range" $ do
        it "valid" $ do
            let r = fromJSON $ object [("int", toJSON ([5, 10] :: [Int]))] :: Result RangeTest'
            case r of
                Success r' -> map safeData . int <$> validate r' `shouldBe` Just [5, 10]
                Error e -> expectationFailure e

        it "too small" $ do
            let r = fromJSON $ object [("int", toJSON ([4, 10] :: [Int]))] :: Result RangeTest'
            case r of
                Success r' -> do
                    validate r' `shouldBe` Nothing
                    map ((return . safeData) <=< value) (fromJust $ value $ int' r') `shouldBe` [Nothing, Just 10]
                Error e -> expectationFailure e

        it "too large" $ do
            let r = fromJSON $ object [("int", toJSON ([5, 11] :: [Int]))] :: Result RangeTest'
            case r of
                Success r' -> do
                    validate r' `shouldBe` Nothing
                    map ((return . safeData) <=< value) (fromJust $ value $ int' r') `shouldBe` [Just 5, Nothing]
                Error e -> expectationFailure e

    describe "Alphabet" $ do
        it "valid" $ do
            let r = fromJSON $ object [("alphabet", toJSON (["abc", "def"] :: [String]))] :: Result AlphabetTest'
            case r of
                Success r' -> map safeData . alphabet <$> validate r' `shouldBe` Just ["abc", "def"]
                Error e -> expectationFailure e

        it "invalid" $ do
            let r = fromJSON $ object [("alphabet", toJSON (["a0c", "def"] :: [String]))] :: Result AlphabetTest'
            case r of
                Success r' -> do
                    validate r' `shouldBe` Nothing
                    map ((return . safeData) <=< value) (fromJust $ value $ alphabet' r') `shouldBe` [Nothing, Just "def"]
                Error e -> expectationFailure e