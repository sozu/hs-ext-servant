{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Ext.Servant.THSpec where

import Test.Hspec
import GHC.Generics
import Data.Proxy
import Data.Aeson
import Ext.Servant.Validation

data ForInt = ForInt { int :: Int } deriving (Eq, Show, Generic)
data ForStr = ForStr { str :: String } deriving (Eq, Show, Generic)
data ForIntList = ForIntList { intlist :: [Int] } deriving (Eq, Show, Generic)
data ForStrList = ForStrList { strlist :: [String] } deriving (Eq, Show, Generic)
data ForVal = ForVal { val :: ForInt } deriving (Eq, Show, Generic)
data ForValList = ForValList { vallist :: [ForInt] } deriving (Eq, Show, Generic)
data ForIntMaybe = ForIntMaybe { intmaybe :: Maybe Int } deriving (Eq, Show, Generic)
data ForValMaybe = ForValMaybe { valmaybe :: Maybe ForInt } deriving (Eq, Show, Generic)

$(validatable [''ForInt, ''ForStr, ''ForIntList, ''ForStrList, ''ForVal, ''ForValList, ''ForIntMaybe, ''ForValMaybe])

spec :: Spec
spec = do
    describe "validation of int field" $ do
        it "valid" $ do
            let r = fromJSON $ object [("int", Number 1)] :: Result ForInt'
            case r of
                Success r' -> validate r' `shouldBe` Just (ForInt 1)
                Error e -> expectationFailure e

    describe "validation of string field" $ do
        it "valid" $ do
            let r = fromJSON $ object [("str", String "abc")] :: Result ForStr'
            case r of
                Success r' -> validate r' `shouldBe` Just (ForStr "abc")
                Error e -> expectationFailure e

    describe "validation of int list field" $ do
        it "valid" $ do
            let r = fromJSON $ object [("intlist", toJSON ([1,2,3] :: [Int]))] :: Result ForIntList'
            case r of
                Success r' -> validate r' `shouldBe` Just (ForIntList [1,2,3])
                Error e -> expectationFailure e

    describe "validation of string list field" $ do
        it "valid" $ do
            let r = fromJSON $ object [("strlist", toJSON (["abc", "def", "ghi"] :: [String]))] :: Result ForStrList'
            case r of
                Success r' -> validate r' `shouldBe` Just (ForStrList ["abc", "def", "ghi"])
                Error e -> expectationFailure e

    describe "validation of validatable field" $ do
        it "valid" $ do
            let r = fromJSON $ object [("val", object [("int", Number 1)])] :: Result ForVal'
            case r of
                Success r' -> validate r' `shouldBe` Just (ForVal (ForInt 1))
                Error e -> expectationFailure e

    describe "validation of validatable list field" $ do
        it "valid" $ do
            let r = fromJSON $ object [("vallist", toJSON [ object [("int", Number 1)]
                                                          , object [("int", Number 2)]
                                                          , object [("int", Number 3)]
                                                          ])] :: Result ForValList'
            case r of
                Success r' -> validate r' `shouldBe` Just (ForValList [ForInt 1, ForInt 2, ForInt 3])
                Error e -> expectationFailure e

    describe "validation of int maybe field" $ do
        it "valid" $ do
            let r = fromJSON $ object [("intmaybe", Number 1)] :: Result ForIntMaybe'
            case r of
                Success r' -> validate r' `shouldBe` Just (ForIntMaybe $ Just 1)
                Error e -> expectationFailure e

        it "null" $ do
            let r = fromJSON $ object [("intmaybe", Null)] :: Result ForIntMaybe'
            case r of
                Success r' -> validate r' `shouldBe` Just (ForIntMaybe $ Nothing)
                Error e -> expectationFailure e

        it "empty" $ do
            let r = fromJSON $ object [] :: Result ForIntMaybe'
            case r of
                Success r' -> validate r' `shouldBe` Just (ForIntMaybe $ Nothing)
                Error e -> expectationFailure e