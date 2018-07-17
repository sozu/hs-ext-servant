{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Ext.Servant.THFormSpec where

import Test.Hspec
import GHC.Generics
import Data.Proxy
import qualified Data.Text as T
import Web.FormUrlEncoded
import Ext.Servant.Validation
import Debug.Trace

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
            let r = fromForm $ toForm [("int", "1")] :: Either T.Text ForInt'
            case r of
                Right r' -> validate r' `shouldBe` Just (ForInt 1)
                Left e -> expectationFailure (T.unpack e)

    describe "validation of string field" $ do
        it "valid" $ do
            let r = fromForm $ toForm [("str", "abc")] :: Either T.Text ForStr'
            case r of
                Right r' -> validate r' `shouldBe` Just (ForStr "abc")
                Left e -> expectationFailure (T.unpack e)

    describe "validation of int list field" $ do
        it "valid" $ do
            let r = fromForm $ toForm [("intlist", "1"), ("intlist", "2"), ("intlist", "3")] :: Either T.Text ForIntList'
            case r of
                Right r' -> validate r' `shouldBe` Just (ForIntList [1,2,3])
                Left e -> expectationFailure (T.unpack e)

    describe "validation of string list field" $ do
        it "valid" $ do
            let r = fromForm $ toForm [("strlist", "abc"), ("strlist", "def"), ("strlist", "ghi")] :: Either T.Text ForStrList'
            case r of
                Right r' -> validate r' `shouldBe` Just (ForStrList ["abc", "def", "ghi"])
                Left e -> expectationFailure (T.unpack e)

    describe "validation of validatable field from HTTP form must fail" $ do
        it "valid" $ do
            let r = fromForm $ toForm [("val", "1")] :: Either T.Text ForVal'
            case r of
                Right r' -> validate r' `shouldBe` (Nothing :: Maybe ForVal)
                Left e -> expectationFailure (T.unpack e)

    describe "validation of validatable list field from HTTP form must fail" $ do
        it "valid" $ do
            let r = fromForm $ toForm [ ("vallist", "1")
                                      , ("vallist", "2")
                                      , ("vallist", "3")
                                      ] :: Either T.Text ForValList'
            case r of
                Right r' -> validate r' `shouldBe` (Nothing :: Maybe ForValList)
                Left e -> expectationFailure (T.unpack e)

    describe "validation of int maybe field" $ do
        it "valid" $ do
            let r = fromForm $ toForm [("intmaybe", "1")] :: Either T.Text ForIntMaybe'
            case r of
                Right r' -> validate r' `shouldBe` Just (ForIntMaybe $ Just 1)
                Left e -> expectationFailure (T.unpack e)

        it "empty" $ do
            let r = fromForm $ toForm ([] :: [(String, String)]) :: Either T.Text ForIntMaybe'
            case r of
                Right r' -> validate r' `shouldBe` Just (ForIntMaybe $ Nothing)
                Left e -> expectationFailure (T.unpack e)