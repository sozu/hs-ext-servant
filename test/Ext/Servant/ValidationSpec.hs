{-# LANGUAGE OverloadedStrings #-}

module Ext.Servant.ValidationSpec where

import Test.Hspec
import Data.Proxy
import Data.Aeson.BetterErrors
import Ext.Servant.Validation

data V1 = V1 { f11 :: F String
             , f12 :: F Int
             , f13 :: F Bool
             }

instance FromJSONBetterErrors V1 where
    fromJSONBetterErrors = V1 <$> asField (Proxy :: Proxy (F String)) (KeyPointer "f11")
                              <*> asField (Proxy :: Proxy (F Int)) (KeyPointer "f12")
                              <*> asField (Proxy :: Proxy (F Bool)) (KeyPointer "f13")

data V2 = V2 { f21 :: F (Maybe String)
             , f22 :: F (Maybe Int)
             }

instance FromJSONBetterErrors V2 where
    fromJSONBetterErrors = V2 <$> asField (Proxy :: Proxy (F (Maybe String))) (KeyPointer "f21")
                              <*> asField (Proxy :: Proxy (F (Maybe Int))) (KeyPointer "f22")

spec :: Spec
spec = do
    describe "validation of primitive types" $ do
        it "valid" $ do
            let res = parse (fromJSONBetterErrors :: Parse' V1) $ "{\
                        \ \"f11\": \"test\", \
                        \ \"f12\": 12, \
                        \ \"f13\": true \
                        \ }"
            case res of
                Right v -> do
                    value (f11 v) `shouldBe` Just "test"
                    value (f12 v) `shouldBe` Just 12
                    value (f13 v) `shouldBe` Just True

        it "invalid integer" $ do
            let res = parse (fromJSONBetterErrors :: Parse' V1) $ "{\
                        \ \"f11\": \"test\", \
                        \ \"f12\": \"abc\", \
                        \ \"f13\": true \
                        \ }"
            case res of
                Right v -> do
                    value (f11 v) `shouldBe` Just "test"
                    value (f12 v) `shouldBe` Nothing
                    cause (f12 v) `shouldBe` Just (TypeMismatch [KeyPointer "f12"] (Proxy :: Proxy Int))
                    value (f13 v) `shouldBe` Just True

    describe "validation of Maybes" $ do
        it "valid" $ do
            let res = parse (fromJSONBetterErrors :: Parse' V2) $ "{\
                        \ \"f21\": \"test\" \
                        \ }"
            case res of
                Right v -> do
                    value (f21 v) `shouldBe` Just (Just "test")
                    value (f22 v) `shouldBe` Just Nothing

        it "invalid interger" $ do
            let res = parse (fromJSONBetterErrors :: Parse' V2) $ "{\
                        \ \"f21\": \"test\", \
                        \ \"f22\": \"abc\" \
                        \ }"
            case res of
                Right v -> do
                    value (f21 v) `shouldBe` Just (Just "test")
                    value (f22 v) `shouldBe` Nothing
                    cause (f22 v) `shouldBe` Just (TypeMismatch [KeyPointer "f22"] (Proxy :: Proxy (Maybe Int)))