{-# LANGUAGE TypeOperators #-}

module Servant.API.Check.ShapeSpec where

import           Data.Proxy
import           Servant.API
import           Test.Hspec

import           Servant.API.Check.CheckResult
import           Servant.API.Check.Shape
import           Test.Utils

check :: Proxy api -> Proxy (CheckShape api)
check Proxy = Proxy

spec :: Spec
spec = do
  context ":>" $ do
    it "allows :>" $ do
      shouldDemoteTo
        (check (Proxy :: Proxy (() :> Get' Int)))
        success

    it "allows multiple :>" $ do
      shouldDemoteTo
        (check (Proxy :: Proxy (() :> () :> Get' Int)))
        success

    it "allows :<|>" $ do
      shouldDemoteTo
        (check (Proxy :: Proxy (Get' Int :<|> Post' Int)))
        success

  it "reports invalid apis" $ do
    shouldDemoteTo
      (check (Proxy :: Proxy ()))
      (Failure "invalid servant api: ()")
