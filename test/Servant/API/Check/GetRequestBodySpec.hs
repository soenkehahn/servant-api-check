{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Servant.API.Check.GetRequestBodySpec where

import           Data.Proxy
import           Servant.API
import           Test.Hspec

import           Servant.API.Check.CheckResult
import           Servant.API.Check.GetRequestBody
import           Test.Utils

check :: Proxy api -> Proxy (CheckGetRequestBody api)
check Proxy = Proxy

spec :: Spec
spec = do
  it "disallows request bodies in GET endpoints" $ do
    check (Proxy :: Proxy (ReqBody '[JSON] Int :> Get' Int))
      `shouldDemoteTo`
      Failure "GET endpoints are not allowed to have request bodies"

  it "allows request bodies in POST endpoints" $ do
    check (Proxy :: Proxy (ReqBody '[JSON] Int :> Post' Int))
      `shouldDemoteTo`
      success

  it "allows other combinators in GET endpoints" $ do
    check (Proxy :: Proxy (Capture "foo" Int :> Post' Int))
      `shouldDemoteTo`
      success

  context ":<|>" $ do
    it "disallows request bodies in GET endpoints" $ do
      check (Proxy :: Proxy (ReqBody '[JSON] Int :> Get' Int :<|> Raw))
        `shouldDemoteTo`
        Failure "GET endpoints are not allowed to have request bodies"
