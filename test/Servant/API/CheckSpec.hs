{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.API.CheckSpec where

import           Data.Kind
import           Data.Proxy
import           Data.Typeable
import           GHC.Stack
import           Servant.API
-- import           Servant.API.Internal.Test.ComprehensiveAPI
import           Test.Hspec

import           Servant.API.Check
import           Servant.API.Check.CheckResult
import           Test.Utils

check :: Proxy api -> Proxy (CheckApi api)
check Proxy = Proxy

spec :: Spec
spec = do
  describe "Check" $ do
    it "allows simple GET endpoints" $ do
      check (Proxy :: Proxy (Get' Int))
        `shouldHaveDemoted`
        success

    it "disallows invalid endpoints after :<|>" $ do
      check (Proxy :: Proxy (Get' Int :<|> ()))
        `shouldHaveDemoted`
        Failure "invalid servant api: ()"

    validApiSpec (Proxy :: Proxy (QueryParam "foo" Int :> Get' Int))
    validApiSpec (Proxy :: Proxy ("foo" :> Get' Int))
    validApiSpec (Proxy :: Proxy ("foo" :> QueryParam "foo" Int :> Get' Int))
    validApiSpec (Proxy :: Proxy (Get' Int :<|> Post' Int))
    validApiSpec comprehensiveAPI

validApiSpec ::
  (?loc :: CallStack, Typeable api) =>
  (IsValid api) =>
  Proxy (api :: Type) -> Spec
validApiSpec proxy = do
  it ("allows (" ++ show (typeRep proxy) ++ ")") $ do
    let _x@() = isValid proxy
    True

-- | Allowing 'Vault' as a combinator triggers a bug in ghc,
-- so we can't go with 'comprehensiveAPI' from the 'servant'
-- package.
comprehensiveAPI :: Proxy ComprehensiveAPI
comprehensiveAPI = Proxy

type GET = Get '[JSON] NoContent

type ComprehensiveAPI =
  GET :<|>
  Get '[JSON] Int :<|>
  Capture "foo" Int :> GET :<|>
  Header "foo" Int :> GET :<|>
  HttpVersion :> GET :<|>
  IsSecure :> GET :<|>
  QueryParam "foo" Int :> GET :<|>
  QueryParams "foo" Int :> GET :<|>
  QueryFlag "foo" :> GET :<|>
  RemoteHost :> GET :<|>
  ReqBody '[JSON] Int :> GET :<|>
  Get '[JSON] (Headers '[Header "foo" Int] NoContent) :<|>
  "foo" :> GET :<|>
--  Vault :> GET :<|>
  Verb 'POST 204 '[JSON] NoContent :<|>
  Verb 'POST 204 '[JSON] Int :<|>
  WithNamedContext "foo" '[] GET :<|>
  CaptureAll "foo" Int :> GET :<|>
  GET :<|>
  Raw
