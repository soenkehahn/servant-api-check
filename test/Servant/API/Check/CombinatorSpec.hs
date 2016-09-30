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

module Servant.API.Check.CombinatorSpec where

import           Data.Proxy
import           Servant.API
import           Test.Hspec

import           Servant.API.Check
import           Servant.API.Check.CheckResult
import           Servant.API.Check.Combinator
import           Test.Utils

data Custom

instance IsCombinator Custom

check :: Proxy api -> Proxy (CheckCombinators api)
check Proxy = Proxy

spec :: Spec
spec = do
  describe "check" $ do
    it "works for valid apis" $ do
      check (Proxy :: Proxy (Get' Int))
        `shouldHaveDemoted` success

    it "adds constraints for invalid combinators" $ do
      check (Proxy :: Proxy (() :> Get' Int))
        `shouldHaveDemoted` AddConstraints ["IsCombinator * ()"]

    it "allows to add custom combinators" $ do
      check (Proxy :: Proxy (Custom :> Get' Int))
        `shouldHaveDemoted` AddConstraints ["IsCombinator * Custom"]

    it "allows to add custom combinators twice" $ do
      check (Proxy :: Proxy (Custom :> Custom :> Get' Int))
        `shouldHaveDemoted` AddConstraints ["IsCombinator * Custom", "IsCombinator * Custom"]

    it "allows to add custom combinators" $ do
      isValid (Proxy :: Proxy (Custom :> Get' Int))
        `shouldBe` ()
