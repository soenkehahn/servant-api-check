{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.UtilsSpec where

import           Data.Proxy
import           GHC.TypeLits
import           Test.Hspec

import           Servant.API.Check.CheckResult
import           Test.Utils

spec :: Spec
spec = do
  describe "demoting" $ do
    it "works for Symbol" $ do
      let  p = Proxy :: Proxy "huhu"
      p `shouldHaveDemoted` ("huhu" :: String)

    it "works for ()" $ do
      let  p = Proxy :: Proxy ('() :: ())
      p `shouldHaveDemoted` ()

    context "CheckResult" $ do
      it "works for Left" $ do
        let  p = Proxy :: Proxy ('Failure '() :: CheckResult () ())
        p `shouldHaveDemoted` Failure ()

      it "works for Right" $ do
        let  p = Proxy :: Proxy ('AddConstraints '[ '() ] :: CheckResult () ())
        p `shouldHaveDemoted` AddConstraints [()]

      it "works for Success" $ do
        let  p = Proxy :: Proxy (Success :: CheckResult () ())
        p `shouldHaveDemoted` success

    it "works for Text" $ do
      let  p = Proxy :: Proxy ('Text "foo")
      p `shouldHaveDemoted` "foo"
