{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

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
      p `shouldDemoteTo` ("huhu" :: String)

    it "works for ()" $ do
      let  p = Proxy :: Proxy ('() :: ())
      p `shouldDemoteTo` ()

    context "CheckResult" $ do
      it "works for Left" $ do
        let  p = Proxy :: Proxy ('Failure '() :: CheckResult () ())
        p `shouldDemoteTo` Failure ()

      it "works for Right" $ do
        let  p = Proxy :: Proxy ('HoldsConstraints '[ '() ] :: CheckResult () ())
        p `shouldDemoteTo` HoldsConstraints [()]

      it "works for Success" $ do
        let  p = Proxy :: Proxy (Success :: CheckResult () ())
        p `shouldDemoteTo` success

    it "works for Text" $ do
      let  p = Proxy :: Proxy ('Text "foo")
      p `shouldDemoteTo` "foo"
