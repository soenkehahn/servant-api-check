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

module Servant.API.CheckSpec where

import           Data.Kind
import           Data.Proxy
import           Data.Typeable
import           GHC.Stack
import           GHC.TypeLits
import           Servant.API
import           Test.Hspec

import           Servant.API.Check

spec :: Spec
spec = do
  describe "validateRequestBodies" $ do
    it "complains about GET endpoints with request body" $ do
      validateRequestBodies (Proxy :: Proxy (ReqBody '[JSON] Int :> Get '[JSON] Int))
        `shouldHaveDemoted`
        Left "GET endpoints shouldn't have request bodies. (:> * * (ReqBody * (': * JSON '[]) Int) (Verb StdMethod * 'GET 200 (': * JSON '[]) Int))"

    it "works for valid apis" $ do
      validateRequestBodies (Proxy :: Proxy (QueryParam "foo" Int :> Get '[JSON] Int))
        `shouldHaveDemoted` Right ()

    it "reports invalid apis" $ do
      validateRequestBodies (Proxy :: Proxy ())
        `shouldHaveDemoted` Left "invalid api type: ()"

    it "" $ do
      let () = isValid (Proxy :: Proxy (QueryParam "foo" Int :> Get '[JSON] Int))
      return () :: IO ()

    it "" $ do
      pending
      -- print $ validate (Proxy :: Proxy (Get '[JSON] Int))

    it "" $ do
      let  p = Proxy :: Proxy "huhu"
      lower p `shouldBe` ("huhu" :: String)
      let  p = Proxy :: Proxy ('() :: ())
      lower p `shouldBe` ()
      let  p = Proxy :: Proxy ('Right '() :: Either () ())
      lower p `shouldBe` Right ()
      let  p = Proxy :: Proxy ('Left '() :: Either () ())
      lower p `shouldBe` Left ()
      let  p = Proxy :: Proxy ('Text "foo")
      lower p `shouldBe` "foo"

shouldHaveType :: (?loc :: CallStack, Typeable a) => Proxy (a :: k) -> String -> IO ()
shouldHaveType p expected =
  show (typeRep p) `shouldBe` expected

shouldHaveDemoted :: (Demote k a, Eq (Term k), Show (Term k)) =>
  Proxy (a :: k) -> Term k -> IO ()
shouldHaveDemoted proxy expected =
  lower proxy `shouldBe` expected

class Demote (k :: Type) (t :: k) where
  type Term k :: Type
  lower :: Proxy (t :: k) -> Term k

instance KnownSymbol t => Demote Symbol t where
  type Term Symbol = String
  lower proxy = symbolVal proxy

instance Demote () '() where
  type Term () = ()
  lower Proxy = ()

instance Demote k a => Demote (Either k l) (Left (a :: k)) where
  type Term (Either k l) = Either (Term k) (Term l)
  lower Proxy = Left $ lower (Proxy :: Proxy a)

instance Demote l a => Demote (Either k l) (Right (a :: l)) where
  type Term (Either k l) = Either (Term k) (Term l)
  lower Proxy = Right $ lower (Proxy :: Proxy a)

instance KnownSymbol symbol => Demote ErrorMessage (Text symbol) where
  type Term ErrorMessage = String
  lower Proxy = lower (Proxy :: Proxy symbol)

instance (Demote ErrorMessage a, Demote ErrorMessage b) => Demote ErrorMessage (a :<>: b) where
  type Term ErrorMessage = String
  lower Proxy = lower (Proxy :: Proxy a) ++ lower (Proxy :: Proxy b)

instance Typeable t => Demote ErrorMessage (ShowType t) where
  type Term ErrorMessage = String
  lower Proxy = show $ typeRep (Proxy :: Proxy t)
