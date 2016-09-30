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

data Custom

type instance IsCombinatorOpen Custom = Right '()

type Get' = Get '[JSON]

spec :: Spec
spec = do
  describe "validateRequestBodies" $ do
    it "works for valid apis" $ do
      validateRequestBodies (Proxy :: Proxy (Get' Int))
        `shouldHaveDemoted` Right ()

    it "" $ do
      validateRequestBodies (Proxy :: Proxy (() :> Get' Int))
        `shouldHaveDemoted` Left "invalid combinator: ()"

    it "allows to add custom combinators" $ do
      validateRequestBodies (Proxy :: Proxy (Custom :> Get' Int))
        `shouldHaveDemoted` Right ()

    it "complains about GET endpoints with request body" $ do
      pending
      validateRequestBodies (Proxy :: Proxy (ReqBody '[JSON] Int :> Get' Int))
        `shouldHaveDemoted`
        Left "GET endpoints shouldn't have request bodies. (:> * * (ReqBody * (': * JSON '[]) Int) (Verb StdMethod * 'GET 200 (': * JSON '[]) Int))"

    it "reports invalid apis" $ do
      validateRequestBodies (Proxy :: Proxy ())
        `shouldHaveDemoted` Left "invalid api type: ()"

    it "" $ do
      let validProxy :: ValidApiWitness (QueryParam "foo" Int :> Get' Int)
          validProxy = ValidApiWitness
      True

    it "" $ do
      pending
      -- print $ validate (Proxy :: Proxy (Get' Int))

  describe "demoting (fixme)" $ do
    it "" $ do
      let  p = Proxy :: Proxy "huhu"
      p `shouldHaveDemoted` ("huhu" :: String)
      let  p = Proxy :: Proxy ('() :: ())
      p `shouldHaveDemoted` ()
      let  p = Proxy :: Proxy ('Right '() :: Either () ())
      p `shouldHaveDemoted` Right ()
      let  p = Proxy :: Proxy ('Left '() :: Either () ())
      p `shouldHaveDemoted` Left ()
      let  p = Proxy :: Proxy ('Text "foo")
      p `shouldHaveDemoted` "foo"

shouldHaveType :: (?loc :: CallStack, Typeable a) => Proxy (a :: k) -> String -> IO ()
shouldHaveType p expected =
  show (typeRep p) `shouldBe` expected

shouldHaveDemoted :: (?loc :: CallStack, Typeable a, Demote k a, Eq (Term k), Show (Term k)) =>
  Proxy (a :: k) -> Term k -> IO ()
shouldHaveDemoted proxy expected =
  demote proxy `shouldBe` expected

class Demote (k :: Type) (t :: k) where
  type Term k :: Type
  demote :: Proxy (t :: k) -> Term k

instance KnownSymbol t => Demote Symbol t where
  type Term Symbol = String
  demote proxy = symbolVal proxy

instance Demote () '() where
  type Term () = ()
  demote Proxy = ()

instance Demote k a => Demote (Either k l) (Left (a :: k)) where
  type Term (Either k l) = Either (Term k) (Term l)
  demote Proxy = Left $ demote (Proxy :: Proxy a)

instance Demote l a => Demote (Either k l) (Right (a :: l)) where
  type Term (Either k l) = Either (Term k) (Term l)
  demote Proxy = Right $ demote (Proxy :: Proxy a)

instance KnownSymbol symbol => Demote ErrorMessage (Text symbol) where
  type Term ErrorMessage = String
  demote Proxy = demote (Proxy :: Proxy symbol)

instance (Demote ErrorMessage a, Demote ErrorMessage b) => Demote ErrorMessage (a :<>: b) where
  type Term ErrorMessage = String
  demote Proxy = demote (Proxy :: Proxy a) ++ demote (Proxy :: Proxy b)

instance Typeable t => Demote ErrorMessage (ShowType t) where
  type Term ErrorMessage = String
  demote Proxy = show $ typeRep (Proxy :: Proxy t)
