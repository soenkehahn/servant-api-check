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

type Get' = Get '[JSON]

data Custom

type instance ComputeHasReqBody Custom hasReqBody = hasReqBody

spec :: Spec
spec = do
  describe "validateRequestBodies" $ do
    it "validates Verb correctly" $ do
      validateRequestBodies (Proxy :: Proxy (Get' Int))
        `shouldHaveDemoted`
        Right ()

    it "complains about GET endpoints with request body" $ do
      validateRequestBodies (Proxy :: Proxy (ReqBody '[JSON] Int :> Get' Int))
        `shouldHaveDemoted`
        Left "GET endpoints shouldn't have request bodies."

    it "works for valid apis" $ do
      validateRequestBodies (Proxy :: Proxy (QueryParam "foo" Int :> Get' Int))
        `shouldHaveDemoted` Right ()

    it "reports invalid apis" $ do
      validateRequestBodies (Proxy :: Proxy ())
        `shouldHaveDemoted` Left "invalid api type: ()"

    it "allows to make custom combinators valid" $ do
      validateRequestBodies (Proxy :: Proxy (Custom :> Get' Int))
        `shouldHaveDemoted` Right ()

{-
    it "reports invalid combinators" $ do
      validateRequestBodies (Proxy :: Proxy (() :> Get' Int))
        `shouldHaveDemoted` Left "invalid combinator: ()"
        -}

    it "" $ do
      let () = isValid (Proxy :: Proxy (QueryParam "foo" Int :> Get '[JSON] Int))
      return () :: IO ()

    it "" $ do
      pending
      -- print $ validate (Proxy :: Proxy (Get '[JSON] Int))

    it "" $ do
      let  p = Proxy :: Proxy "huhu"
      demote p `shouldBe` ("huhu" :: String)
      let  p = Proxy :: Proxy ('() :: ())
      demote p `shouldBe` ()
      let  p = Proxy :: Proxy ('Right '() :: Either () ())
      demote p `shouldBe` Right ()
      let  p = Proxy :: Proxy ('Left '() :: Either () ())
      demote p `shouldBe` Left ()
      let  p = Proxy :: Proxy ('Text "foo")
      demote p `shouldBe` "foo"

shouldHaveType :: (?loc :: CallStack, Typeable a) => Proxy (a :: k) -> String -> IO ()
shouldHaveType p expected =
  show (typeRep p) `shouldBe` expected

shouldHaveDemoted :: (?loc :: CallStack , Demote k a, Eq (Term k), Show (Term k)) =>
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
