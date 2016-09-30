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

module Test.Utils where

import           Data.Kind
import           Data.Proxy
import           Data.Typeable
import           GHC.Stack
import           GHC.TypeLits
import           Servant.API
import           Test.Hspec

import           Servant.API.Check.Combinator
import           Servant.API.Check.CheckResult

type Get' = Get '[JSON]

type Post' = Post '[JSON]

-- * demotion from types to values

shouldHaveDemoted :: (?loc :: CallStack, Demote k a, Eq (Term k), Show (Term k)) =>
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

-- * lists

instance Demote [k] ('[] :: [k]) where
  type Term [k] = [Term k]
  demote Proxy = []

instance (Demote k a, Demote [k] r) =>
  Demote [k] ((a :: k) ': (r :: [k])) where

  type Term [k] = [Term k]
  demote Proxy = demote (Proxy :: Proxy a) : demote (Proxy :: Proxy r)

-- * ErrorMessage

instance KnownSymbol symbol => Demote ErrorMessage ('Text symbol) where
  type Term ErrorMessage = String
  demote Proxy = demote (Proxy :: Proxy symbol)

instance (Demote ErrorMessage a, Demote ErrorMessage b) => Demote ErrorMessage (a ':<>: b) where
  type Term ErrorMessage = String
  demote Proxy = demote (Proxy :: Proxy a) ++ demote (Proxy :: Proxy b)

instance Typeable t => Demote ErrorMessage ('ShowType t) where
  type Term ErrorMessage = String
  demote Proxy = show $ typeRep (Proxy :: Proxy t)

-- * CheckResult

instance Demote e err => Demote (CheckResult e a) ('Failure (err :: e)) where
  type Term (CheckResult e a) = CheckResult (Term e) (Term a)
  demote Proxy = Failure $ demote (Proxy :: Proxy err)

instance (Demote a r, Demote [a] rs) => Demote (CheckResult e a) ('AddConstraints ((r ': rs) :: [a])) where
  type Term (CheckResult e a) = CheckResult (Term e) (Term a)
  demote Proxy = AddConstraints $ demote (Proxy :: Proxy (r ': rs))

instance Demote (CheckResult e a) ('AddConstraints '[]) where
  type Term (CheckResult e a) = CheckResult (Term e) (Term a)
  demote Proxy = success

-- * Constraints

instance Typeable a => Demote Constraint (IsCombinator (a :: Type)) where
  type Term Constraint = String
  demote = show . typeRep

instance Typeable a => Demote Constraint (IsCombinator (a :: Symbol)) where
  type Term Constraint = String
  demote = show . typeRep

instance Demote Constraint (IsCombinator a, IsCombinator b) where
  type Term Constraint = String
  demote = error "void"
