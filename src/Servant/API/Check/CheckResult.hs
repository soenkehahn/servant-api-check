{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Servant.API.Check.CheckResult where

import           Data.Kind
import           GHC.TypeLits

-- | Result type for api checks. Mostly used as a datakind.
-- Forms a Monoid with '<>' and 'Success'.
data CheckResult error constraint
  = Failure error
  | HoldsConstraints [constraint]
  deriving (Eq, Show)

type Success = 'HoldsConstraints '[]

success :: CheckResult e constraint
success = HoldsConstraints []

-- | Type family to execute 'CheckResult's.
--
-- - In the case of 'HoldsConstraints' the contained constraints will be returned.
--   This allows to actually require that the constraints be fullfilled.
-- - In the case of 'Failure' it'll make the compiler throw the contained error
--   message as a type error.
type family RunCheckResult (action :: CheckResult ErrorMessage Constraint) :: Constraint where
  RunCheckResult ('Failure err) = TypeError err
  RunCheckResult ('HoldsConstraints constraints) = RunConstraints constraints

type family (<>) (a :: CheckResult error constraint) (b :: CheckResult error constraint) :: CheckResult error constraint where
  'Failure e <> next = 'Failure e
  'HoldsConstraints cs <> 'Failure e = 'Failure e
  'HoldsConstraints as <> 'HoldsConstraints bs =
    'HoldsConstraints ((as ++ bs) :: [Constraint])

-- * Type Utils

type family (++) (a :: [k]) (b :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

type family RunConstraints (constraints :: [Constraint]) :: Constraint where
  RunConstraints (c ': cs) = (c, RunConstraints cs)
  RunConstraints '[] = ()
