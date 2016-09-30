{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.API.Check.CheckResult where

import           Data.Kind
import           GHC.TypeLits

-- | Result type for api checks. Mostly used as a datakind.
-- Forms a Monoid with '<>' and 'Success'.
data CheckResult error constraint
  = Failure error
  | AddConstraints [constraint]
  deriving (Eq, Show)

type Success = 'AddConstraints '[]

success :: CheckResult e constraint
success = AddConstraints []

-- | Type family to execute 'CheckResult's.
--
-- - In the case of 'AddConstraints' the contained constraints will be returned.
--   This allows to actually require that the constraints be fullfilled.
-- - In the case of 'Failure' it'll make the compiler throw the contained error
--   message as a type error.
type family RunCheckM (action :: CheckResult ErrorMessage Constraint) :: Constraint where
  RunCheckM ('Failure err) = TypeError err
  RunCheckM ('AddConstraints constraints) = RunConstraints constraints

type family (<>) (a :: CheckResult error constraint) (b :: CheckResult error constraint) :: CheckResult error constraint where
  'Failure e <> 'Failure f = 'Failure e
  'Failure e <> 'AddConstraints cs = 'Failure e
  'AddConstraints cs <> 'Failure e = 'Failure e
  'AddConstraints as <> 'AddConstraints bs = 'AddConstraints ((as ++ bs) :: [Constraint])

-- * Type Utils

type family (++) (a :: [k]) (b :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)

type family RunConstraints (constraints :: [Constraint]) :: Constraint where
  RunConstraints (c ': cs) = (c, RunConstraints cs)
  RunConstraints '[] = ()
