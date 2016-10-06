{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.API.Check.Shape where

import           Data.Kind
import           GHC.TypeLits
import           Servant.API

import           Servant.API.Check.CheckResult

type family CheckShape (api :: k) :: CheckResult ErrorMessage Constraint where
  CheckShape ((a :: l) :> b) = CheckShape b
  CheckShape (a :<|> b) = CheckShape a <> CheckShape b
  CheckShape (Verb (method :: StdMethod) (status :: Nat) (contentTypes :: [Type]) (result :: Type)) =
    Success
  CheckShape (WithNamedContext (name :: Symbol) (subContext :: [*]) (api :: *)) =
    CheckShape api
  CheckShape Raw =
    Success
  CheckShape api =
    'Failure ('Text "invalid servant api: " ':<>: 'ShowType api)
