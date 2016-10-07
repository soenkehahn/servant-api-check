{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.API.Check.GetRequestBody where

import           Data.Kind
import           GHC.TypeLits
import           Servant.API

import           Servant.API.Check.CheckResult

type family CheckGetRequestBody (api :: k) :: CheckResult ErrorMessage Constraint where
  CheckGetRequestBody api = CheckGetRequestBodyH 'False api

type family CheckGetRequestBodyH (hasReqBody :: Bool) (api :: k) :: CheckResult ErrorMessage Constraint where
  CheckGetRequestBodyH hasReqBody (ReqBody contentTypes body :> api) =
    CheckGetRequestBodyH 'True api
  CheckGetRequestBodyH hasReqBody (combinator :> api) =
    CheckGetRequestBodyH hasReqBody api
  CheckGetRequestBodyH hasReqBody (a :<|> b) =
    CheckGetRequestBodyH 'False a <>
    CheckGetRequestBodyH 'False b
  CheckGetRequestBodyH 'True (Get contentTypes result) =
    'Failure ('Text "GET endpoints are not allowed to have request bodies")
  CheckGetRequestBodyH hasReqBody (Verb method status contentTypes result) =
    Success
  CheckGetRequestBodyH hasReqBody api =
    Success
