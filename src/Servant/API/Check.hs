{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.API.Check where

import           Data.Kind
import           Data.Proxy
import           GHC.TypeLits
import           Servant.API

type IsValid api = RunEither (ValidateRequestBodies api)

isValid :: (IsValid api) => Proxy api -> ()
isValid Proxy = ()

validateRequestBodies :: Proxy api -> Proxy (ValidateRequestBodies api)
validateRequestBodies Proxy = Proxy

type family ValidateRequestBodies (api :: *) :: Either ErrorMessage () where
  ValidateRequestBodies api = ValidateRequestBodiesH NoReqBody api

data HasReqBody api
  = YesReqBody (api :: *)
  | NoReqBody

type family ValidateRequestBodiesH
  (hasReqBody :: HasReqBody endpoint)
  (api :: *)
    :: Either ErrorMessage () where

  ValidateRequestBodiesH
    (YesReqBody reqBodyEndpoint)
    (Verb (GET :: StdMethod) (status :: Nat) (contentTypes :: [*]) (result :: *)) =
      Left (
        'Text "GET endpoints shouldn't have request bodies. (" :<>:
        ShowType reqBodyEndpoint :<>:
        'Text ")")

  ValidateRequestBodiesH
    hasReqBody
    (Verb (method :: StdMethod) (status :: Nat) (contentTypes :: [*]) (result :: *)) =
      Right '()

  ValidateRequestBodiesH
    hasReqBody
    (QueryParam (name :: Symbol) (p :: *) :> api)
      = ValidateRequestBodiesH hasReqBody api
  ValidateRequestBodiesH hasReqBody (ReqBody (contentTypes :: [*]) (body :: *) :> api) =
    ValidateRequestBodiesH (YesReqBody (ReqBody contentTypes body :> api)) api

  ValidateRequestBodiesH hasReqBody api =
    Left ('Text "invalid api type: " ':<>: 'ShowType api)

type family RunEither (action :: Either ErrorMessage ()) :: Constraint where
  RunEither ('Right '()) = (() :: Constraint)
  RunEither ('Left error) = TypeError error
