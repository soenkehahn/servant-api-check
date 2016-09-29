{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.API.Check where

import           Data.Kind
import           Data.Proxy
import           GHC.TypeLits
import           Servant.API

isValid :: (IsValid api) => Proxy api -> ()
isValid Proxy = ()

class IsValid api where

validateRequestBodies :: (ValidateRequestBodiesH NoReqBody api result) =>
  Proxy api -> Proxy result
validateRequestBodies Proxy = Proxy

{-
type family ValidateRequestBodies (api :: Type) :: Either ErrorMessage () where
  ValidateRequestBodies api = ValidateRequestBodiesH NoReqBody api
  -}

data HasReqBody
  = YesReqBody
  | NoReqBody

class ValidateRequestBodiesH
  (hasReqBody :: HasReqBody)
  (api :: Type)
  (isValid :: Either ErrorMessage ())
  | hasReqBody api -> isValid

{-
instance ValidateRequestBodiesH
  YesReqBody
  (Verb (GET :: StdMethod) (status :: Nat) (contentTypes :: [Type]) (result :: Type))
  (Left ('Text "GET endpoints shouldn't have request bodies."))
-}

instance ValidateRequestBodiesH
  hasReqBody
  (Verb (method :: StdMethod) (status :: Nat) (contentTypes :: [Type]) (result :: Type))
  (Right '())

instance (ValidateRequestBodiesH hasReqBody api isValid) =>
  ValidateRequestBodiesH
    hasReqBody
    (combinator :> api)
    isValid

instance ValidateRequestBodiesH
  hasReqBody
  ()
  (Left ('Text "invalid api type: " ':<>: 'ShowType ()))

type family ComputeHasReqBody combinator (hasReqBody :: HasReqBody) :: HasReqBody

type instance ComputeHasReqBody (QueryParam name p) hasReqBody = hasReqBody

type instance ComputeHasReqBody (ReqBody contentTypes body) hasReqBody = YesReqBody

type family RunEither (action :: Either ErrorMessage ()) :: Constraint where
  RunEither ('Right '()) = (() :: Constraint)
  RunEither ('Left error) = TypeError error
