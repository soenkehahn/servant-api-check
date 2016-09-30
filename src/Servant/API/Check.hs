{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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

type IsValid api = (RunEither (CheckApi api) :: Constraint)

data ValidApiWitness api where
  ValidApiWitness :: IsValid api => ValidApiWitness api

validateRequestBodies :: Proxy api -> Proxy (CheckApi api)
validateRequestBodies Proxy = Proxy

type family CheckApi (api :: *) :: Either ErrorMessage () where
  CheckApi api = CheckApiH NoReqBody api

data HasReqBody api
  = YesReqBody (api :: *)
  | NoReqBody

type family CheckApiH
  (hasReqBody :: HasReqBody endpoint)
  (api :: *)
    :: Either ErrorMessage () where

  CheckApiH
    (YesReqBody reqBodyEndpoint)
    (Verb (GET :: StdMethod) (status :: Nat) (contentTypes :: [*]) (result :: *)) =
      Left (
        'Text "GET endpoints shouldn't have request bodies. (" :<>:
        ShowType reqBodyEndpoint :<>:
        'Text ")")

  CheckApiH
    hasReqBody
    (Verb (method :: StdMethod) (status :: Nat) (contentTypes :: [*]) (result :: *)) =
      Right '()

  CheckApiH hasReqBody (combinator :> api) =
    IsCombinator combinator >>
    CheckApiH hasReqBody api

  CheckApiH hasReqBody api =
    Left ('Text "invalid api type: " ':<>: 'ShowType api)

type family IsCombinator (combinator :: *) :: Either ErrorMessage () where
  IsCombinator (QueryParam (name :: Symbol) (p :: *))
    = Right '()
  IsCombinator (ReqBody (contentTypes :: [*]) (body :: *))
    = Right '()
  IsCombinator combinator = IsCombinatorOpen combinator
  {- IsCombinator combinator
    = Left ('Text "invalid combinator: " ':<>: 'ShowType combinator) -}

type family IsCombinatorOpen (combinator :: *) :: Either ErrorMessage ()

type instance IsCombinatorOpen api =
  Left ('Text "invalid combinator: " ':<>: 'ShowType api)

type family (>>=) (a :: Either e k) (b :: k -> Either e ()) :: Either e () where
  Left err >>= next = Left err
  Right result >>= next = next result

type family (>>) (a :: Either e ()) (b :: Either e ()) :: Either e () where
  Left err >> next = Left err
  Right '() >> next = next

type family RunEither (action :: Either ErrorMessage ()) :: Constraint where
  RunEither ('Right '()) = (() :: Constraint)
  RunEither ('Left error) = TypeError error
