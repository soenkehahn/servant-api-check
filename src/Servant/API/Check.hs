{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Servant.API.Check where

import           Data.Kind
import           Data.Proxy
import           GHC.TypeLits

import           Servant.API.Check.CheckResult
import           Servant.API.Check.Combinator
import           Servant.API.Check.Shape

-- | Constraint to check whether a servant api is well-formed. It is
-- mainly provided to give better error messages in case of
-- invalid apis.
--
-- Currently 'IsValid' checks
--
-- - the basic structure of the api, i.e. endpoints separated by
--   'Servant.API.:<|>' consisting of combinators chained by
--   'Servant.API.:>' and ending in 'Servant.API.Verb'.
-- - whether all used combinators are allowed.
--
-- If you want to allow custom combinators, see 'IsCombinator'.
--
-- Currently 'Servant.API.Vault' will be reported as an invalid combinator, since
-- providing an instance 'IsCombinator' 'Servant.API.Vault' triggers a ghc bug.
type IsValid api = RunCheckResult (CheckApi api)

type family CheckApi (api :: k) :: CheckResult ErrorMessage Constraint where
  CheckApi api =
    CheckShape api <>
    CheckCombinators api

-- | 'isValid' is a convenience function that allows to invoke
-- 'IsValid' (the constraint from above) for a given servant api.
--
-- 'isValid' is just one way to invoke the 'IsValid' constraint though.
-- You can of course stick that constraint to other functions or values.
isValid :: IsValid api => Proxy api -> ()
isValid Proxy = ()
