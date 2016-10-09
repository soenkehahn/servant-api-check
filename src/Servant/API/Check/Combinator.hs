{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.API.Check.Combinator where

import           Data.Kind
import           GHC.TypeLits
import           Servant.API

import           Servant.API.Check.CheckResult

-- | Open (i.e. extensible) class for validating combinators.
--
-- If you have a custom combinator, just create an instance like this:
--
-- >>> import Data.Proxy
-- >>> import Servant.API.Check
--
-- >>> data CustomCombinator = CustomCombinator
-- >>> instance IsCombinator CustomCombinator
--
-- >>> type MyApi = CustomCombinator :> Get '[JSON] Int
-- >>> myApi = Proxy :: Proxy MyApi
-- >>> isValid myApi
-- ()
class IsCombinator (combinator :: k)

instance IsCombinator (path :: Symbol)

instance IsCombinator (Capture (name :: Symbol) (p :: Type))

instance IsCombinator (CaptureAll (name :: Symbol) (p :: Type))

instance IsCombinator (QueryParam (name :: Symbol) (p :: Type))

instance IsCombinator (QueryParams (name :: Symbol) (p :: Type))

instance IsCombinator (QueryFlag (name :: Symbol))

instance IsCombinator (ReqBody (contentTypes :: [*]) (p :: Type))

instance IsCombinator (Header (key :: Symbol) (value :: Type))

instance IsCombinator HttpVersion

instance IsCombinator IsSecure

instance IsCombinator RemoteHost

type family CheckCombinators (api :: Type) :: CheckResult ErrorMessage Constraint where
  CheckCombinators ((combinator :: k) :> api) =
    'HoldsConstraints '[IsCombinator combinator] <>
    CheckCombinators api

  CheckCombinators (a :<|> b) =
    CheckCombinators a <> CheckCombinators b

  CheckCombinators (Verb method status contentTypes result) =
    Success

  CheckCombinators Raw =
    Success

  CheckCombinators (WithNamedContext name subContext api) =
    CheckCombinators api
