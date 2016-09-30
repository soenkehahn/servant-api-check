{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

import           Data.String.Interpolate
import           Data.String.Interpolate.Util
import           System.Directory
import           System.Exit
import           System.Process
import           Test.Hspec
import           Test.Mockery.Directory

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Combinator" $ do
    it "complains about invalid combinators" $ do
      "isValid (Proxy :: Proxy (() :> Get' Int))"
        `shouldNotTypecheckWith` "No instance for (IsCombinator ())"

    it "complains about invalid combinators" $ do
      "isValid (Proxy :: Proxy (QueryParam \"foo\" :> Get' Int))"
        `shouldNotTypecheckWith` "No instance for (IsCombinator (QueryParam \"foo\"))"

    it "complains about invalid combinators after valid ones" $ do
      "isValid (Proxy :: Proxy (QueryParam \"foo\" Int :> QueryParam \"bar\" :> Get' Int))"
        `shouldNotTypecheckWith` "No instance for (IsCombinator (QueryParam \"bar\"))"

    it "complains about invalid combinators before valid ones" $ do
      "isValid (Proxy :: Proxy (QueryParam \"foo\" :> QueryParam \"bar\" Int :> Get' Int))"
        `shouldNotTypecheckWith` "No instance for (IsCombinator (QueryParam \"foo\"))"

  describe "Shape" $ do
    it "complains about apis that are not built with :<|> and :>" $ do
      "isValid (Proxy :: Proxy ())"
        `shouldNotTypecheckWith` "invalid servant api: ()"

shouldNotTypecheckWith :: String -> String -> IO ()
shouldNotTypecheckWith expression expected = do
  repoDir <- getCurrentDirectory
  inTempDirectory $ do
    let code = unindent [i|
        {-# LANGUAGE DataKinds #-}
        {-# LANGUAGE TypeOperators #-}

        module Foo where

        import Data.Proxy
        import Servant.API

        import Servant.API.Check
        import Servant.API.Check.Combinator
        import Servant.API.Check.Shape

        type Get' = Get '[JSON]

        x = #{expression}
      |]
    callCommand $ "cp -r " ++ repoDir ++ "/src/* ."
    writeFile "code.hs" code
    (exitCode, _stdout, stderr) <- readProcessWithExitCode "ghc" ["--make", "code.hs"] ""
    exitCode `shouldBe` ExitFailure 1
    stderr `shouldContain` expected
