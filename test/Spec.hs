module Main where

import qualified Nix.JenkinsPlugins2Nix.Tests.Parser as Parser
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec $ do
  Parser.spec
