{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module    : Nix.JenkinsPlugins2Nix.Types
-- Copyright : (c) 2017 Mateusz Kowalczyk
-- License   : BSD3
--
-- @jenkinsPlugins2nix@ entry point.
module Main (main) where

import           Data.Monoid ((<>))
import qualified Data.Text as Text
import           Nix.JenkinsPlugins2Nix
import           Nix.JenkinsPlugins2Nix.Types
import           System.Environment
import           System.Exit
import           System.IO
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty

usage :: Handle -> IO ()
usage h = do
  pName <- getProgName
  hPutStrLn h $
    pName <> ": PLUGIN_NAME{:PLUGIN_VERSION} [PLUGIN_NAME{:PLUGIN_VERSION} ...]"

main :: IO ()
main = do
  getArgs >>= \case
    [] -> do
      usage stderr
      exitFailure
    [v] | v == "-h" || v == "--help" -> do
      usage stdout
      exitSuccess
    plugins -> do
      mkExprsFor (map parseRequestedPlugin plugins) >>= \case
        Left err -> do
          hPutStrLn stderr err
          exitFailure
        Right p -> do
          Pretty.putDoc p
          exitSuccess
  where
    parseRequestedPlugin :: String -> RequestedPlugin
    parseRequestedPlugin p = case break (== ':') p of
      (n, ':' : ver) -> RequestedPlugin
        { requested_name = Text.pack n
        , requested_version = Just (Text.pack ver)
        }
      _ -> RequestedPlugin
        { requested_name = Text.pack p
        , requested_version = Nothing
        }
