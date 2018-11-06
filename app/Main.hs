{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module    : Main
-- Copyright : (c) 2017 Mateusz Kowalczyk
-- License   : BSD3
--
-- @jenkinsPlugins2nix@ entry point.
module Main (main) where

import qualified Data.Bimap as Bimap
import           Data.List (intersperse)
import           Data.Monoid ((<>), mconcat)
import           Nix.JenkinsPlugins2Nix
import           Nix.JenkinsPlugins2Nix.Types
import qualified Options.Applicative as Opt
import           System.Exit
import           System.IO
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty
import           Text.Printf (printf)

main :: IO ()
main = do
  config <- Opt.execParser opts
  mkExprsFor config >>= \case
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right p -> do
      Pretty.putDoc p
      exitSuccess
  where
    opts = Opt.info (parseConfig Opt.<**> Opt.helper)
           ( Opt.fullDesc
          <> Opt.progDesc "Generate nix expressions for requested Jenkins plugins." )


parseConfig :: Opt.Parser Config
parseConfig = Config
  <$> Opt.option resolutionReader
      ( Opt.long "dependency-resolution"
     <> Opt.short 'r'
     <> Opt.help "Dependency resolution"
     <> Opt.showDefaultWith (resolutions Bimap.!)
     <> Opt.metavar (printf "[%s]" . mconcat . intersperse "|" $ Bimap.keysR resolutions)
     <> Opt.value Latest )
  <*> (Opt.many ( Opt.option requestedPluginReader
                ( Opt.metavar "PLUGIN_NAME{:PLUGIN_VERSION}"
               <> Opt.long "plugin"
               <> Opt.short 'p'
               <> Opt.help "Plugins we should generate nix for. Latest version is used if not specified." )
               ) )
  <*> Opt.option (Opt.maybeReader $ \x -> Just $ if x == "" then Nothing else Just x)
     ( Opt.long "file"
    <> Opt.short 'f'
    <> Opt.help "A file containing a list of plugins in the same format as -p, one plugin per line."
    <> Opt.value Nothing
    )

  where
    resolutions :: Bimap.Bimap ResolutionStrategy String
    resolutions = Bimap.fromList [(AsGiven, "as-given"), (Latest, "latest")]

    resolutionReader :: Opt.ReadM ResolutionStrategy
    resolutionReader = Opt.eitherReader $ \s -> case Bimap.lookupR s resolutions of
      Nothing -> Left $ "Invalid dependency resolution, needs to be one of "
                     <> show (Bimap.keysR resolutions)
      Just v -> Right v

    requestedPluginReader :: Opt.ReadM RequestedPlugin
    requestedPluginReader = Opt.maybeReader $ parseRequestedPlugin


