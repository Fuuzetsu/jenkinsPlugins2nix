{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module    : Main
-- Copyright : (c) 2017 Mateusz Kowalczyk
-- License   : BSD3
--
-- @jenkinsPlugins2nix@ entry point.
module Main (main) where

import qualified Data.Bimap                                as Bimap
import           Data.List                                 (intersperse)
import qualified Data.Text                                 as Text
import qualified Prettyprinter.Render.Terminal             as Pretty
import           Nix.JenkinsPlugins2Nix
import           Nix.JenkinsPlugins2Nix.Types
import qualified Options.Applicative                       as Opt
import           System.Exit
import           System.IO
import           Text.Printf                               (printf)

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
     <> Opt.metavar (printf "[%s]" . concat . intersperse "|" $ Bimap.keysR resolutions)
     <> Opt.value Latest )
  <*> Opt.switch
      ( Opt.long "no-deps"
     <> Opt.help "Do not download or include transitive dependencies; only generate nix for explicitly requested --plugin entries." )
  <*> Opt.some (Opt.option requestedPluginReader
                 ( Opt.metavar "PLUGIN_NAME{:PLUGIN_VERSION}"
                <> Opt.long "plugin"
                <> Opt.short 'p'
                <> Opt.help "Plugins we should generate nix for. Latest version is used if not specified." )
                )
  <*> Opt.flag Optional Mandatory
      ( Opt.long "skip-optional"
        <> Opt.help "skip optional dependencies" )
  where
    resolutions :: Bimap.Bimap ResolutionStrategy String
    resolutions = Bimap.fromList [(AsGiven, "as-given"), (Latest, "latest"), (JenkinsVersion(""), "jenkins")]

    resolutionReader :: Opt.ReadM ResolutionStrategy
    resolutionReader =
      let
        splitOnce :: String -> (String, Maybe String)
        splitOnce s =
          let (k, rest) = Text.breakOn ":" (Text.pack s)
          in if Text.null rest
             then (Text.unpack k, Nothing)
             else (Text.unpack k, Just (Text.unpack (Text.drop 1 rest)))

      in Opt.eitherReader $ \s ->
        let (k, mv) = splitOnce s
        in case Bimap.lookupR k resolutions of
             Nothing -> Left $
               "Invalid dependency resolution, needs to be one of "
               <> show (Bimap.keysR resolutions)
             Just (JenkinsVersion "") ->
               case mv of
                 Nothing -> Left "Invalid dependency resolution: jenkins strategy needs a version (jenkins:<VERSION>)"
                 Just "" -> Left "Invalid dependency resolution: jenkins strategy needs a version (jenkins:<VERSION>)"
                 Just v -> Right (JenkinsVersion v)
             Just v -> Right v

    requestedPluginReader :: Opt.ReadM RequestedPlugin
    requestedPluginReader = Opt.maybeReader $ \p -> Just $! case break (== ':') p of
      (n, ':' : ver) -> RequestedPlugin
        { requested_name = Text.pack n
        , requested_version = Just (Text.pack ver)
        }
      _ -> RequestedPlugin
        { requested_name = Text.pack p
        , requested_version = Nothing
        }
