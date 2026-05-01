{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module    : Nix.JenkinsPlugins2Nix.Parser
-- Copyright : (c) 2017 Mateusz Kowalczyk
-- License   : BSD3
--
-- Parsers.
module Nix.JenkinsPlugins2Nix.Parser
  ( parseManifest
  , runParseManifest
  ) where

import           Control.Applicative
import           Control.Monad (void)
import qualified Data.Attoparsec.Text as A
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import           Nix.JenkinsPlugins2Nix.Types

-- | Run parser on manifest file contents.
runParseManifest :: Text -> Either String Manifest
runParseManifest = A.parseOnly parseManifest

-- | Manifest parser.
parseManifest :: A.Parser Manifest
parseManifest = do
  kvs <- kvMap
  let getKey :: Text -> Either String Text
      getKey k = case Map.lookup k kvs of
        Nothing -> Left $ "Could not find " <> Text.unpack k <> " in " <> show kvs
        Just v -> Right v

      getOptionalKey :: Text -> Either String (Maybe Text)
      getOptionalKey k = Right (Map.lookup k kvs)

      getKeyParsing :: Text -> A.Parser a -> Either String a
      getKeyParsing k p = getKey k >>= A.parseOnly p

      eManifest :: Either String Manifest
      eManifest = do
        manifest_version' <- getKey "Manifest-Version"
        archiver_version' <- getOptionalKey "Archiver-Version"
        created_by' <- getOptionalKey "Created-By"
        built_by' <- getOptionalKey "Built-By"
        build_jdk' <- getOptionalKey "Build-Jdk"
        extension_name' <- getOptionalKey "Extension-Name"
        specification_title' <- getOptionalKey "Specification-Title"
        implementation_title' <- getOptionalKey "Implementation-Title"
        implementation_version' <- getOptionalKey "Implementation-Version"
        group_id' <- getOptionalKey "Group-Id"
        short_name' <- getKey "Short-Name"
        long_name' <- getKey "Long-Name"
        url' <- getOptionalKey "Url"
        plugin_version' <- getKey "Plugin-Version"
        hudson_version' <- getOptionalKey "Hudson-Version"
        jenkins_version' <- getOptionalKey "Jenkins-Version"
        plugin_dependencies' <- either (\_ -> Right Set.empty) return $
          getKeyParsing "Plugin-Dependencies" parsePluginDependencies
        plugin_developers' <- either (\_ -> Right Set.empty) return $
          getKeyParsing "Plugin-Developers" parsePluginDevelopers
        return $! Manifest
          { manifest_version = manifest_version'
          , archiver_version = archiver_version'
          , created_by = created_by'
          , built_by = built_by'
          , build_jdk = build_jdk'
          , extension_name = extension_name'
          , specification_title = specification_title'
          , implementation_title = implementation_title'
          , implementation_version = implementation_version'
          , group_id = group_id'
          , short_name = short_name'
          , long_name = long_name'
          , url = url'
          , plugin_version = plugin_version'
          , hudson_version = hudson_version'
          , jenkins_version = jenkins_version'
          , plugin_dependencies = plugin_dependencies'
          , plugin_developers = plugin_developers'
          }
  case eManifest of
    Left err -> fail err
    Right m -> return m
  where
    parsePluginDevelopers :: A.Parser (Set Text)
    parsePluginDevelopers = Set.fromList . Text.splitOn "," <$> A.takeText

    parsePluginDependencies :: A.Parser (Set PluginDependency)
    parsePluginDependencies =
      let plugin = do
            name <- A.takeWhile1 (/= ':') <* A.char ':'
            version <- A.takeWhile1 (\c -> c /= ',' && c /= ';')
            resolution <- A.peekChar >>= \case
              -- end of input
              Nothing -> return Mandatory
              -- next dependency
              Just ',' -> A.char ',' *> return Mandatory
              -- specifier
              Just ';' -> do
                _ <- A.string ";resolution:="
                A.takeWhile1 (/= ',') >>= \case
                  "optional" -> return Optional
                  res' -> fail $ "Don't know how to parse resolution: " <> Text.unpack res'
              Just c -> fail $ "plugin: expected , or ; but got: " <> [c]

            -- Consume trailing comma if any
            void (A.char ',') <|> return ()
            return $! PluginDependency
              { plugin_dependency_name = name
              , plugin_dependency_version = version
              , plugin_dependency_resolution = resolution
              }
      in Set.fromList <$> many plugin

    -- Lines in manifest can span multiple file lines as long as they
    -- are indented with a space on next physical line.
    kvEntry :: A.Parser (Text, Text)
    kvEntry = do
      key <- A.takeWhile1 (/= ':')
      A.anyChar *> A.skipSpace
      let restOfLine = A.takeTill A.isEndOfLine <* (A.endOfLine <|> return ())
          indentedLine = A.peekChar' >>= \case
            ' ' -> A.skipSpace *> restOfLine
            _ -> fail "indentedLine doesn't start with a space"

      valueLines <- (:) <$> restOfLine <*> many indentedLine
      return $! (key, Text.concat valueLines)

    kvMap :: A.Parser (Map Text Text)
    kvMap = Map.fromList <$> many kvEntry
