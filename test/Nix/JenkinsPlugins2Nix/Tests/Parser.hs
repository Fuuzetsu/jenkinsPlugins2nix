{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module    : Nix.JenkinsPlugins2Nix.Types
-- Copyright : (c) 2017 Mateusz Kowalczyk
-- License   : BSD3
--
-- Parser tests.
module Nix.JenkinsPlugins2Nix.Tests.Parser (spec) where

import qualified Data.Set as Set
import qualified Data.Text as Text
import           Nix.JenkinsPlugins2Nix.Parser (runParseManifest)
import           Nix.JenkinsPlugins2Nix.Types
import qualified Test.Tasty.Hspec as Hspec

spec :: Hspec.Spec
spec = Hspec.describe "parser spec" $ do
  Hspec.it "parser github-pullrequest manifest" githubPr

githubPr :: Hspec.Expectation
githubPr = Right expected `Hspec.shouldBe` runParseManifest githubPrManifestText
  where
    pluginDependencies :: Set.Set PluginDependency
    pluginDependencies = Set.fromList
      [ PluginDependency { plugin_dependency_resolution = Mandatory
                         , plugin_dependency_name = "github"
                         , plugin_dependency_version  = "1.22.1" }
      , PluginDependency { plugin_dependency_resolution = Mandatory
                         , plugin_dependency_name = "workflow-step-api"
                         , plugin_dependency_version = "1.14" }
      , PluginDependency { plugin_dependency_resolution = Optional
                         , plugin_dependency_name = "block-queued-job"
                         , plugin_dependency_version = "0.2.0" }
      , PluginDependency { plugin_dependency_resolution = Optional
                         , plugin_dependency_name = "email-ext"
                         , plugin_dependency_version = "2.38.2" }
      , PluginDependency { plugin_dependency_resolution = Mandatory
                         , plugin_dependency_name = "github-api"
                         , plugin_dependency_version = "1.80" }
      , PluginDependency { plugin_dependency_resolution = Optional
                         , plugin_dependency_name = "job-dsl"
                         , plugin_dependency_version = "1.38" }
      , PluginDependency { plugin_dependency_resolution = Optional
                         , plugin_dependency_name = "jucies"
                         , plugin_dependency_version = "0.2.1" }
      , PluginDependency { plugin_dependency_resolution = Mandatory
                         , plugin_dependency_name = "matrix-project"
                         , plugin_dependency_version = "1.6" }
      , PluginDependency { plugin_dependency_resolution = Optional
                         , plugin_dependency_name = "token-macro"
                         , plugin_dependency_version = "1.10" }
      ]

    expected :: Manifest
    expected = Manifest
      { manifest_version = "1.0"
      , archiver_version = Just "Plexus Archiver"
      , created_by = Just "Apache Maven"
      , built_by = Just "integer"
      , build_jdk = Just "1.8.0_112"
      , extension_name = Just "github-pullrequest"
      , specification_title = Just "GitHub Integration Plugin for Jenkins"
      , implementation_title = Just "github-pullrequest"
      , implementation_version = Just "0.1.0-rc24"
      , group_id = Just "org.jenkins-ci.plugins"
      , short_name = "github-pullrequest"
      , long_name = "GitHub Integration Plugin"
      , url = Just "https://wiki.jenkins-ci.org/display/JENKINS/GitHub+Integration+Plugin"
      , plugin_version = "0.1.0-rc24"
      , hudson_version = Just "1.625.3"
      , jenkins_version = Just "1.625.3"
      , plugin_dependencies = pluginDependencies
      , plugin_developers = Set.fromList ["Kanstantsin Shautsou:KostyaSha:"]
      }

githubPrManifestText :: Text.Text
githubPrManifestText = Text.intercalate "\r\n"
  [ "Manifest-Version: 1.0"
  , "Archiver-Version: Plexus Archiver"
  , "Created-By: Apache Maven"
  , "Built-By: integer"
  , "Build-Jdk: 1.8.0_112"
  , "Extension-Name: github-pullrequest"
  , "Specification-Title: GitHub Integration Plugin for Jenkins"
  , "Implementation-Title: github-pullrequest"
  , "Implementation-Version: 0.1.0-rc24"
  , "Group-Id: org.jenkins-ci.plugins"
  , "Short-Name: github-pullrequest"
  , "Long-Name: GitHub Integration Plugin"
  , "Url: https://wiki.jenkins-ci.org/display/JENKINS/GitHub+Integration+Pl"
  , " ugin"
  , "Plugin-Version: 0.1.0-rc24"
  , "Hudson-Version: 1.625.3"
  , "Jenkins-Version: 1.625.3"
  , "Plugin-Dependencies: github:1.22.1,workflow-step-api:1.14,block-queued"
  , " -job:0.2.0;resolution:=optional,email-ext:2.38.2;resolution:=optional"
  , " ,github-api:1.80,job-dsl:1.38;resolution:=optional,jucies:0.2.1;resol"
  , " ution:=optional,matrix-project:1.6,token-macro:1.10;resolution:=optio"
  , " nal"
  , "Plugin-Developers: Kanstantsin Shautsou:KostyaSha:"
  ]
