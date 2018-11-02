{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module    : Nix.JenkinsPlugins2Nix
-- Copyright : (c) 2017 Mateusz Kowalczyk
-- License   : BSD3
--
-- Main library entry point.
module Nix.JenkinsPlugins2Nix where

import qualified Codec.Archive.Zip as Zip
import           Control.Arrow ((&&&))
import           Control.Monad (foldM)
import qualified Control.Monad.Except as MTL
import qualified Crypto.Hash as Hash
import qualified Data.ByteString.Lazy as BSL
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Network.HTTP.Simple as HTTP
import qualified Nix.Expr as Nix
import qualified Nix.JenkinsPlugins2Nix.Parser as Parser
import           Nix.JenkinsPlugins2Nix.Types
import qualified Nix.Pretty as Nix
import           System.IO (stderr)
import qualified Text.PrettyPrint.ANSI.Leijen as Pretty
import           Text.Printf (printf)

-- | Get the download URLs of the plugin we're looking for.
getPluginUrls :: RequestedPlugin -> [Text]
getPluginUrls (RequestedPlugin { requested_name = n, requested_version = Just v })
  = fmap Text.pack
      [ printf "https://updates.jenkins-ci.org/download/plugins/%s/%s/%s.hpi"
          (Text.unpack n) (Text.unpack v) (Text.unpack n)
      , printf "http://archives.jenkins-ci.org/plugins/%s/%s/%s.hpi"
          (Text.unpack n) (Text.unpack v) (Text.unpack n)
      ]
getPluginUrls (RequestedPlugin { requested_name = n, requested_version = Nothing })
  = fmap Text.pack
      [ printf "https://updates.jenkins-ci.org/latest/%s.hpi"
          (Text.unpack n)
      ]

-- | Given the Url, returns the manifest file, the Url and the hash of the original
-- file.
getManifestFileText :: Text -> IO (Maybe (Text, Text, Hash.Digest Hash.SHA256))
getManifestFileText fullUrl = do
  Text.hPutStrLn stderr $ "Downloading " <> fullUrl
  req <- HTTP.parseRequest $ Text.unpack fullUrl
  archiveLBS <- HTTP.getResponseBody <$> HTTP.httpLBS req
  let
    manifestFileText =
        fmap (Text.decodeUtf8 . BSL.toStrict . Zip.fromEntry)
      $ Zip.findEntryByPath "META-INF/MANIFEST.MF"
      $ Zip.toArchive archiveLBS
  pure $
    (\m -> (m, fullUrl, Hash.hashlazy archiveLBS)) <$> manifestFileText

useFirstWorking :: (a -> IO (Maybe b)) -> [a] -> IO (Maybe b)
useFirstWorking _ [] = do
  putStrLn "No options."
  pure Nothing
useFirstWorking f (x:xs) = do
  m <- f x
  case m of
    Just a ->
      pure $ Just a
    Nothing -> do
      putStrLn "Failed. Trying next option."
      useFirstWorking f xs

-- | Download a plugin from 'getPluginUrls'.
downloadPlugin :: RequestedPlugin -> IO (Either String Plugin)
downloadPlugin p = do
  let fullUrls = getPluginUrls p
  manifestFileText <- useFirstWorking getManifestFileText fullUrls
  case manifestFileText of
    Nothing -> return $ Left "Could not find manifest file in the archive."
    Just (t, url', sha256') -> return $! case Parser.runParseManifest t of
      Left err -> Left err
      Right manifest' -> Right $! Plugin
           -- We have to account for user not specifying the version.
           -- If they haven't, we downloaded the latest version and do
           -- not have a URL pointing at static resource. We do
           -- however have the version of the package now so we can
           -- reconstruct the URL.
        { download_url =
            case requested_version p of
              Nothing ->
                head $ getPluginUrls $
                  p { requested_version = Just $! plugin_version manifest' }
              Just _ -> url'
        , sha256 = sha256'
        , manifest = manifest'
        }

-- | Download the given plugin as well as recursively download its dependencies.
downloadPluginsRecursive
  :: ResolutionStrategy -- ^ Decide what version of dependencies to pick.
  -> Map Text RequestedPlugin -- ^ Plugins user requested.
  -> Map Text Plugin -- ^ Already downloaded plugins.
  -> RequestedPlugin -- ^ Plugin we're going to download.
  -> MTL.ExceptT String IO (Map Text Plugin)
downloadPluginsRecursive strategy uPs m p = if Map.member (requested_name p) m
  then return m
  else do
        -- Adjust the requested plugin based on whether it was
        -- specifically requested by the user and on resolution
        -- strategy.
    let adjustedPlugin = case Map.lookup (requested_name p) uPs of
          -- This is not a user-requested plugin which means we have
          -- to decide what version we're going to grab.
          Nothing -> case strategy of
            -- We're just going with whatever was in the manifest
            -- file, i.e. the thing we passed in in the first place.
            AsGiven -> p
            -- It's not a user-specified plugin and we want the latest
            -- version per strategy so download the latest one.
            Latest -> p { requested_version = Nothing }
          -- The user has asked for this plugin explicitly so use
          -- their possibly-versioned request rather than picking
          -- based on versions listed in manifest dependencies.
          Just userPlugin -> userPlugin
    plugin <- MTL.ExceptT $ downloadPlugin adjustedPlugin
    foldM (\m' p' -> downloadPluginsRecursive strategy uPs m' $
              RequestedPlugin { requested_name = plugin_dependency_name p'
                              , requested_version = Just $! plugin_dependency_version p'
                              })
      (Map.insert (requested_name p) plugin m)
      (plugin_dependencies $ manifest plugin)

-- | Pretty-print nix expression for all the given plugins and their
-- dependencies that the user asked for.
mkExprsFor :: Config
           -> IO (Either String Pretty.Doc)
mkExprsFor (Config { resolution_strategy = st, requested_plugins = ps }) = do
  eplugins <- MTL.runExceptT $ do
    let userPlugins = Map.fromList $ map (requested_name &&& id) ps
    plugins <- foldM (downloadPluginsRecursive st userPlugins) Map.empty ps
    return $ Map.elems plugins
  return $! case eplugins of
    Left err -> Left err
    Right plugins ->
      let args = Nix.mkParamset exprs False
          res = Nix.mkNonRecSet $ map formatPlugin plugins
          mkJenkinsPlugin = Nix.bindTo "mkJenkinsPlugin" $
            Nix.mkFunction (Nix.mkParamset
                              [ ("name", Nothing)
                              , ("src", Nothing)
                              ] False) $
              (Nix.@@) (Nix.mkSym "stdenv.mkDerivation") $ Nix.mkNonRecSet
                [ "name" Nix.$= Nix.mkSym "name"
                , "src" Nix.$= Nix.mkSym "src"
                , "phases" Nix.$= Nix.mkStr "installPhase"
                , "installPhase" Nix.$= Nix.mkStr "cp $src $out"
                ]
      in return $ Nix.prettyNix
                $ Nix.mkFunction args
                $ Nix.mkLets [mkJenkinsPlugin] res

  where
    fetchurl :: Plugin -> Nix.NExpr
    fetchurl p = (Nix.@@) (Nix.mkSym "fetchurl") $
      Nix.mkNonRecSet [ "url" Nix.$= Nix.mkStr (download_url p)
                      , "sha256" Nix.$= Nix.mkStr (Text.pack . show $ sha256 p)
                      ]

    mkBody :: Plugin -> Nix.NExpr
    mkBody p = (Nix.mkSym "mkJenkinsPlugin" Nix.@@) $
      Nix.mkNonRecSet $ [ "name" Nix.$= Nix.mkStr (short_name $ manifest p)
                        , "src" Nix.$= fetchurl p
                        ]

    formatPlugin :: Plugin -> Nix.Binding Nix.NExpr
    formatPlugin p = short_name (manifest p) Nix.$= mkBody p

    exprs :: [(Text, Maybe Nix.NExpr)]
    exprs =
      [ ("stdenv", Nothing)
      , ("fetchurl", Nothing)
      ]
