{-
Modification of WriteNRT to work with Windows
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE NoIncoherentInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE NoUndecidableInstances #-}

module WriteNRTWin where
import qualified Vivid.SC.Server.Commands as SCCmd
import Vivid.Actions.NRT
import Vivid.Actions.Class
import Vivid.Actions.IO () -- maybe not in the future
import Vivid.OSC
import Vivid.OSC.Bundles (encodeOSCBundles)
import Vivid.SCServer
-- import Vivid.SCServer.State
import Vivid.SynthDef (encodeSD, sdToLiteral)
import Vivid.SynthDef.Types

import Control.Applicative
-- import Control.Arrow (first, second)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, modify, execStateT, StateT)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (writeFile)
import qualified Data.ByteString.UTF8 as UTF8
import Data.Char (toLower)
import Data.Hashable (hash)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import qualified Data.Set as Set
import System.Exit
import System.FilePath (takeExtension)
import System.Process (system)
import Prelude

writeNRTWin :: FilePath -> NRT a -> IO ()
writeNRTWin = writeNRTWith' defaultNRTArgs

writeNRTWith' ::  NRTArgs -> FilePath -> NRT a -> IO ()
writeNRTWith' nrtArgs fPath nrtActions = do
   when ('\'' `elem` fPath) $ error "Didnt have time to implement filepaths with single quotes"
   contents <- encodeOSCBundles <$> runNRT nrtActions

   --  ${SHELL}
   --system "scsynth" >>= \case
   --   ExitSuccess -> return ()
   --   ExitFailure _ -> error "No 'scsynth' found! Be sure to put it in your $PATH"
   let tempFile = "vivid_nrt_" <> (show . hash) contents <> ".osc"
       !fileType =
          case Map.lookup (map toLower $ takeExtension fPath) extensionMap of
             Just x -> x
             Nothing -> error $
                "The only file extensions we currently understand are: "
                ++ show (Map.keys extensionMap)
       extensionMap = Map.fromList [
            (".aif", "AIFF")
          , (".aiff", "AIFF")
          , (".wav", "WAV")
            -- todo: these formats seem not to work:
          -- ".flac" -> "FLAC"
          -- ".ogg" -> "vorbis"
          ]

   BS.writeFile tempFile contents
   ExitSuccess <- system $ mconcat [
        --  ${SHELL}
      --  "/bin/sh -c "
      --, " \"" -- Note these beginning and ending quotes
       "scsynth"
      , " -o ", show $ _nrtArgs_numChans nrtArgs
      , " -N "
      , tempFile
      , " _ ", fPath, " " -- single quotes removed for Windows version
      , show $ _nrtArgs_sampleRate nrtArgs," ", fileType, " int16 "
      --, " \""
      ]
   return ()