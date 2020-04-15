{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_diagrams_playground (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/sbothwell/.cabal/bin"
libdir     = "/Users/sbothwell/.cabal/lib/x86_64-osx-ghc-8.6.5/diagrams-playground-0.1.0.0-inplace-diagrams-playground"
dynlibdir  = "/Users/sbothwell/.cabal/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/sbothwell/.cabal/share/x86_64-osx-ghc-8.6.5/diagrams-playground-0.1.0.0"
libexecdir = "/Users/sbothwell/.cabal/libexec/x86_64-osx-ghc-8.6.5/diagrams-playground-0.1.0.0"
sysconfdir = "/Users/sbothwell/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "diagrams_playground_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "diagrams_playground_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "diagrams_playground_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "diagrams_playground_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "diagrams_playground_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "diagrams_playground_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
