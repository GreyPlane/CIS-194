{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_toy (
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

bindir     = "C:\\Users\\70932\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\70932\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.4\\toy-0.1.0.0-JgwsgtMUfZrA6xy6K05cok-toy-exe"
dynlibdir  = "C:\\Users\\70932\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.4"
datadir    = "C:\\Users\\70932\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-8.6.4\\toy-0.1.0.0"
libexecdir = "C:\\Users\\70932\\AppData\\Roaming\\cabal\\toy-0.1.0.0-JgwsgtMUfZrA6xy6K05cok-toy-exe\\x86_64-windows-ghc-8.6.4\\toy-0.1.0.0"
sysconfdir = "C:\\Users\\70932\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "toy_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "toy_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "toy_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "toy_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "toy_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "toy_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
