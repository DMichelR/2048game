{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_gloss_poc (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/santi/Desktop/2048game/.stack-work/install/x86_64-linux-tinfo6/9e674c18893230eded940111b5dec61f54a70d18002fce18a78533c8395fb304/9.6.4/bin"
libdir     = "/home/santi/Desktop/2048game/.stack-work/install/x86_64-linux-tinfo6/9e674c18893230eded940111b5dec61f54a70d18002fce18a78533c8395fb304/9.6.4/lib/x86_64-linux-ghc-9.6.4/gloss-poc-0.1.0.0-CcDCSF5NxOHAIGzk9xfC3w-gloss-poc-exe"
dynlibdir  = "/home/santi/Desktop/2048game/.stack-work/install/x86_64-linux-tinfo6/9e674c18893230eded940111b5dec61f54a70d18002fce18a78533c8395fb304/9.6.4/lib/x86_64-linux-ghc-9.6.4"
datadir    = "/home/santi/Desktop/2048game/.stack-work/install/x86_64-linux-tinfo6/9e674c18893230eded940111b5dec61f54a70d18002fce18a78533c8395fb304/9.6.4/share/x86_64-linux-ghc-9.6.4/gloss-poc-0.1.0.0"
libexecdir = "/home/santi/Desktop/2048game/.stack-work/install/x86_64-linux-tinfo6/9e674c18893230eded940111b5dec61f54a70d18002fce18a78533c8395fb304/9.6.4/libexec/x86_64-linux-ghc-9.6.4/gloss-poc-0.1.0.0"
sysconfdir = "/home/santi/Desktop/2048game/.stack-work/install/x86_64-linux-tinfo6/9e674c18893230eded940111b5dec61f54a70d18002fce18a78533c8395fb304/9.6.4/etc"

getBinDir     = catchIO (getEnv "gloss_poc_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "gloss_poc_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "gloss_poc_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "gloss_poc_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "gloss_poc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "gloss_poc_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'