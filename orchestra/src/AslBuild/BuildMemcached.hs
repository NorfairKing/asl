module AslBuild.BuildMemcached where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.Utils

memcachedRule :: String
memcachedRule = "memcached"

cleanMemcachedRule :: String
cleanMemcachedRule = "cleanmemcached"

buildMemcachedRules :: Rules ()
buildMemcachedRules = do
    memcachedBinRules
    memaslapBinRules

    memcachedRule ~> need [memcachedBin, memaslapBin]

    cleanMemcachedRule ~> do
        removeFilesAfter outDir [memcachedBin, memaslapBin]
        removeFilesAfter tmpDir ["//"]

memaslapBinName :: String
memaslapBinName = "memaslap"

memaslapBin :: FilePath
memaslapBin = outDir </> memaslapBinName

libmemcachedVersion :: String
libmemcachedVersion = "1.0.18"

libmemcachedArchiveFileName :: FilePath
libmemcachedArchiveFileName = "libmemcached-" ++ libmemcachedVersion

libmemcachedArchiveFile :: FilePath
libmemcachedArchiveFile = libmemcachedArchiveFileName <.> "tar.gz"

libmemcachedArchiveFullFile :: FilePath
libmemcachedArchiveFullFile = tmpDir </> libmemcachedArchiveFile

libmemcachedArchiveFullDir :: FilePath
libmemcachedArchiveFullDir = tmpDir </> libmemcachedArchiveFileName

libmemcachedArchiveConfigureFile :: FilePath
libmemcachedArchiveConfigureFile = libmemcachedArchiveFullDir </> configure

libmemcachedUrl :: String
libmemcachedUrl = "https://launchpad.net/libmemcached/1.0"
    </> libmemcachedVersion
    </> "+download"
    </> libmemcachedArchiveFile

makeFileInMemaslapDir :: FilePath
makeFileInMemaslapDir = libmemcachedArchiveFullDir </> makefile

memaslapBinInCacheDir :: FilePath
memaslapBinInCacheDir = libmemcachedArchiveFullDir </> "clients" </> memaslapBinName

memaslapBinRules :: Rules ()
memaslapBinRules = do
    libmemcachedArchiveFullFile %> \_ ->
        cmd "wget" libmemcachedUrl
            "--output-document" libmemcachedArchiveFullFile

    libmemcachedArchiveConfigureFile %> \_ -> do
        need [libmemcachedArchiveFullFile]
        cmd tarCmd
            "--extract"
            "--verbose"
            "--file" libmemcachedArchiveFullFile
            "--directory" tmpDir

    makeFileInMemaslapDir %> \_ -> do
        need [libmemcachedArchiveConfigureFile]
        cmd (Cwd libmemcachedArchiveFullDir)
            (AddEnv "LDFLAGS" "-lpthread")
            ("." </> configure) "--enable-memaslap"

    memaslapBinInCacheDir %> \_ -> do
        need [makeFileInMemaslapDir]
        cmd (Cwd libmemcachedArchiveFullDir)
            "make" "--jobs"

    memaslapBin `byCopying` memaslapBinInCacheDir

memcachedBinName :: FilePath
memcachedBinName = "memcached"

memcachedBin :: FilePath
memcachedBin = outDir </> memcachedBinName

memcachedVersion :: String
memcachedVersion = "1.4.31"

memcachedArchiveFileName :: FilePath
memcachedArchiveFileName = "memcached-" ++ memcachedVersion

memcachedArchiveFile :: FilePath
memcachedArchiveFile = memcachedArchiveFileName <.> "tar.gz"

memcachedArchiveFullFile :: FilePath
memcachedArchiveFullFile = tmpDir </> memcachedArchiveFile

memcachedArchiveFullDir :: FilePath
memcachedArchiveFullDir = tmpDir </> memcachedArchiveFileName

memcachedArchiveConfigureFile :: FilePath
memcachedArchiveConfigureFile = memcachedArchiveFullDir </> configure

memcachedUrl :: String
memcachedUrl = "http://memcached.org/files" </> memcachedArchiveFile

makeFileInMemcachedDir :: FilePath
makeFileInMemcachedDir = memcachedArchiveFullDir </> makefile

memcachedMakeTarget :: FilePath
memcachedMakeTarget = memcachedBinName

memcachedBinInCacheDir :: FilePath
memcachedBinInCacheDir = memcachedArchiveFullDir </> memcachedBinName

memcachedBinRules :: Rules ()
memcachedBinRules = do
    memcachedArchiveFullFile %> \_ ->
        cmd "wget" memcachedUrl
            "--output-document" memcachedArchiveFullFile

    memcachedArchiveConfigureFile %> \_ -> do
        need [memcachedArchiveFullFile]
        cmd "tar"
            "--extract"
            "--verbose"
            "--file" memcachedArchiveFullFile
            "--directory" tmpDir

    makeFileInMemcachedDir %> \_ -> do
        need [memcachedArchiveConfigureFile]
        cmd (Cwd memcachedArchiveFullDir)
            ("." </> configure) "--enable-memaslap"

    memcachedBinInCacheDir %> \_ -> do
        need [makeFileInMemcachedDir]
        cmd (Cwd memcachedArchiveFullDir)
            "make" "--jobs"

    memcachedBin `byCopying` memcachedBinInCacheDir
