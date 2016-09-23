module AslBuild.Memcached where

import           Development.Shake
import           Development.Shake.FilePath

import           AslBuild.Constants
import           AslBuild.OptParse
import           AslBuild.Utils

cleanMemcachedRule :: String
cleanMemcachedRule = "cleanmemcached"

memcachedRules :: AslBuilder ()
memcachedRules = do
    c <- ask
    lift $ do
        case c of
            BuildAll _ -> want [memaslapBin, memcachedBin]
            BuildClean -> want [cleanMemcachedRule]
            _ -> return ()

        memaslapBinRules
        memcachedBinRules

        phony cleanMemcachedRule $ do
            removeFilesAfter outDir ["//"]
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

configure :: String
configure = "configure"

libmemcachedArchiveConfigureFile :: FilePath
libmemcachedArchiveConfigureFile = libmemcachedArchiveFullDir </> configure

libmemcachedUrl :: String
libmemcachedUrl = "https://launchpad.net/libmemcached/1.0"
    </> libmemcachedVersion
    </> "+download"
    </> libmemcachedArchiveFile

makefile :: String
makefile = "Makefile"

makeFileInMemaslapDir :: FilePath
makeFileInMemaslapDir = libmemcachedArchiveFullDir </> makefile

clients :: String
clients = "clients"

memaslapMakeTarget :: FilePath
memaslapMakeTarget = clients </> memaslapBinName

memaslapBinInCacheDir :: FilePath
memaslapBinInCacheDir = libmemcachedArchiveFullDir </> clients </> memaslapBinName

memaslapBinRules :: Rules ()
memaslapBinRules = do
    libmemcachedArchiveFullFile %> \_ ->
        cmd "wget" libmemcachedUrl
            "--output-document" libmemcachedArchiveFullFile

    libmemcachedArchiveConfigureFile %> \_ -> do
        need [libmemcachedArchiveFullFile]
        cmd "tar"
            "--extract"
            "--verbose"
            "--file" libmemcachedArchiveFullFile
            "--directory" tmpDir

    makeFileInMemaslapDir %> \_ -> do
        need [libmemcachedArchiveConfigureFile]
        cmd (Cwd libmemcachedArchiveFullDir)
            (AddEnv "LDFLAGS" "-lpthread")
            "./configure" "--enable-memaslap"

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
            "./configure" "--enable-memaslap"

    memcachedBinInCacheDir %> \_ -> do
        need [makeFileInMemcachedDir]
        cmd (Cwd memcachedArchiveFullDir)
            "make" "--jobs"

    memcachedBin `byCopying` memcachedBinInCacheDir
