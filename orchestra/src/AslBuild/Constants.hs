module AslBuild.Constants where

import           Development.Shake.FilePath

import           System.Directory
import           System.IO.Unsafe

-- Username
myNetzh :: String
myNetzh = "tomk"

-- Constants
asl :: String
asl = "asl"

bin :: String
bin = "bin"

out :: String
out = "out"

dist :: String
dist = "dist"

test :: String
test = "test"

src :: String
src = "src"

txt :: String
txt = "txt"

tmp :: String
tmp = "tmp"

eps :: String
eps = "eps"

lib :: String
lib = "lib"

dot :: String
dot = "dot"

svg :: String
svg = "svg"

png :: String
png = "png"

reports :: String
reports = "reports"

properties :: String
properties = "properties"

pdf :: String
pdf = "pdf"

tex :: String
tex = "tex"

bib :: String
bib = "bib"

csv :: String
csv = "csv"

xml :: String
xml = "xml"

jar :: String
jar = "jar"

java :: String
java = "java"

ant :: String
ant = "ant"

build :: String
build = "build"

libs :: String
libs = "libs"

settings :: String
settings = "settings"

configure :: String
configure = "configure"

makefile :: String
makefile = "Makefile"

-- Directories
aslDir :: FilePath
aslDir = ""

{-# NOINLINE aslCacheDir #-}
aslCacheDir :: FilePath
aslCacheDir = unsafePerformIO $ do
    home <- getHomeDirectory
    return $ home </> ".asl"

outDir :: FilePath
outDir = aslCacheDir </> out

tmpDir :: FilePath
tmpDir = aslCacheDir </> tmp

remoteTmpDir :: FilePath
remoteTmpDir = "/tmp/asl" </> tmp

assignmentDir :: FilePath
assignmentDir = aslDir </> "assignment"

resultsDir :: FilePath
resultsDir = aslDir </> "results"

analysisDir :: FilePath
analysisDir = aslDir </> "analysis"

analysisPlotsDir :: FilePath
analysisPlotsDir = tmpDir </> "plots"

analysisTmpDir :: FilePath
analysisTmpDir = tmpDir </> "analysis"

reportsDir :: FilePath
reportsDir = aslDir </> reports

codeSrcDir :: FilePath
codeSrcDir = aslDir </> asl

scriptsDir :: FilePath
scriptsDir = aslDir </> "scripts"

javaSourceDir :: FilePath
javaSourceDir = codeSrcDir </> src

javaTestDir :: FilePath
javaTestDir = codeSrcDir </> test

-- Extensions
type Extension = FilePath

pdfExt :: Extension
pdfExt = pdf

texExt :: Extension
texExt = tex

bibExt :: Extension
bibExt = bib

txtExt :: Extension
txtExt = txt

jarExt :: Extension
jarExt = jar

dotExt :: Extension
dotExt = dot

epsExt :: Extension
epsExt = eps

pngExt :: Extension
pngExt = png

javaExt :: Extension
javaExt = java

xmlExt :: Extension
xmlExt = xml

csvExt :: Extension
csvExt = csv

svgExt :: Extension
svgExt = svg

propertiesExt :: Extension
propertiesExt = properties

tarGzExt :: Extension
tarGzExt = "tar.gz"

jsonExt :: Extension
jsonExt = "json"

-- Rules
cleanTarget :: String
cleanTarget = "clean"


-- Commands
antCmd :: String
antCmd = "ant"

jarCmd :: String
jarCmd = "jar"

curlCmd :: String
curlCmd = "curl"

wgetCmd :: String
wgetCmd = "wget"

tarCmd :: String
tarCmd = "tar"

dotCmd :: String
dotCmd = "dot"

makeCmd :: String
makeCmd = "make"

gitCmd :: String
gitCmd = "git"

stackCmd :: String
stackCmd = "stack"

javaCmd :: String
javaCmd = "java"

sedCmd :: String
sedCmd = "sed"

azureCmd :: String
azureCmd = "azure"

convertCmd :: String
convertCmd = "convert"

-- Memcached
defaultMemcachedPort :: Int
defaultMemcachedPort = 11211

-- Azure

resourceGroupName :: String
resourceGroupName = "myResourceGroup"

resourceGroupLocation :: String
resourceGroupLocation = "westeurope"

localhostIp :: String
localhostIp = "127.0.0.1"

-- Remote caching
remoteMemaslap :: FilePath
remoteMemaslap = "/tmp/memaslap"

remoteMemcached :: FilePath
remoteMemcached = "/tmp/memcached"

remoteMiddleware :: FilePath
remoteMiddleware = "/tmp/asl.jar"
