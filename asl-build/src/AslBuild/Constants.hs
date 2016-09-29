module AslBuild.Constants where

import           Development.Shake.FilePath

-- Constants
asl :: String
asl = "asl"

bin :: String
bin = "bin"

out :: String
out = "out"

dist :: String
dist = "dist"

src :: String
src = "src"

txt :: String
txt = "txt"

tmp :: String
tmp = "tmp"

lib :: String
lib = "lib"

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
outDir :: FilePath
outDir = out

tmpDir :: FilePath
tmpDir = tmp

resultsDir :: FilePath
resultsDir = "results"

reportsDir :: FilePath
reportsDir = reports

codeSrcDir :: FilePath
codeSrcDir = asl

scriptsDir :: FilePath
scriptsDir = "scripts"

javaSourceDir :: FilePath
javaSourceDir = codeSrcDir </> src

-- Extensions
type Extension = FilePath

pdfExt :: Extension
pdfExt = pdf

texExt :: Extension
texExt = tex

txtExt :: Extension
txtExt = txt

jarExt :: Extension
jarExt = jar

pngExt :: Extension
pngExt = png

javaExt :: Extension
javaExt = java

xmlExt :: Extension
xmlExt = xml

propertiesExt :: Extension
propertiesExt = properties

tarGzExt :: Extension
tarGzExt = "tar.gz"

-- Rules
cleanTarget :: String
cleanTarget = "clean"


-- Commands
curlCmd :: String
curlCmd = "curl"

wgetCmd :: String
wgetCmd = "wget"

tarCmd :: String
tarCmd = "tar"

makeCmd :: String
makeCmd = "make"

gitCmd :: String
gitCmd = "git"

stackCmd :: String
stackCmd = "stack"

javaCmd :: String
javaCmd = "java"

-- Hosts
localhost :: String
localhost = "localhost"

-- Memcached
defaultMemcachedPort :: Int
defaultMemcachedPort = 11211
