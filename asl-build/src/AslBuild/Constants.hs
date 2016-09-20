module AslBuild.Constants where

import           Development.Shake.FilePath

-- Constants
asl :: String
asl = "asl"

out :: String
out = "out"

src :: String
src = "src"

txt :: String
txt = "txt"

reports :: String
reports = "reports"

pdf :: String
pdf = "pdf"

tex :: String
tex = "tex"

jar :: String
jar = "jar"

java :: String
java = "java"

gradle :: String
gradle = "gradle"

build :: String
build = "build"

libs :: String
libs = "libs"

settings :: String
settings = "settings"

-- Directories
outDir :: FilePath
outDir = out

reportsDir :: FilePath
reportsDir = reports

codeSrcDir :: FilePath
codeSrcDir = asl

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

javaExt :: Extension
javaExt = java

gradleExt :: Extension
gradleExt = gradle

