module AslBuild.Constants where

import           Development.Shake.FilePath

-- Constants
asl :: String
asl = "asl"

out :: String
out = "out"

dist :: String
dist = "dist"

src :: String
src = "src"

txt :: String
txt = "txt"

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

xmlExt :: Extension
xmlExt = xml

propertiesExt :: Extension
propertiesExt = properties

-- Rules
cleanTarget :: String
cleanTarget = "clean"


