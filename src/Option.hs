module Option where

import           Data.Version        (showVersion)
import           Options.Applicative

import qualified Paths_qjanno        as QJANNO

-- | Command options
data Option = Option {
    query                 :: Maybe String,
    queryFile             :: Maybe String,
    tabDelimited          :: Bool,
    delimiter             :: Maybe String,
    noHeader              :: Bool,
    keepLeadingWhiteSpace :: Bool,
    tabDelimitedOutput    :: Bool,
    outputDelimiter       :: Maybe String,
    outputHeader          :: Bool
  }

-- | Option parser
options :: Parser Option
options = Option
  <$> optional (argument str (
       metavar "QUERY"
    <> help "MYSQL syntax query with paths to files for table names. \
            \See the online documentation for examples. \
            \The special table name syntax 'd(path1,path2,...)'' treats the paths (path1, path2, ...) \
            \as base directories where .janno files are searched recursively. \
            \All detected .janno files are merged into one table and can thus be \
            \subjected to arbitrary queries."))
  <*> optional (strOption (
       long "queryFile"
    <> short 'q'
    <> metavar "FILE"
    <> help "Read query from the provided filename."))
  <*> switch (
       long "tabSep"
    <> short 't'
    <> help "Short for --sep $'\\t'.")
  <*> optional (strOption (
       long "sep"
    <> metavar "DELIM"
    <> help "Field delimiter. Will be automatically detected if it's not specified."))
  <*> switch (
       long "noHeader"
    <> help "Does the file have no column names?")
  <*> switch (
       long "keepWS"
    <> help "Keep leading whitespace in values. The leading whitespaces are stripped off by default.")
  <*> switch (
       long "tabSepOut"
    <> short 'T'
    <> help "Short for --outSep $'\\t'.")
  <*> optional (strOption (
       long "outSep"
    <> metavar "DELIM"
    <> help "Field delimiter for the output. If not specified, the argument of --sep is used."))
  <*> switch (
       long "outHeader"
    <> help "Add the header line to the output.")

-- | Parser for --version/-v
version :: Parser (a -> a)
version = abortOption (InfoMsg ("qjanno " ++ showVersion QJANNO.version)) $
    long "version" <>
    short 'v' <>
    help "Show the version of the command." <>
    hidden
