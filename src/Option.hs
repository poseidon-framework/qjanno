module Option where

import           Data.Version        (showVersion)
import           Options.Applicative

import qualified Paths_qjanno        as QJANNO

-- | Command options
data Option = Option {
    query                 :: Maybe String,
    queryFile             :: Maybe String,
    showColumns           :: Bool,
    tabDelimited          :: Bool,
    delimiter             :: Maybe String,
    noHeader              :: Bool,
    outputRaw             :: Bool,
    outputNoHeader        :: Bool
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
    <> help "Read query from the provided file."))
  <*> switch (
       long "showColumns"
    <> short 'c'
    <> help "Don't run the query, but show all available columns in the input files.")
  <*> switch (
       long "tabSep"
    <> short 't'
    <> help "Short for --sep $'\\t'.")
  <*> optional (strOption (
       long "sep"
    <> metavar "DELIM"
    <> help "Input file field delimiter. Will be automatically detected if it's not specified."))
  <*> switch (
       long "noHeader"
    <> help "Does the input file have no column names? They will be filled automatically with \
            \placeholders of the form c1,c2,c3,...")
  <*> switch (
       long "raw"
    <> help "Return the output table as tsv.")
  <*> switch (
       long "noOutHeader"
    <> help "Remove the header line from the output.")

-- | Parser for --version/-v
version :: Parser (a -> a)
version = abortOption (InfoMsg ("qjanno " ++ showVersion QJANNO.version)) $
    long "version" <>
    short 'v' <>
    help "Show the version of the command." <>
    hidden
