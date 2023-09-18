module Main where

import           Control.Monad                     (forM, forM_, unless, when)
import           Data.Char                         (isSpace)
import           Data.List                         (intercalate, transpose)
import qualified Data.Map                          as Map
import           Data.Maybe
import           Data.Set                          ((\\))
import qualified Data.Set                          as Set
import           Data.Version                      (showVersion)
import qualified Database.SQLite.Simple            as SQLite
import qualified Options.Applicative               as OP
import           System.Exit                       (exitFailure)
import           System.IO
import           Text.Layout.Table                 (asciiRoundS, column, def,
                                                    expandUntil, rowsG,
                                                    tableString, titlesH)
import           Text.Layout.Table.Spec.HeaderSpec (HeaderSpec (..))
import           Text.Read                         (readMaybe)

import           Paths_qjanno_hs                   (version)
import qualified Qjanno.File                       as File
import qualified Qjanno.Janno                      as Janno
import qualified Qjanno.Option                     as Option
import qualified Qjanno.Parser                     as Parser
import qualified Qjanno.SQL                        as SQL
import qualified Qjanno.SQLType                    as SQLType
import           System.Directory                  (doesFileExist)

main :: IO ()
main = do
    cmdOpts <- OP.customExecParser (OP.prefs OP.showHelpOnEmpty) optParserInfo
    runCommand cmdOpts

optParserInfo :: OP.ParserInfo Option.Option
optParserInfo = OP.info (OP.helper <*> versionOption <*> Option.options) (
    OP.briefDesc <>
    OP.progDesc "Command line tool to allow SQL queries on .janno (and arbitrary .csv and .tsv) files."
    )

versionOption :: OP.Parser (a -> a)
versionOption = OP.infoOption (showVersion version) (OP.long "version" <> OP.help "Show qjanno version")

runCommand :: Option.Option -> IO ()
runCommand opts = do
  queryTableMap <- parseQuery =<< fetchQuery opts
  conn <- SQL.open ":memory:"
  runQuery opts conn queryTableMap
  SQL.close conn

runQuery :: Option.Option -> SQLite.Connection -> (String, Parser.TableNameMap) -> IO ()
runQuery opts conn (query, tableMap) = do
  columnOverview <- readFilesCreateTables opts conn tableMap
  if Option.showColumns opts
  then do
      -- just show columns
      let columnOverviewRowwise = concatMap (\(p, cs) -> map (compileColTableRow p) cs) columnOverview
      printTable ["Column", "Path", "qjanno Table name"] columnOverviewRowwise
  else do
      -- run regular query
      ret <- SQL.execute conn query
      case ret of
          Right (colnames, rows) -> do
              printTable colnames (map (map show) rows)
          Left err -> do
              hPutStrLn stderr err
              exitFailure
  where
      compileColTableRow :: String -> String -> [String]
      compileColTableRow p c = [c, p, fromMaybe "" $ Map.lookup p tableMap]
      printTable :: [String] -> [[String]] -> IO ()
      printTable tableH tableB = do
          if Option.outputRaw opts
          then do
              unless (Option.outputNoHeader opts) $ putStrLn $ intercalate "\t" $ tableH
              putStrLn $ intercalate "\n" [intercalate "\t" row | row <- tableB]
          else do
              let colSpecs = replicate (length tableH) (column (expandUntil 60) def def def)
              if Option.outputNoHeader opts
              then do
                  putStrLn $ tableString colSpecs asciiRoundS NoneHS [rowsG tableB]
              else do
                  putStrLn $ tableString colSpecs asciiRoundS (titlesH tableH) [rowsG tableB]

fetchQuery :: Option.Option -> IO String
fetchQuery opts = do
  when (isJust (Option.query opts) && isJust (Option.queryFile opts)) $ do
    hPutStrLn stderr "Can't provide both a query file and a query on the command line."
    exitFailure
  query <- fromMaybe "" <$>
    case Option.query opts of
        Just q -> return (Just q)
        Nothing -> mapM readFile (Option.queryFile opts)
  when (all isSpace query) $ do
    hPutStrLn stderr "Query cannot be empty."
    hPutStrLn stderr "See, qjanno -h for help."
    exitFailure
  return query

parseQuery :: String -> IO (String, Parser.TableNameMap)
parseQuery qs = do
  let (query, tableMap) = Parser.replaceTableNames qs
  case Parser.extractTableNames query "<<query>>" of
       Left err -> do
         hPutStrLn stderr $ Parser.replaceBackTableNames tableMap $ Parser.errorString err
         exitFailure
       Right tableNames -> do
         let xs = Set.fromList tableNames
         let ys = Set.fromList (Map.elems tableMap)
         if xs == ys
            then return (query, tableMap)
            else do
              hPutStrLn stderr "Invalid table name:"
              hPutStrLn stderr $ "  " ++ show (Set.union (xs \\ ys) (ys \\ xs))
              hPutStrLn stderr "Probably a bug of qjanno. Please submit a issue report."
              exitFailure

readFilesCreateTables :: Option.Option -> SQLite.Connection -> Parser.TableNameMap -> IO [(String, [String])]
readFilesCreateTables opts conn tableMap = do
  forM (Map.toList tableMap) $ \(path, name) -> do
    let path' = unquote path
    case Parser.readFROM path' of
        Left s -> do
            hPutStrLn stderr "Invalid FROM string: "
            hPutStrLn stderr s
            exitFailure
        Right (Parser.Jannos j) -> do
            allJannoPaths <- concat <$> mapM Janno.findJannoPaths j
            when (null allJannoPaths) $ do
                hPutStrLn stderr "No .janno files found."
                exitFailure
            forM_ allJannoPaths $ \p -> do
                fileExists <- doesFileExist p
                unless fileExists $ do
                    hPutStrLn stderr $ "File expected, but does not exist: " ++ p
                    exitFailure
            let jannoOpts = opts {Option.tabDelimited = True}
            allJannoHandles <- mapM (`openFile` ReadMode) allJannoPaths
            allJannos <- mapM (File.readFromFile jannoOpts) allJannoHandles
            let (columns, body) = Janno.mergeJannos allJannos
            createTable conn name path columns body
            -- returns all columns for the --showColumns feature
            return (path, columns)
        Right Parser.StdIn -> do
            makeDBFromNormalFile name "/dev/stdin"
        Right (Parser.AnyFile _) -> do
            fileExists <- doesFileExist path'
            unless fileExists $ do
                hPutStrLn stderr $ "File does not exist: " ++ path'
                exitFailure
            makeDBFromNormalFile name path'
  where 
    unquote (x:xs@(_:_)) | x `elem` "\"'`" && x == last xs = init xs
    unquote xs = xs
    makeDBFromNormalFile :: String -> FilePath -> IO (String, [String])
    makeDBFromNormalFile name path = do
        handle <- openFile path ReadMode
        (columns, body) <- File.readFromFile opts handle
        when (length columns == 0) $ do
            if Option.noHeader opts
            then hPutStrLn stderr "Warning - data is empty."
            else hPutStrLn stderr $ "Header line is expected but missing in file " ++ path
            exitFailure
        when (any (elem ',') columns) $ do
            hPutStrLn stderr "Column name cannot contain commas."
            exitFailure
        when (length columns >= 1) $
            createTable conn name path columns body
        hClose handle
        return (path, columns)



createTable :: SQLite.Connection -> String -> String -> [String] -> [[String]] -> IO ()
createTable conn name path columns bodyRaw = do
  let body = makeNAEmptyString bodyRaw
  let probablyNumberColumn =
        [ all isJust [ readMaybe x :: Maybe Float | x <- xs, not (all isSpace x) ]
                                                  | xs <- transpose body ]
  let types = [ if b then SQLType.SQLInt else SQLType.SQLChar | b <- probablyNumberColumn ]
  ret <- SQL.createTable conn name columns types
  case ret of
       Just err -> do
         putStrLn "Error on creating a new table:"
         putStrLn $ "  " ++ path ++ " (" ++ name ++ ") " ++ show columns
         putStrLn err
       Nothing ->
         forM_ body $ \entry -> do
           mapM_ (hPutStrLn stderr) =<< SQL.insertRow conn name columns types entry

makeNAEmptyString :: [[String]] -> [[String]]
makeNAEmptyString = map (map trans)
    where
        trans :: String -> String
        trans "n/a" = ""
        trans x     = x
