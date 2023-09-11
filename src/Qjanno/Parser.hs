module Qjanno.Parser (
    replaceTableNames,
    roughlyExtractTableNames,
    replaceBackTableNames,
    extractTableNames,
    errorString,
    TableNameMap,
    readFROM
) where

import           Data.Char                      (isAlphaNum, isNumber, isSpace,
                                                 toUpper)
import           Data.Generics                  (everything, mkQ)
import           Data.List                      (unfoldr)
import qualified Data.Map                       as Map
import           Data.Tuple                     (swap)
import qualified Language.SQL.SimpleSQL.Dialect as Dialect
import qualified Language.SQL.SimpleSQL.Parse   as Parse
import qualified Language.SQL.SimpleSQL.Syntax  as Syntax
import           System.FilePath                (dropExtension)
import qualified Text.Parsec              as P
import qualified Text.Parsec.Error        as P
import qualified Text.Parsec.String       as P

data FROM =
      AnyFile FilePath
    | Jannos [JannosFROM]

data JannosFROM =
      ViaLatestPackages [FilePath]
    | ViaAllPackages [FilePath]
    | DirectJannoFiles [FilePath]

readFROM :: String -> Either String FROM
readFROM s =
    case P.runParser parseFROMString () "" s of
        Left err -> Left $ showParsecErr err
        Right x  -> Right x
    where
        parseFROMString :: P.Parser FROM
        parseFROMString = P.try parseJannos P.<|> (AnyFile <$> P.many1 P.alphaNum)
        parseJannos :: P.Parser FROM
        parseJannos = Jannos <$>
            P.sepBy1
            (P.try parseViaLatestPackages P.<|> P.try parseViaAllPackages P.<|> P.try parseDirectJannoFiles)
            consumeCommaSep
        parseViaLatestPackages :: P.Parser JannosFROM
        parseViaLatestPackages = do
            _ <- P.string "d("
            paths <- parseListOfPaths
            _ <- P.char ')'
            return $ ViaLatestPackages paths
        parseViaAllPackages :: P.Parser JannosFROM
        parseViaAllPackages = do
            _ <- P.string "da("
            paths <- parseListOfPaths
            _ <- P.char ')'
            return $ ViaAllPackages paths
        parseDirectJannoFiles :: P.Parser JannosFROM
        parseDirectJannoFiles = do
            _ <- P.string "j("
            paths <- parseListOfPaths
            _ <- P.char ')'
            return $ DirectJannoFiles paths

parseListOfPaths :: P.Parser [FilePath]
parseListOfPaths = P.sepBy1 (P.many1 P.alphaNum) consumeCommaSep

consumeCommaSep :: P.Parser ()
consumeCommaSep = do
    _ <- P.spaces *> P.char ',' <* P.spaces
    return ()

showParsecErr :: P.ParseError -> String
showParsecErr err =
    P.showErrorMessages
        "or" "unknown parse error"
        "expecting" "unexpected" "end of input"
        (P.errorMessages err)


type TableNameMap = Map.Map String String

-- | Replace all the occurrence of table names (file names, in many cases) into
-- valid table names in SQL.
replaceTableNames :: String -> (String, TableNameMap)
replaceTableNames qs = (replaceQueryWithTableMap tableMap qs, tableMap)
  where genTableName :: String -> String
        genTableName path =
          case dropWhile isNumber $ filter isAlphaNum $ dropExtension path of
            "" -> "empty_name_table"
            x  -> x
        tableMap = Map.fromList [ (name, genTableName name) | name <- roughlyExtractTableNames qs ]

-- | This function roughly extract the table names. We need this function because
-- the given query contains the file names so the SQL parser cannot parse.
roughlyExtractTableNames :: String -> [String]
roughlyExtractTableNames qs = [ ys | (xs, ys) <- zip qss (drop 2 qss), isTableNamePrefix xs ]
  where qss = splitQuery qs

-- | The words after these words are possibly table names.
isTableNamePrefix :: String -> Bool
isTableNamePrefix xs = map toUpper xs `elem` ["FROM", "JOIN"]

-- | Replace the table names using the tableMap.
replaceQueryWithTableMap :: TableNameMap -> String -> String
replaceQueryWithTableMap tableMap qs = query
  where qss = splitQuery qs
        query = concat [ if isTableNamePrefix xs then Map.findWithDefault ys ys tableMap else ys | (xs, ys) <- zip ("" : "" : qss) qss ]

-- | Split the query string with spaces, taking the quotes into consideration.
splitQuery :: String -> [String]
splitQuery = unfoldr split'
  where split' :: String -> Maybe (String, String)
        split' ccs@(c:cs) | c `elem` "\"'`" = Just $ splitAt (1 + countUntil c cs) ccs
                          | isSpace c = Just $ span isSpace ccs
                          | otherwise = Just $ break (\d -> isSpace d || d `elem` "\"'`") ccs
        split' [] = Nothing
        countUntil c ('\\':c':cs) | c == c' = 2 + countUntil c cs
        countUntil c (c':cs) | c == c' = 1
                             | otherwise = 1 + countUntil c cs
        countUntil _ [] = 0

-- | Replace the generated table names back into the original file names.
replaceBackTableNames :: TableNameMap -> String -> String
replaceBackTableNames tableMap = replaceQueryWithTableMap reverseMap
  where reverseMap = Map.fromList $ map swap $ Map.toList tableMap

-- | Extracts the table names using the rigid SQL parser.
extractTableNames :: String -> FilePath -> Either Parse.ParseError [String]
extractTableNames query path = everything (++) ([] `mkQ` tableNames)
                            <$> Parse.parseQueryExpr Dialect.mysql path Nothing query
  where tableNames (Syntax.TRSimple (name:_)) = fromName name
        tableNames _                          = []
        fromName (Syntax.Name _ name) = [name]

errorString :: Parse.ParseError -> String
errorString = Parse.peFormattedError
