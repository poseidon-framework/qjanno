{-# LANGUAGE OverloadedStrings #-}

module Qjanno.Janno where

import qualified Qjanno.Parser    as Parser

import           Control.Monad    (filterM)
import           Data.Aeson       (FromJSON, parseJSON, withObject, (.:), (.:?))
import           Data.Either      (lefts, rights)
import           Data.Foldable    (foldl')
import           Data.List        (groupBy, sortBy, transpose)
import qualified Data.Map.Strict  as M
import qualified Data.Set         as Set
import           Data.Version     (Version)
import           Data.Yaml        (decodeFileEither)
import           System.Directory (doesDirectoryExist, listDirectory)
import           System.FilePath  (takeDirectory, takeExtension, takeFileName,
                                   (</>))
import           System.IO

data PoseidonYml = PoseidonYml
    { _posYmlPackageTite    :: String
    , _posYmlPackageVersion :: Maybe Version
    , _posYamlJannoFile     :: Maybe FilePath
    } deriving Show

instance FromJSON PoseidonYml where
    parseJSON = withObject "PoseidonYml" $ \v -> PoseidonYml
        <$> v .:   "title"
        <*> v .:?  "packageVersion"
        <*> v .:?  "jannoFile"

findJannoPaths :: Parser.JannosFROM -> IO [FilePath]
findJannoPaths j = do
    case j of
        Parser.ViaLatestPackages ps -> do
            ymlPaths <- concat <$> mapM findAllPOSEIDONymlFiles ps
            eitherYmlFiles <- mapM readYmlFile ymlPaths
            mapM_ (hPutStrLn stderr) $ lefts eitherYmlFiles
            let lastVersions = filterToLastPackageVersion $ rights eitherYmlFiles
            eitherJannoPath <- mapM getAbsJannoPath $ lastVersions
            mapM_ (hPutStrLn stderr) $ lefts eitherJannoPath
            return $ rights eitherJannoPath
        Parser.ViaAllPackages ps -> do
            ymlPaths <- concat <$> mapM findAllPOSEIDONymlFiles ps
            eitherYmlFiles <- mapM readYmlFile ymlPaths
            mapM_ (hPutStrLn stderr) $ lefts eitherYmlFiles
            eitherJannoPath <- mapM getAbsJannoPath $ rights eitherYmlFiles
            mapM_ (hPutStrLn stderr) $ lefts eitherJannoPath
            return $ rights eitherJannoPath
        Parser.DirectJannoFiles ps -> do
            concat <$> mapM findAllJannoFiles ps
    where
        findAllPOSEIDONymlFiles :: FilePath -> IO [FilePath]
        findAllPOSEIDONymlFiles = findAllFilesByPredicate (== "POSEIDON.yml")
        findAllJannoFiles :: FilePath -> IO [FilePath]
        findAllJannoFiles = findAllFilesByPredicate (\p -> takeExtension p == ".janno")
        findAllFilesByPredicate :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
        findAllFilesByPredicate predicate baseDir = do
            entries <- listDirectory baseDir
            let files = map (baseDir </>) $ filter predicate $ map takeFileName entries
            subDirs <- filterM doesDirectoryExist . map (baseDir </>) $ entries
            moreFiles <- fmap concat . mapM findAllJannoFiles $ subDirs
            return $ files ++ moreFiles
        readYmlFile :: FilePath -> IO (Either String (FilePath, PoseidonYml))
        readYmlFile ymlPath = do
            eitherYml <- decodeFileEither ymlPath
            case eitherYml of
                Left e    -> return $ Left $ ymlPath ++ ": " ++ show e
                Right yml -> return $ Right (ymlPath, yml)
        filterToLastPackageVersion :: [(FilePath, PoseidonYml)] -> [(FilePath, PoseidonYml)]
        filterToLastPackageVersion ymlsWithPath =
            let ordfunc  (_, PoseidonYml t1 v1 _) (_, PoseidonYml t2 v2 _) = compare (t1, v1) (t2, v2)
                compfunc (_, PoseidonYml t1 v1 _) (_, PoseidonYml t2 v2 _) = (t1, v1) == (t2, v2)
            in case ymlsWithPath of
                [] -> []
                xs -> last $ groupBy compfunc $ sortBy ordfunc xs
        getAbsJannoPath :: (FilePath, PoseidonYml) -> IO (Either String FilePath)
        getAbsJannoPath (ymlPath, ymlFile) = do
            case _posYamlJannoFile ymlFile of
                Nothing -> return $ Left $ ymlPath ++ ": No .janno file linked"
                Just x  -> return $ Right $ takeDirectory ymlPath </> x

mergeJannos :: [([String], [[String]])] -> ([String], [[String]])
mergeJannos xs =
    let allKeys     = Set.fromList $ concatMap fst xs
        emptyMap    = M.fromSet (const []) allKeys
        nrows       = map (\(_, rs) -> length rs) xs
        maps        = map toMap xs
        mapsAllKeys = map (`M.union` emptyMap) maps
        mapsFilled  = map (\(i,a) -> M.map (\b -> if null b then fillNA i else b) a) $ zip nrows mapsAllKeys
        merged      = foldl' (M.unionWith (++)) emptyMap mapsFilled
        output      = fromMap merged
    in output
    where
        toMap :: ([String], [[String]]) -> M.Map String [String]
        toMap (colnames, valuesByRow) = M.fromList $ zip colnames (transpose valuesByRow)
        fillNA :: Int -> [String]
        fillNA i = replicate i "n/a"
        fromMap :: M.Map String [String] -> ([String], [[String]])
        fromMap m = (M.keys m, transpose $ M.elems m)
