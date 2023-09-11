{-# LANGUAGE OverloadedStrings #-}

module Qjanno.Janno where

import qualified Qjanno.Parser                     as Parser

import           Control.Monad    (filterM)
import           Data.Char        (isSpace)
import           Data.Foldable    (foldl')
import           Data.List        (transpose)
import qualified Data.Map.Strict  as M
import qualified Data.Set         as Set
import           System.Directory (doesDirectoryExist, listDirectory)
import           System.FilePath  (takeExtension, takeFileName, (</>), takeDirectory)
import Data.Version (Version)
import           Data.Aeson                 (FromJSON, withObject, parseJSON,
                                             (.:), (.:?))
import System.IO
import Data.Yaml (decodeFileEither)
import Data.Either (lefts, rights)

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
        Parser.ViaLatestPackages _ -> do
            return ["test1"]
        Parser.ViaAllPackages ps -> do
            ymlPaths <- concat <$> mapM findAllPOSEIDONymlFiles ps
            errOrJannoPath <- mapM getAbsJannoPath ymlPaths
            mapM_ (hPutStrLn stderr) $ lefts errOrJannoPath
            return $ rights errOrJannoPath
        Parser.DirectJannoFiles ps -> do
            concat <$> mapM findAllJannoFiles ps
    where
        getAbsJannoPath :: FilePath -> IO (Either String FilePath)
        getAbsJannoPath ymlPath = do
            eitherYml <- decodeFileEither ymlPath
            case eitherYml of
                Left e    -> return $ Left $ ymlPath ++ ": " ++ show e
                Right yml ->
                    case _posYamlJannoFile yml of
                        Nothing -> return $ Left $ ymlPath ++ ": No .janno file linked"
                        Just x  -> return $ Right $ takeDirectory ymlPath </> x


extractBaseDirs :: String -> [FilePath]
extractBaseDirs baseDirsString =
    map trimWS $ splitDirs $ removeFrame baseDirsString
    where
        removeFrame :: String -> String
        removeFrame s = reverse $ drop 1 $ reverse $ drop 2 s
        splitDirs :: String -> [String]
        splitDirs s =
            let p = (==',')
            in case dropWhile p s of
              "" -> []
              s' -> w : splitDirs s'' where (w, s'') = break p s'
        trimWS :: String -> String
        trimWS = f . f where f = reverse . dropWhile isSpace

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
