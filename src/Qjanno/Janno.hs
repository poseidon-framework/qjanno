{-# LANGUAGE OverloadedStrings #-}

module Qjanno.Janno where

import qualified Qjanno.Parser    as Parser

import           Control.Monad    (filterM)
import           Data.Aeson       (FromJSON, parseJSON, withObject, (.:), (.:?))
import           Data.Either      (lefts, rights)
import           Data.Foldable    (foldl')
import           Data.List        (elemIndices, groupBy, sortBy, sortOn,
                                   transpose)
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

data JannoWithContext = JannoWithContext
    { _jannoContextJannoPath :: FilePath
    , _jannoContextYml       :: Maybe (FilePath, PoseidonYml)
    }

findJannos :: Parser.JannosFROM -> IO [JannoWithContext]
findJannos j = do
    case j of
        Parser.ViaLatestPackages ps -> do
            ymlPaths <- concat <$> mapM findAllPOSEIDONymlFiles ps
            eitherYmlFiles <- mapM readYmlFile ymlPaths
            mapM_ (hPutStrLn stderr) $ lefts eitherYmlFiles
            let lastVersions = filterToLastPackageVersion $ rights eitherYmlFiles
            eitherJannoWithContext <- mapM makeJannoWithContext lastVersions
            mapM_ (hPutStrLn stderr) $ lefts eitherJannoWithContext
            return $ rights eitherJannoWithContext
        Parser.ViaAllPackages ps -> do
            ymlPaths <- concat <$> mapM findAllPOSEIDONymlFiles ps
            eitherYmlFiles <- mapM readYmlFile ymlPaths
            mapM_ (hPutStrLn stderr) $ lefts eitherYmlFiles
            eitherJannoWithContext <- mapM makeJannoWithContext $ rights eitherYmlFiles
            mapM_ (hPutStrLn stderr ) $ lefts eitherJannoWithContext
            return $ rights eitherJannoWithContext
        Parser.AllJannoFiles ps -> do
            map (\x -> JannoWithContext x Nothing) . concat <$> mapM findAllJannoFiles ps
        Parser.OneJannoFile p -> do
            return [JannoWithContext p Nothing]
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
            moreFiles <- fmap concat . mapM (findAllFilesByPredicate predicate) $ subDirs
            return $ files ++ moreFiles
        readYmlFile :: FilePath -> IO (Either String (FilePath, PoseidonYml))
        readYmlFile ymlPath = do
            eitherYml <- decodeFileEither ymlPath
            case eitherYml of
                Left e    -> return $ Left $ ymlPath ++ ": " ++ show e
                Right yml -> return $ Right (ymlPath, yml)
        filterToLastPackageVersion :: [(FilePath, PoseidonYml)] -> [(FilePath, PoseidonYml)]
        filterToLastPackageVersion ymlsWithPath =
            case ymlsWithPath of
                [] -> []
                xs -> map last $ groupBy compfunc $ sortBy ordfunc xs
            where
                ordfunc  (_, PoseidonYml t1 v1 _) (_, PoseidonYml t2 v2 _) = compare (t1, v1) (t2, v2)
                compfunc (_, PoseidonYml t1 _ _)  (_, PoseidonYml t2 _ _)  = t1 == t2
        makeJannoWithContext :: (FilePath, PoseidonYml) -> IO (Either String JannoWithContext)
        makeJannoWithContext (ymlPath, ymlFile) = do
            case _posYamlJannoFile ymlFile of
                Nothing -> return $ Left $ ymlPath ++ ": No .janno file linked"
                Just x  -> return $ Right $ JannoWithContext (takeDirectory ymlPath </> x) (Just (ymlPath, ymlFile))

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

reorderJannoColumns :: ([String], [[String]]) -> ([String], [[String]])
reorderJannoColumns (oldCols, oldRowsData) =
    let orderedCols = sortOn getOrder oldCols
        orderingIndices = concatMap (`elemIndices` oldCols) orderedCols
        orderedRowsData = map (\row -> map (row !!) orderingIndices) oldRowsData
    in (orderedCols, orderedRowsData)
    where
        -- https://stackoverflow.com/a/26260968/3216883
        getOrder :: String -> Int
        getOrder k = M.findWithDefault (length jannoOrder) k ordermap
        ordermap :: M.Map String Int
        ordermap = M.fromList (zip jannoOrder [0..])

jannoOrder :: [String]
jannoOrder = "package_title" : "package_version" : "source_file" : jannoHeader

jannoHeader :: [String]
jannoHeader = [
      "Poseidon_ID"
    , "Genetic_Sex"
    , "Group_Name"
    , "Alternative_IDs"
    , "Relation_To"
    , "Relation_Degree"
    , "Relation_Type"
    , "Relation_Note"
    , "Collection_ID"
    , "Country"
    , "Country_ISO"
    , "Location"
    , "Site"
    , "Latitude"
    , "Longitude"
    , "Date_Type"
    , "Date_C14_Labnr"
    , "Date_C14_Uncal_BP"
    , "Date_C14_Uncal_BP_Err"
    , "Date_BC_AD_Start"
    , "Date_BC_AD_Median"
    , "Date_BC_AD_Stop"
    , "Date_Note"
    , "MT_Haplogroup"
    , "Y_Haplogroup"
    , "Source_Tissue"
    , "Nr_Libraries"
    , "Library_Names"
    , "Capture_Type"
    , "UDG"
    , "Library_Built"
    , "Genotype_Ploidy"
    , "Data_Preparation_Pipeline_URL"
    , "Endogenous"
    , "Nr_SNPs"
    , "Coverage_on_Target_SNPs"
    , "Damage"
    , "Contamination"
    , "Contamination_Err"
    , "Contamination_Meas"
    , "Contamination_Note"
    , "Genetic_Source_Accession_IDs"
    , "Primary_Contact"
    , "Publication"
    , "Note"
    , "Keywords"
    ]
