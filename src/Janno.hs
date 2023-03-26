module Janno where

import           System.Directory           (doesDirectoryExist, listDirectory)
import           System.FilePath            ((</>), takeFileName, takeExtension)
import Control.Monad (filterM)
import qualified Data.Map.Strict as M
import Data.Foldable (foldl')
import Data.List (transpose)
import qualified Data.Set as Set
import Data.Char (isSpace)

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

findAllJannoFiles :: FilePath -> IO [FilePath]
findAllJannoFiles baseDir = do
    entries <- listDirectory baseDir
    let files = map (baseDir </>) $ filter (\p -> takeExtension p == ".janno") $ map takeFileName entries
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
