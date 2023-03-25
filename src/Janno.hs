module Janno where

import           System.Directory           (doesDirectoryExist, listDirectory)
import           System.FilePath            ((</>), takeFileName, takeExtension)
import Control.Monad (filterM)

findAllJannoFiles :: FilePath -> IO [FilePath]
findAllJannoFiles baseDir = do
    entries <- listDirectory baseDir
    let files = map (baseDir </>) $ filter (\p -> takeExtension p == ".janno") $ map takeFileName entries
    subDirs <- filterM doesDirectoryExist . map (baseDir </>) $ entries
    moreFiles <- fmap concat . mapM findAllJannoFiles $ subDirs
    return $ files ++ moreFiles

mergeJannos :: [([String], [[String]])] -> ([String], [[String]])
mergeJannos xs = head xs -- just a dummy, needs to be implemented