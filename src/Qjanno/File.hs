{-# LANGUAGE BangPatterns #-}

module Qjanno.File where

import           Control.Applicative ((<|>))
import           Control.Monad       (guard, when)
import           Data.Char           (isSpace)
import           Data.Version        (Version, showVersion)
import           System.Exit         (exitFailure)
import           System.IO

import qualified Qjanno.Janno        as Janno
import qualified Qjanno.Option       as Option

readFromJanno :: Option.Option -> Janno.JannoWithContext -> IO ([String], [[String]])
readFromJanno opts (Janno.JannoWithContext p Nothing) = do
    readFromFile opts p
readFromJanno opts (Janno.JannoWithContext p (Just (_, Janno.PoseidonYml pacTitle pacVersion _))) = do
    (columns, body) <- readFromFile opts p
    let columnsWithPacTitleAndVersion = "package_title" : "package_version" : columns
    let bodyWithPacTitleAndVersion = map (\x -> pacTitle : renderPacVersion pacVersion : x) body
    return (columnsWithPacTitleAndVersion, bodyWithPacTitleAndVersion)
    where
        renderPacVersion :: Maybe Version -> String
        renderPacVersion Nothing  = ""
        renderPacVersion (Just x) = showVersion x

readFromFile :: Option.Option -> FilePath -> IO ([String], [[String]])
readFromFile opts path = do
  !contents <- joinMultiLines <$> lines <$> readFile path
  let contentList = contents ++ [ "", "" ]
      headLine = contentList !! 0
      secondLine = contentList !! 1
  let delimiter = guard (Option.tabDelimited opts) *> Just "\t" <|> Option.delimiter opts
  when (maybe False ((/=1) . length) delimiter) $ do
    hPutStrLn stderr "Invalid delimiter."
    exitFailure
  let splitter = case delimiter of
                      Just [c] -> (==) c
                      _        -> detectSplitter headLine secondLine
  let headColumns = splitFixedSize splitter 0 headLine
  let size = length headColumns
  let columns = if Option.noHeader opts then [ 'c' : show i | i <- [1..size] ] else headColumns
  let skipLine = if Option.noHeader opts then id else tail
  let stripSpaces = dropWhile isSpace
  let body = filter (not . null) $ map (map stripSpaces . splitFixedSize splitter size) (skipLine contents)
  -- add file path column
  let columnsWithSourceFile = "source_file" : columns
  let bodyWithSourceFile = map (path:) body
  return (columnsWithSourceFile, bodyWithSourceFile)
  where joinMultiLines (cs:ds:css) | valid True cs = cs : joinMultiLines (ds:css)
                                   | otherwise = joinMultiLines $ (cs ++ "\n" ++ ds) : css
          where valid False ('"':'"':xs)  = valid False xs
                valid False ('\\':'"':xs) = valid False xs
                valid b ('"':xs)          = valid (not b) xs
                valid b (_:xs)            = valid b xs
                valid b ""                = b
        joinMultiLines css = css

detectSplitter :: String -> String -> Char -> Bool
detectSplitter xs ys = head $ [ splitter | (x, y, splitter) <- map splitLines splitters
                                         , 1 < length x && length x <= length y ] ++ splitters
  where splitLines f = (splitFixedSize f 0 xs, splitFixedSize f 0 ys, f)
        splitters = [ (==','), isSpace ]

splitFixedSize :: (Char -> Bool) -> Int -> String -> [String]
splitFixedSize f n = fill . go n
  where go _ "" = []
        go k (c:cs@(c':_)) | f c && f c' && not (f ' ' && isSpace c') = "" : go (k - 1) cs
                           | f c = go k cs
        go k ('"':cs) = let (ys, xs) = takeQuotedString cs in xs : go (k - 1) ys
          where takeQuotedString ('"':'"':xs) = fmap ('"':) (takeQuotedString xs)
                takeQuotedString ('\\':'"':xs) = fmap ('"':) (takeQuotedString xs)
                takeQuotedString ('"':xs) = (xs, "")
                takeQuotedString (x:xs) = fmap (x:) (takeQuotedString xs)
                takeQuotedString "" = ("", "")
        go k (c:cs) | f c = go k cs
        go 1 cs = [cs]
        go k cs = let (xs, ys) = break f cs in xs : go (k - 1) ys
        fill [] = []
        fill xs = xs ++ replicate (n - length xs) ""
