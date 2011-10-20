
module Radio.Util where

import Control.Monad (filterM)
import List (sortBy)
import System.Directory (getDirectoryContents, getModificationTime, doesFileExist)
import System.Cmd (rawSystem)


oldestFileInDir :: FilePath -> IO FilePath
oldestFileInDir dir = do 
  dirContents <- getDirectoryContents dir
  dirContents <- filterM doesFileExist dirContents
  dirContents <- return $ filter (\fn -> head fn /= '.') dirContents  -- Ignore hidden files
  modTimes <- sequence $ map getModificationTime dirContents
  return $ fst $ head $ sortBy orderModTime $ zip dirContents modTimes where
    orderModTime (a,ma) (b,mb) = compare ma mb

touchFile :: FilePath -> IO ()
touchFile fp = do
  rawSystem "touch" [fp]
  return ()
