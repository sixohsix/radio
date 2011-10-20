
module Radio.Util where

import Control.Monad (filterM)
import List (sortBy)
import System.Directory (getDirectoryContents, getModificationTime, doesFileExist)
import System.Cmd (rawSystem)


oldestFileInDir :: FilePath -> IO FilePath
oldestFileInDir dir = do 
  dirContents        <- getDirectoryContents dir
  dirFiles           <- filterM doesFileExist dirContents
  dirNonHiddenFiles  <- return $ filter (\fn -> head fn /= '.') dirFiles  -- Ignore hidden files
  modTimes <- sequence $ map getModificationTime dirNonHiddenFiles
  return $ fst $ head $ sortBy orderModTime $ zip dirNonHiddenFiles modTimes where
    orderModTime (_,ma) (_,mb) = compare ma mb

touchFile :: FilePath -> IO ()
touchFile fp = do
  _ <- rawSystem "touch" [fp]
  return ()
