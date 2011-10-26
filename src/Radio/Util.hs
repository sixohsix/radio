
module Radio.Util where

import List (sortBy)
import System.Directory (getDirectoryContents, doesFileExist, canonicalizePath)
import System.Posix.Types (EpochTime)
import System.Posix.Files (accessTime, getFileStatus, modificationTime, setFileTimes)

import Debug.Trace (trace, traceShow)


withSlash :: FilePath -> FilePath
withSlash dir = if last dir == '/'
                then dir
                else dir ++ "/"


joinPath :: FilePath -> FilePath -> FilePath
joinPath dir file = (withSlash dir) ++ file


filterHidden :: [FilePath] -> [FilePath]
filterHidden = filter (\fn -> head fn /= '.')


fileModTime :: FilePath -> IO EpochTime
fileModTime fp = (getFileStatus fp) >>= (return . modificationTime)


allFilesRecursive :: FilePath -> IO [FilePath]
allFilesRecursive fp = do
  isFile <- doesFileExist fp
  case isFile of
    True      -> return [fp]
    False     -> do
      dirContents    <- ((getDirectoryContents fp) >>= (return . filterHidden))
      subdirContents <- sequence (map (allFilesRecursive . (joinPath fp)) dirContents)
      return (concat subdirContents)


sortOldestFirst :: [FilePath] -> IO [FilePath]
sortOldestFirst files = do
  modTimes <- sequence (map fileModTime files)
  return (map fst (sortBy orderModTime (zip files modTimes))) where
    orderModTime (_,ma) (_,mb) = compare ma mb


nextFileToPlay :: FilePath -> IO FilePath
nextFileToPlay dir = 
  (allFilesRecursive dir) >>= sortOldestFirst >>= (return . head) >>= canonicalizePath


setModTime :: FilePath -> EpochTime -> IO ()
setModTime fp mtime = setFileTimes fp mtime mtime
