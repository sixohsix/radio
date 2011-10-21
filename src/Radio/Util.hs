
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


fileAccessTime :: FilePath -> IO EpochTime
fileAccessTime fp = (getFileStatus fp) >>= (return . accessTime)


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
  accTimes <- sequence (map fileAccessTime files)
  return (map fst (sortBy orderAccTime (zip files accTimes))) where
    orderAccTime (_,ma) (_,mb) = compare ma mb


setAccessTime :: FilePath -> EpochTime -> IO ()
setAccessTime fp atime = do
  _ <- return (traceShow fp ())
  mtime <- (getFileStatus fp) >>= (return . modificationTime)
  setFileTimes fp atime mtime


makeFileAncient :: FilePath -> EpochTime -> IO ()
makeFileAncient = setAccessTime


nextFileToPlay :: FilePath -> IO FilePath
nextFileToPlay dir = 
  (allFilesRecursive dir) >>= sortOldestFirst >>= (return . head) >>= canonicalizePath
