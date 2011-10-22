
module Radio.Util where

import List (sortBy)
import System.Directory (getDirectoryContents, doesFileExist, canonicalizePath)
import Radio.Xattr (getLastPlayTime)

-- import Debug.Trace (trace, traceShow)


withSlash :: FilePath -> FilePath
withSlash dir = if last dir == '/'
                then dir
                else dir ++ "/"


joinPath :: FilePath -> FilePath -> FilePath
joinPath dir file = (withSlash dir) ++ file


filterHidden :: [FilePath] -> [FilePath]
filterHidden = filter (\fn -> head fn /= '.')


allFilesRecursive :: FilePath -> IO [FilePath]
allFilesRecursive fp = do
  isFile <- doesFileExist fp
  case isFile of
    True      -> return [fp]
    False     -> do
      dirContents    <- ((getDirectoryContents fp) >>= (return . filterHidden))
      subdirContents <- sequence (map (allFilesRecursive . (joinPath fp)) dirContents)
      return (concat subdirContents)


sortByLastPlayTime :: [FilePath] -> IO [FilePath]
sortByLastPlayTime files = do
  lastPlayTimes <- sequence (map getLastPlayTime files)
  return (map fst (sortBy orderPlayTime (zip files lastPlayTimes))) where
    orderPlayTime a b = compare (snd a) (snd b)


nextFileToPlay :: FilePath -> IO FilePath
nextFileToPlay dir = 
  (allFilesRecursive dir) >>= sortByLastPlayTime >>= (return . head) >>= canonicalizePath
