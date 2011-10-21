
module Radio.Util where

import Control.Monad (filterM)
import List (sortBy)
import System.Directory (getDirectoryContents, doesFileExist, removeFile, canonicalizePath)
import System.Posix.Types (EpochTime)
import System.Posix.Files (accessTime, getFileStatus)
import System.Cmd (rawSystem)
import Text.Printf (printf)

-- import Debug.Trace (trace)


cmdLogging = False

rawSystem' c a = do
  _ <- if cmdLogging then putStrLn $ c ++ (show a)
       else return ()
  rawSystem c a


withSlash dir = if last dir == '/'
                then dir
                else dir ++ "/"


joinPath dir file = (withSlash dir) ++ file

filterHidden = filter (\fn -> head fn /= '.')


fileAccessTime :: FilePath -> IO EpochTime
fileAccessTime fp = (getFileStatus fp) >>= (return . accessTime)


allFilesRecursive :: FilePath -> IO [FilePath]
allFilesRecursive fp = do
  isFile <- doesFileExist fp
  case isFile of
    True      -> return [fp]
    otherwise -> do
      dirContents    <- ((getDirectoryContents fp) >>= (return . filterHidden))
      subdirContents <- sequence (map (allFilesRecursive . (joinPath fp)) dirContents)
      return (concat subdirContents)


sortOldestFirst :: [FilePath] -> IO [FilePath]
sortOldestFirst files = do
  accTimes <- sequence (map fileAccessTime files)
  return (map fst (sortBy orderAccTime (zip files accTimes))) where
    orderAccTime (_,ma) (_,mb) = compare ma mb


touchFile :: FilePath -> IO ()
touchFile fp = do
  _ <- rawSystem' "touch" ["-a", fp]
  return ()


makeFileAncient :: FilePath -> Int -> IO ()
makeFileAncient fp idx = let
    offsetStr = printf "%05d" idx
  in do
    _ <- rawSystem' "touch" ["-a", "-d", "1970-01-01 00:00:00." ++ offsetStr, fp]
    return ()


hardlinkFile :: FilePath -> FilePath -> IO ()
hardlinkFile file dir = do
  _ <- rawSystem' "ln" [file, dir]
  return ()


moveFilesAsAncient :: [FilePath] -> FilePath -> IO ()
moveFilesAsAncient files dir = let
  moveAndMakeAncient (file, idx) = do
    _ <- makeFileAncient file idx
    _ <- hardlinkFile file dir
    removeFile file
  in do
    _ <- sequence $ map moveAndMakeAncient (zip files [0..])
    return ()


nextFileToPlay :: FilePath -> IO FilePath
nextFileToPlay dir = 
  (allFilesRecursive dir) >>= sortOldestFirst >>= (return . head) >>= canonicalizePath
