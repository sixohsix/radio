
module Radio.Util where

import Control.Monad (filterM)
import List (sortBy)
import System.Directory (getDirectoryContents, doesFileExist, removeFile)
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


filesInDir :: FilePath -> IO [FilePath]
filesInDir dir = do
  dirContents <- getDirectoryContents dir
  dirFiles    <- filterM (\fn -> doesFileExist $ (withSlash dir) ++ fn)  dirContents
  return $ filter (\fn -> head fn /= '.') dirFiles  -- Ignore hidden files
  

fileAccessTime :: FilePath -> IO EpochTime
fileAccessTime fp = do
  status <- getFileStatus fp
  return (accessTime status)


oldestFileInDir :: FilePath -> IO FilePath
oldestFileInDir dir = do 
  dirFiles           <- filesInDir dir
  accTimes           <- sequence $ map (\fn -> fileAccessTime (joinPath dir fn)) dirFiles
  return $ fst $ head $ sortBy orderModTime $ zip dirFiles accTimes where
    orderModTime (_,ma) (_,mb) = compare ma mb


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


moveAllFilesInDirAsAncient inDir outDir = do
  files <- filesInDir inDir
  moveFilesAsAncient (map (joinPath inDir) files) outDir
