
import Control.Monad (when)
import System.Environment (getArgs)
import Radio.Xattr (setLastPlayTime)

-- import Debug.Trace


usageMsg :: String
usageMsg = "\nUSAGE:\n  play_next <file> [file] ..."


resetPlayTimes :: [FilePath] -> IO ()
resetPlayTimes files = do
  _ <- sequence (
    map (\ (f, idx) -> setLastPlayTime f idx) (zip files [0..]))
  return ()


main :: IO ()
main = do
  args <- getArgs
  _ <- when (null args) (fail usageMsg)
  _ <- resetPlayTimes args
  return ()
