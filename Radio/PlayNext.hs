
import Control.Monad (when)
import System.Environment (getArgs)
import Radio.Util (makeFileAncient)

import Debug.Trace


usageMsg :: String
usageMsg = "\nUSAGE:\n  play_next <file> [file] ..."


makeAllAncient :: [FilePath] -> IO ()
makeAllAncient files = do
  _ <- sequence (
    map (\ (f, idx) -> makeFileAncient f idx) (zip files [0..]))
  return ()


main :: IO ()
main = do
  args <- getArgs
  _ <- when (null args) (fail usageMsg)
  _ <- makeAllAncient args
  return ()
