
import Data.List (foldl')
import System.Environment (getArgs)
import Radio.Util (makeFileAncient)

usageStr = "USAGE:\n  play_next <file> [file] ..."


makeAllAncient files =
  sequence (
    map (\ (idx, f) -> makeFileAncient f idx) (zip files [0..]))


main :: IO ()
main = do
  args <- getArgs
  case null args of
    True  -> return (error usage)
    False -> makeAllAncient args
  return ()
