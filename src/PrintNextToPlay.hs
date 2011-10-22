
import System.Environment (getArgs)
import Radio.Util (nextFileToPlay)
import Radio.Xattr (setLastPlayTime)
import Data.Time.Clock.POSIX (getPOSIXTime)


main :: IO ()
main = do
  args <- getArgs
  dir <- return $ if null args
    then error "You need to give the directory to find songs in."
    else head args
  nextToPlay <- nextFileToPlay dir
  _ <- putStrLn nextToPlay
  now <- getPOSIXTime >>= (return . floor . toRational)
  setLastPlayTime nextToPlay now
