
import System.Environment (getArgs)
import System.Posix.Files (touchFile)
import Radio.Util (nextFileToPlay)


main :: IO ()
main = do
  args <- getArgs
  dir <- return $ if null args
    then error "You need to give the directory to find songs in."
    else head args
  nextToPlay <- nextFileToPlay dir
  _ <- putStrLn nextToPlay
  touchFile nextToPlay
