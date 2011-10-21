
import System.Environment (getArgs)
import Radio.Util (nextFileToPlay)


printNextToPlay :: FilePath -> IO ()
printNextToPlay dir = (nextFileToPlay dir) >>= putStrLn


main :: IO ()
main = do
  args <- getArgs
  dir <- return $ if null args
    then error "You need to give the directory to find songs in."
    else head args
  printNextToPlay dir
