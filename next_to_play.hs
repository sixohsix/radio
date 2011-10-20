
import System.Environment (getArgs)
import Radio.PrintNextToPlay (printNextToPlay)

main = do
  args <- getArgs
  dir <- return $ if null args
    then error "You need to give the directory to find songs in."
    else head args
  printNextToPlay dir
