
module Radio.PrintNextToPlay where

import Radio.Util (touchFile, oldestFileInDir)

printNextToPlay :: FilePath -> IO ()
printNextToPlay dir = do
  fp <- oldestFileInDir dir
  touchFile fp
  putStrLn fp
  return ()
