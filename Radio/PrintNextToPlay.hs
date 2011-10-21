
module Radio.PrintNextToPlay where

import Radio.Util (touchFile, oldestFileInDir, canonicalizeFileInDir)

printNextToPlay :: FilePath -> IO ()
printNextToPlay dir = do
  fp <- oldestFileInDir dir
  absFp <- canonicalizeFileInDir dir fp
  putStrLn absFp
  return ()
