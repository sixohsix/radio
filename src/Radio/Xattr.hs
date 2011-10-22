
module Radio.Xattr where

import Data.ByteString (ByteString)
import Data.Encoding (encodeStrictByteString, decodeStrictByteString)
import Data.Encoding.UTF8
import System.Xattr (XattrMode (RegularMode), getxattr, setxattr)


lastPlayTimeKey :: String
lastPlayTimeKey = "ca.verdone.radio.lastPlayTime"


encodeIntUTF8 :: Int -> ByteString
encodeIntUTF8 = encodeStrictByteString UTF8 . show


decodeIntUTF8 :: ByteString -> Int
decodeIntUTF8 = read . decodeStrictByteString UTF8


getLastPlayTime :: FilePath -> IO Int
getLastPlayTime fp = do
  attr <- catch (getxattr fp lastPlayTimeKey)
                (\e -> return (encodeIntUTF8 0))
  return (decodeIntUTF8 attr)


setLastPlayTime :: FilePath -> Int -> IO ()
setLastPlayTime fp playTime = setxattr fp lastPlayTimeKey (encodeIntUTF8 playTime) RegularMode

