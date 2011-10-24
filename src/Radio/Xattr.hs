
module Radio.Xattr where

import Data.ByteString (ByteString)
import Data.Text (pack, unpack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import System.Xattr (XattrMode (RegularMode), getxattr, setxattr)


lastPlayTimeKey :: String
lastPlayTimeKey = "user.sixohsix_radio.lastPlayTime"


encodeIntUTF8 :: Int -> ByteString
encodeIntUTF8 = encodeUtf8 . pack . show


decodeIntUTF8 :: ByteString -> Int
decodeIntUTF8 = read . unpack . decodeUtf8


getLastPlayTime :: FilePath -> IO Int
getLastPlayTime fp = do
  attr <- catch (getxattr fp lastPlayTimeKey)
                (\e -> return (encodeIntUTF8 0))
  return (decodeIntUTF8 attr)


setLastPlayTime :: FilePath -> Int -> IO ()
setLastPlayTime fp playTime = setxattr fp lastPlayTimeKey (encodeIntUTF8 playTime) RegularMode
