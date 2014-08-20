{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Char
import Data.Maybe
import Data.Digest.Pure.SHA
import Data.Time.Clock.POSIX
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Aeson
import Network.Curl
import Network.Curl.Aeson
import Control.Applicative
import Control.Monad
import GHC.Generics
import Credentials

main :: IO ()
main = do
  time <- getTime
  let myToken = requestToken staticToken (show time)
  let myLogin = Login {username=myUsername, password=myPassword, timestamp=time, token=myToken}
  reply <- getReply myLogin
  putStrLn $ show reply

staticToken = "m198sOkJEn37DjqZ32lpRu76xmw288xSQ9"
apiURL = "https://feelinsonice-hrd.appspot.com/bq/login"

requestToken :: String -> String -> String
requestToken auth time= 
  let
    secret = "iEk21fuwZApXlz93750dmW22pw389dPwOk"
    pattern = "0001110111101110001111010101111011010001001110011000110001000110"
    first = snapHash secret auth
    second = snapHash time secret
  in
    snapJoin pattern first second

snapHash :: String -> String -> String
snapHash first second = showDigest ( sha256 ( B.pack (first ++ second)))

snapJoin :: String -> String -> String -> String
snapJoin [] _ _ = []
snapJoin (p:ps) (a:as) (b:bs)
  | p == '0' = a : snapJoin ps as bs
  | p == '1' = b : snapJoin ps as bs
  | otherwise = "invalid input"

getTime :: IO Integer
getTime = fmap round getPOSIXTime

data Login =
  Login 
    { username  :: String
    , password  :: String
    , timestamp :: Integer
    , token     :: String
    } deriving (Show,Generic)
instance FromJSON Login
instance ToJSON Login

data Reply =
  Reply 
    { message :: String
    } deriving (Show,Generic)
instance FromJSON Reply
instance ToJSON Reply

getReply :: Login -> IO Reply
getReply myLogin = curlAeson parseJSON "post" apiURL opts (return myLogin)

opts = [ CurlConnectTimeout 5, CurlTimeout 10, CurlUserAgent "Snapchat/4.1.07 (Nexus 4; Android 18; gzip)"]
