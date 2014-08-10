import Data.Char
import Data.Digest.Pure.SHA
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
  time <- fmap show timestamp
  putStrLn $ requestToken staticToken time

staticToken = "m198sOkJEn37DjqZ32lpRu76xmw288xSQ9"

requestToken :: String -> String -> String
requestToken auth time= 
  let
    secret = "iEk21fuwZApXlz93750dmW22pw389dPwOk"
    pattern = "0001110111101110001111010101111011010001001110011000110001000110"
    first = snapHash secret auth
    second = snapHash time secret
  in
    join pattern first second

snapHash :: String -> String -> String
snapHash first second = showDigest ( sha256 ( B.pack (first ++ second)))

join :: String -> String -> String -> String
join [] _ _ = []
join (p:ps) (a:as) (b:bs)
  | p == '0' = a : join ps as bs
  | p == '1' = b : join ps as bs
  | otherwise = "invalid input"

timestamp :: IO Integer
timestamp = fmap round getPOSIXTime
