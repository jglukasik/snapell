import Data.Char
import qualified Data.ByteString.Lazy.Char8 as B
import Crypto.Hash.SHA256

import Data.Digest.Pure.SHA


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


