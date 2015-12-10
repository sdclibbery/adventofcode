import qualified Data.ByteString.Char8 as BS
import Crypto.Hash
import Debug.Trace
import qualified Data.List

md5core :: String -> Digest MD5
md5core = hash . BS.pack

md5 :: String -> String
md5 = show . digestToHexByteString . md5core

tryit :: Integer -> String
tryit n = md5 $ input ++ show n

findit :: (String -> Bool) -> Integer
findit pred = until checkit (+1) 0
	where
		checkit n = (n > 100000000) || (pred $ tryit n)

startsWithFiveZeros s = Data.List.isPrefixOf "\"00000" s
startsWithSixZeros s = Data.List.isPrefixOf "\"000000" s

--main = putStrLn $ show $ findit startsWithFiveZeros
main = putStrLn $ show $ findit startsWithSixZeros

input = "iwrupvqb"