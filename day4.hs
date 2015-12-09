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

startsWithFiveZeros :: String -> Bool
startsWithFiveZeros s = Data.List.isPrefixOf "\"00000" s

findit :: Integer
findit = until checkit (+1) 0
	where
		checkit n = (n > 1200000) || (startsWithFiveZeros $ tryit n) -- (trace (show n) $ startsWithFiveZeros $ tryit n)

main = putStrLn $ show findit

input = "iwrupvqb"