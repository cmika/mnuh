import Criterion.Main
import CMUDict
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

setupEnv :: IO ByteString
setupEnv = BS.readFile "cmudict-0.7b"

main :: IO ()
main = defaultMain [env setupEnv $ \input -> bench "read dictionary" $ whnf buildTrie input]