module Main where
       
import System.IO
import Control.Monad
import Text.Printf

indexes :: Integer -> [Integer]
indexes n = [0..n-1]

renderIndexes :: [Integer] -> [String]
renderIndexes vec = map show vec

writeIndex :: Integer -> IO ()
writeIndex n = do
    let vec = renderIndexes $ indexes  n
    let f   = "idx_" ++ show n ++ ".dat"
    withFile f WriteMode $ \h -> 
        (forM_ vec $ \x -> hPutStrLn h x)

main :: IO ()
main = forM_ [16, 64, 256, 1024, 4096, 16384] writeIndex
    
