import           Control.Applicative
import           Control.Monad
import           Control.Monad.Primitive
import           Data.Double.Conversion.Text
import qualified Data.Text                   as T
import Data.Int
import qualified Data.Text.IO as TIO
import qualified Data.Vector.Unboxed         as V
import           System.Environment
import           System.IO
import           System.Random.MWC

printData :: V.Vector (Int64, Int64, Double) -> Handle -> IO ()
printData vec hdl = V.forM_ vec $ \(m, n, d) -> TIO.hPutStrLn hdl $ format m n d

format :: Int64 -> Int64 -> Double -> T.Text
format m n d = 
               (T.pack $ show m)
    `T.append` (T.cons ',' $ T.pack $ show n)
    `T.append` (T.cons ',' $ toFixed 2 d)

mkRow :: Int -> Int -> Gen (PrimState IO) -> IO (V.Vector (Int64, Int64, Double))
mkRow m n gen = 
        V.map (\(n', d) -> (fromIntegral m, n', d))
    <$> V.zip (V.enumFromN 1 n) 
    <$> V.map (* 20000) 
    <$> uniformVector gen n

main :: IO ()
main = do
    [ms, ns, f] <- getArgs
    let m = read ms
        n = read ns

    gen <- create

    withFile f WriteMode $ \hdl ->
        forM_ [1..m] $ \i -> do
            when ((i `mod` 1000) == 0) (putStrLn $ show i)
            row <- mkRow i n gen
            printData row hdl
