-- Naive native DFT implementation
-- Source: http://www.skybluetrades.net/blog/posts/2013/11/13/data-analysis-fft-1.html

import           Criterion.Main
import           Data.Complex
import qualified Data.Vector    as V

iv_2_16 :: V.Vector (Complex Double)
iv_2_16 = V.generate (2^16) (\i -> if i == 0 then 1 :: Complex Double else 0)

i :: Complex Double
i = 0 :+ 1

omega :: Int -> Complex Double
omega n = cis (2 * pi / fromIntegral n)

dft, idft :: V.Vector (Complex Double) -> V.Vector (Complex Double)
dft = dft' 1 1
idft v = dft' (-1) (1.0 / (fromIntegral $ V.length v)) v

dft' :: Int -> Double -> V.Vector (Complex Double) -> V.Vector (Complex Double)
dft' sign scale h = V.generate bigN (((scale :+ 0) *) . doone)
  where bigN = V.length h
        w = omega bigN
        doone n = V.sum $
                  V.zipWith (*) h $ V.generate bigN (\k -> w^^(sign*n*k))

defuzz :: V.Vector (Complex Double) -> V.Vector (Complex Double)
defuzz = V.map (\(r :+ i) -> df r :+ df i)
  where df x = if abs x < 1.0E-6 then 0 else x

main :: IO ()
main = defaultMain [ bench "native_2^16" $ nf (V.toList . dft) iv_2_16 ]

