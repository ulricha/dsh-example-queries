import           Criterion.Main
import qualified Data.Vector as V
import           Data.Complex
import           Numeric.FFT

iv_2_16 :: V.Vector (Complex Double)
iv_2_16 = V.generate (2^16) (\i -> if i == 0 then 1 :: Complex Double else 0)

main = defaultMain [ bench "arb_2^16" (fft iv_2_16) ]
