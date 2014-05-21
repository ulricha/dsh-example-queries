{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Complex numbers in DSH
module DFT.Complex where

import Database.DSH

--------------------------------------------------------------------
-- Complex vectors stored in base tables

fst3 :: (QA a, QA b, QA c) => Q (a, b, c) -> Q a
fst3 (view -> (a, _, _)) = a

vec :: String -> Q [(Integer, Double, Double)]
vec n = table n (defaultHints { keysHint = [Key ["pos"]], nonEmptyHint = NonEmpty })

vecFromTable :: Q [(Integer, Double, Double)] -> Q [Complex]
vecFromTable tab = map (\(view -> (_, i, r)) -> pair r i) $ sortWith fst3 tab

--------------------------------------------------------------------
-- Helpers, type definitions

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

-- * Complex numbers

type Complex = (Double, Double)

-- | Construction of complex values
(|+) :: Q Double -> Q Double -> Q Complex
(|+) = pair

-- | Complex multiplication
-- (a+bi)*(c+di) = (a*c - b*d)+(b*c + c*d)i
(.*) :: Q Complex -> Q Complex -> Q Complex
x .* y = (a*c - b * d) |+ (b*c - c*d)
  where
    (a, b) = view x
    (c, d) = view y

infixl 7 .*

(.+) :: Q Complex -> Q Complex -> Q Complex
x .+ y = (a + c) |+ (b + d)
  where
    (a, b) = view x
    (c, d) = view y

infixl 6 .+

(.-) :: Q Complex -> Q Complex -> Q Complex
x .- y = (a - c) |+ (b - d)
  where
    (a, b) = view x
    (c, d) = view y

infixl 6 .-

real :: Q Complex -> Q Double
real = fst

img :: Q Complex -> Q Double
img = snd

