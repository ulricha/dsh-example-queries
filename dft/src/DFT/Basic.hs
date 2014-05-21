{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Helper functions common to all FFT implementations, as well as
-- the naive n^2 DFT.
module DFT.Basic
    ( ρ
    , φ
    , iArray
    , number0
    , ω
    , sumC
    , dft
    ) where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import DFT.Complex

-- | Matrix transposition
φ :: QA a => Q [[a]] -> Q [[a]]
φ = transpose

-- | Array reshaping
ρ :: QA a => Integer -> Q [a] -> Q [[a]]
ρ = reshape

-- | Generate a vector of (zero-based) positions from an arbitrary
-- vector.
iArray :: QA a => Q [a] -> Q [Integer]
iArray v = map snd $ number0 v

-- | Pair a vectors' elements with their (zero-based) positions.
number0 :: QA a => Q [a] -> Q [(a, Integer)]
number0 v = map (\x -> pair (fst x) ((snd x) - 1)) $ number v

cis :: Q Double -> Q Complex
cis r = cos r |+ sin r

-- | Twiddle factors: ω_N^k
ω :: Integer -> Q Integer -> Q Complex
ω n i = cis $ (toQ (2 * pi) * integerToDouble i) / (fromIntegral n)

-- | Complex vector sum
sumC :: Q [Complex] -> Q Complex
sumC cs = (sum $ map real cs) |+ (sum $ map img cs)

-- | A naive transcription of the basic DFT equation.
dft :: Integer -> Q [Complex] -> Q [Complex]
dft dim v = 
  [ sumC [ ω dim (i * j) .* x | (view -> (x, j)) <- number0 v ]
  | i <- iArray v
  ]
