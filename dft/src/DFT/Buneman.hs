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

module DFT.Buneman where

import Debug.Trace

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import DFT.Complex
import DFT.Basic

--------------------------------------------------------------------
-- Actual DFT implementations on dense vector representation. These
-- are direct transcriptions from Buneman's TR (with the reshape bug
-- fixed...).


-- | Perform /one/ FFT step with a given radix @m@, compute sub-dfts
-- with the naive DFT.
dftFast :: Integer -> Integer -> Q [Complex] -> Q [Complex]
dftFast m n v = trace ("df" P.++ (show (m, n))) $
  concat [ map sumC $ φ [ [ ω (m * n) (a * (toQ n * d + c)) .* t
                          | (view -> (t, c)) <- number0 $ dft n z
                          ]
                        | (view -> (z, a)) <- number0 $ φ $ ρ m v
                        ]
         | d <- toQ [ 0 .. m - 1 ]
         ] 

-- | Perform /two/ FFT steps with a given radix.
dftFastRec1 :: Integer -> Integer -> Q [Complex] -> Q [Complex]
dftFastRec1 m n v =
  concat [ map sumC $ φ [ [ ω (m * n) (a * (toQ n * d + c)) .* t
                          | (view -> (t, c)) <- number0 $ dftFast m (n `P.div` m) z
                          ]
                        | (view -> (z, a)) <- number0 $ φ $ ρ m v
                        ]
         | d <- toQ [ 0 .. m - 1 ]
         ] 

-- | Employ radix-/m/ FFT steps until each sub-vector is below a
-- certain threshold.
dftFastRec :: Integer -> Integer -> Q [Complex] -> Q [Complex]
dftFastRec m n v = trace ("fr" P.++ show (m, n)) $
  if' (n P.<= m P.|| (n P.* m) P.<= 32)
    (dftFast m n v)
    (concat [ map sumC $ φ [ [ ω (m * n) (a * (toQ n * d + c)) .* t
                             | (view -> (t, c)) <- number0 $ dftFastRec m (n `P.div` m) z
                             ]
                           | (view -> (z, a)) <- number0 $ φ $ ρ m v
                           ]
            | d <- toQ [ 0 .. m - 1 ]
            ])

-- | Perform maxDepth - 1 calls recursive calls to fft, finish of with
-- a dft call.
dftFastRecD :: Integer -> Integer -> Integer -> Q [Complex] -> Q [Complex]
dftFastRecD maxDepth = go 1
            
  where
    go :: Integer -> Integer -> Integer -> Q [Complex] -> Q [Complex]
    go depth m n v =
        if' (depth P.== maxDepth)
            (dft (m P.* n) v)
            (concat [ map sumC $ φ [ [ ω (m * n) (a * (toQ n * d + c)) .* t
                                     | (view -> (t, c)) <- number0 $ go (depth P.+ 1) m (n `P.div` m) z
                                     ]
                                   | (view -> (z, a)) <- number0 $ φ $ ρ m v
                                   ]
                    | d <- toQ [ 0 .. m - 1 ]
                    ])
