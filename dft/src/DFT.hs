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

module Main where

import Debug.Trace

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.X100Client

import DFT.Complex
import DFT.Radix

--------------------------------------------------------------------
-- Test vectors

vec1 :: Q [Complex]
vec1 = toQ [ (1.0, 0.0)
           , (0.0, 0.0)
           , (0.0, 0.0)
           , (0.0, 0.0)
           , (0.0, 0.0)
           , (0.0, 0.0)
           , (0.0, 0.0)
           , (0.0, 0.0)
           ]

vec2 :: Q [Complex]
vec2 = toQ [ (0.0, 0.0)
           , (1.0, 0.0)
           , (0.0, 0.0)
           , (0.0, 0.0)
           , (0.0, 0.0)
           , (0.0, 0.0)
           , (0.0, 0.0)
           , (0.0, 0.0)
           ]

vec3 :: Q [Complex]
vec3 = toQ [ (1, 10)
           , (2, 20)
           , (3, 30)
           , (4, 40)
           , (5, 50)
           , (6, 60)
           , (7, 70)
           , (8, 80)
           ]

getConnX100 :: X100Info
getConnX100 = x100Info "localhost" "48130" Nothing

main :: IO ()
main = 
   -- debugQX100 "dftfast_16" getConnX100 (dftFast 32 2048 (vecFromTable $ vec "v1_2_16"))
   -- debugQX100 "dftfastrec1_16" getConnX100 (dftFastRec1 32 2048 (vecFromTable $ vec "v1_2_16"))
   -- debugQX100 "dftfastrec_16" getConnX100 (dftFastRec 2 32768 (vecFromTable $ vec "v1_2_16"))
   debugQX100 "radix2_256" getConnX100 (radix2Fft 128 (vecFromTable $ vec "v1_256"))
