{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Main where

import Debug.Trace

import Database.DSH.Compiler

import Database.X100Client

import DFT.Complex
import DFT.Radix

--------------------------------------------------------------------
-- Test vectors

getConnX100 :: X100Info
getConnX100 = x100Info "localhost" "48130" Nothing

main :: IO ()
main = do
   -- debugQX100 "dftfast_16" getConnX100 (dftFast 32 2048 (vecFromTable $ vec "v1_2_16"))
   -- debugQX100 "dftfastrec1_16" getConnX100 (dftFastRec1 32 2048 (vecFromTable $ vec "v1_2_16"))
   -- debugQX100 "dftfastrec_16" getConnX100 (dftFastRec 2 32768 (vecFromTable $ vec "v1_2_16"))
   -- debugQX100 "radix2_256" getConnX100 (radix2Fft 128 (vecFromTable $ vec "v1_256"))
   debugQX100 "radix4_share_256" getConnX100 (radix4FftShare 64 (vecFromTable $ vec "v1_256"))
