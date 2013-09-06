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

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.X100Client

xs :: Q [Integer]
xs = toQ [1, 2, 3, 4, 5, 6, 7]

ys :: Q [Integer]
ys = toQ [2, 4, 6]

q :: Q [Integer]
q = [ x | x <- xs, null [ toQ () | y <- ys, x == y ]

getConn :: IO X100Info
getConn = P.return $ x100Info "localhost" "48130" Nothing

runQ :: (Show a,QA a) => Q a -> IO ()
runQ q = getConn P.>>= \conn -> fromQX100 conn q P.>>= P.print

debugQ :: (Show a, QA a) => Q a -> IO ()
debugQ q = getConn P.>>= \conn -> debugX100VL "existential-elem" conn q

main :: IO ()
main = debugQ q
