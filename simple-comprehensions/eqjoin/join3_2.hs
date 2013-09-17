{-# LANGUAGE OverloadedStrings, MonadComprehensions, RebindableSyntax, ViewPatterns #-}

module Main where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler
import Database.X100Client

import Debug.Trace

q :: Q [(Integer, Integer, Integer)]
q = [ tuple3 x y z
    | x <- (toQ [1,2,3,4,5,6])
    , y <- (toQ [10, 20, 30])
    , z <- (toQ [100, 200, 300])
    , x == y
    , y == z
    ]

  
getConn :: IO X100Info
getConn = P.return $ x100Info "localhost" "48130" Nothing

main :: IO ()
main = getConn 
       -- P.>>= (\conn -> fromQX100 conn q P.>>= (\i -> putStrLn $ show i))
       P.>>= (\conn -> debugX100VL "join3" conn q)
