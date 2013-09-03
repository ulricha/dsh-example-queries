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

type Row = [(Integer, Double)]

storage :: Q [(Integer, Integer, Double)]
storage = toQ [ (1,1,10)
              , (1,2,20)
              , (1,3,30)
              , (2,1,40)
              , (2,2,50)
              ]

matrix :: Q [(Integer, Row)]
matrix = let rows = nub $ map (\(view -> (r, _, _)) -> r) storage
         in [ tuple2 r ([ tuple2 c v | (view -> (r', c, v)) <- storage, r == r' ])
            | r <- rows
            ]

getConn :: IO X100Info
getConn = P.return $ x100Info "localhost" "48130" Nothing

runQ :: (Show a,QA a) => Q a -> IO ()
runQ q = getConn P.>>= \conn -> fromQX100 conn q P.>>= P.print

debugQ :: (Show a, QA a) => Q a -> IO ()
debugQ q = getConn P.>>= \conn -> debugX100VL "factor-backtest2" conn q

main :: IO ()
main = debugQ matrix 
