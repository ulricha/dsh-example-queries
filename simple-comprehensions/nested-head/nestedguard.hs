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
xs = toQ [1,2,3,4,5,6,7,8,12]

ys :: Q [Integer]
ys = toQ [2,3,2,4,5,5,9,12,2,2,13]

zs :: Q [(Integer, Integer)]
zs = toQ [(2, 20), (5, 60), (3, 30), (3, 80), (4, 40), (5, 10), (5, 30), (12, 120)]

njg1 :: Q [Integer]
njg1 = 
  [ x
  | x <- xs
  , x < 8
  , sum [ snd z | z <- zs, fst z == x ] > 100
  ]

njg2 :: Q [Integer]
njg2 =
  [ x
  | x <- xs
  , and [ y > 1 | y <- ys, x == y ]
  , x < 8
  ]

njg3 :: Q [(Integer, Integer)]
njg3 =
  [ pair x y
  | x <- xs
  , y <- ys
  , length [ toQ () | z <- zs, fst z == x ] > 2
  ]

njg4 :: Q [Integer]
njg4 =
  [ x
  | x <- xs
  , length [ toQ () | y <- ys, x == y ] 
    > length [ toQ () | z <- zs, fst z == x ]
  ]

getConn :: IO X100Info
getConn = P.return $ x100Info "localhost" "48130" Nothing

runQ :: (Show a,QA a) => Q a -> IO ()
runQ q = getConn P.>>= \conn -> runQX100 conn q P.>>= P.print

main :: IO ()
main = getConn P.>>= \conn -> sequence_ [ debugX100VL "njg1" conn njg1
                                        , debugX100VL "njg2" conn njg2
                                        , debugX100VL "njg3" conn njg3
                                        , debugX100VL "njg4" conn njg4
                                        ]
