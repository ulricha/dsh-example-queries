{-# LANGUAGE OverloadedStrings, MonadComprehensions, RebindableSyntax, ViewPatterns #-}

module Main where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler
import Database.HDBC.PostgreSQL

import Debug.Trace

-- Testcases for simple flat joins on scalar values       

xs :: Q [Integer]
xs = toQ [1,2,3,4,5,6]

ys :: Q [Integer]
ys = toQ [2,3,4,5,8,10]

zs :: Q [Integer]
zs = toQ [4, 5, 7, 9, 3]

jf1 :: Q [(Integer, Integer)]
jf1 = [ tuple2 x y
      | x <- xs
      , y <- ys
      , y == x
      ]

jf2 :: Q [(Integer, Integer)]
jf2 = [ tuple2 x y
      | x <- xs
      , y <- ys
      , x == y
      ]

jf3 :: Q [(Integer, Integer)]
jf3 = [ tuple2 x z
      | x <- xs
      , z <- zs
      , z == x
      ]

jf4 :: Q [(Integer, Integer, Integer)]
jf4 = [ tuple3 x y z
      | x <- xs
      , y <- ys
      , x == y
      , z <- zs
      , y == z
      ]

jf5 :: Q [(Integer, Integer, Integer)]
jf5 = [ tuple3 x y z
      | x <- xs
      , y <- ys
      , z <- xs
      , x == y
      , y == z
      ]

jf6 :: Q [(Integer, Integer, Integer)]
jf6 = [ tuple3 x y z
      | x <- xs
      , y <- ys
      , y == x
      , z <- zs
      ]

jf7 :: Q [(Integer, Integer, Integer)]
jf7 = [ tuple3 x y z
      | x <- xs
      , y <- ys
      , y == x
      , z <- zs
      , x > 2
      , y <= 4
      ]

jf8 :: Q [(Integer, Integer, Integer)]
jf8 = [ tuple3 x y z
      | x <- xs
      , y <- ys
      , y == x
      , z <- zs
      , y == z
      , x > 3
      ]

jf9 :: Q [(Integer, Integer)]
jf9 = [ tuple2 x y
      | x <- xs
      , y <- ys
      , x * y > x + 2
      , x == y
      ]

jf10 :: Q [(Integer, Integer)]
jf10 = [ tuple2 x y
       | x <- xs
       , x > 3
       , y <- ys
       , x == y
       ]

jf11 :: Q [(Integer, Integer)]
jf11 = [ tuple2 x y
       | x <- xs
       , y <- ys
       , x == y
       ]

jf12 :: Q [Integer]
jf12 = [ x + y
       | x <- xs
       , y <- ys
       , x == y
       ]

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' dbname = 'au'"

main :: IO ()
main = getConn P.>>= \conn -> sequence_ [ debugVL "jf1" conn jf1
                                        , debugVL "jf2" conn jf2
                                        , debugVL "jf3" conn jf3
                                        , debugVL "jf4" conn jf2
                                        , debugVL "jf5" conn jf5
                                        , debugVL "jf6" conn jf6
                                        , debugVL "jf7" conn jf7
                                        , debugVL "jf8" conn jf8
                                        , debugVL "jf9" conn jf9
                                        , debugVL "jf10" conn jf10
                                        , debugVL "jf11" conn jf11
                                        , debugVL "jf12" conn jf12
				        ]
