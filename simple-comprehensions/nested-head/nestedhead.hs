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

import Database.HDBC.PostgreSQL

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

t1 :: Q [Integer]
t1 = toQ [1,2,3,4,5,6]

t2 :: Q [Integer]
t2 = toQ [3,4,5,6,3,6,4]

nj1 :: Q [[Integer]]
nj1 = [ [ y | y <- t2, x == y ]
    | x <- t1
    ]

nj2 :: Q [(Integer, [Integer])]
nj2 = [ pair x [ y | y <- t2, x == y ]
    | x <- t1
    ]

nj3 :: Q [(Integer, [Integer])]
nj3 = [ pair x ([ y | y <- t2, x == y ] ++ (toQ [100, 200, 300]))
    | x <- t1
    ]

nj4 :: Q [(Integer, [Integer])]
nj4 = [ pair x ([ y | y <- t2, x == y ] ++ [ z | z <- t2, x == z ])
      | x <- t1
      ]

-- Code incurs DistSeg for the literal 15.
nj5 :: Q [(Integer, [Integer])]
nj5 = [ pair x [ y | y <- t2, x + y > 15 ]
      | x <- t1
      ]

nj6 :: Q [(Integer, [Integer])]
nj6 = [ pair x [ y | y <- t2, x + y > 10, y < 7 ]
      | x <- t1
      ]

-- SQL code for outer query has empty SELECT CLAUSE
nj7 :: Q [[Integer]]
nj7 = [ [ x + y | y <- t2, x + 2 == y ] | x <- t1 ]

nj8 :: Q [[Integer]]
nj8 = [ [ x + y | y <- t2, x == y, y < 5 ] | x <- t2, x > 3 ]

nj9 :: Q [[Integer]]
nj9 = [ [ x + y | y <- t2, x + 1 == y, y > 2, x < 6 ] | x <- t1 ]

np1 :: Q [[Integer]]
np1 = [ [ y * 2 | y <- t2 ] | x <- t1 ]
	

np2 :: Q [(Integer, [Integer])]
np2 = [ pair x [ y * 2 | y <- t2 ] | x <- t1 ]

np3 :: Q [[Integer]]
np3 = [ [ x + y | y <- t2 ] | x <- t1 ]

np4 :: Q [[Integer]]
np4 = [ [ y | y <- t2, x > y ] | x <- t1 ]

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' dbname = 'au'"

main :: IO ()
-- main = getConn P.>>= \conn -> runPrint conn nj1
main = getConn P.>>= \conn -> sequence_ [ debugTAOpt "nj1" conn nj1
				        , debugVL "nj2" conn nj2
				        , debugVL "nj3" conn nj3
				        , debugVL "nj4" conn nj4
				        , debugVL "nj5" conn nj5
				        , debugVLOpt "nj6" conn nj6
				        , debugTAOpt "nj7" conn nj7
				        , debugVL "nj8" conn nj8
				        , debugVL "nj9" conn nj9
				        , debugTAOpt "nj7" conn nj7
				        , debugVL "np1" conn np1
				        , debugVL "np2" conn np2
				        , debugVL "np3" conn np3
				        , debugTAOpt "np4" conn np4
				        ]
