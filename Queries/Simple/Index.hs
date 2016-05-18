{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ViewPatterns        #-}

module Queries.Simple.Index
    ( liftIndex
    , liftIndexes
    , takeWhileSmaller
    ) where

import Database.DSH

liftIndex :: Q [Integer]
liftIndex = [ xs !! i | i <- is ]
  where
    xs = toQ [1..10]
    is = toQ [5,3,6,3,0,0,5]

liftIndexes :: Q [[Char]]
liftIndexes = [ [ xs !! i | i <- is ]
              | (view -> (xs, is)) <- zip xss iss
              ]
  where
    xss = toQ [['A', 'B'], ['C', 'D', 'E'], ['F', 'G'], ['H']]
    iss = toQ [[1,0,1],[2],[1,0],[0]]

takeWhileSmaller :: Q [Integer]
takeWhileSmaller = takeWhile (<10) (toQ [1..20])

