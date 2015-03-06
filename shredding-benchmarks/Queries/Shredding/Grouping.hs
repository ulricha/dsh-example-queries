{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ViewPatterns        #-}

module Queries.Shredding.Grouping where

import           Database.DSH
import qualified Prelude          as P

import           Schema.Shredding

q1g :: Q [(Text, Integer)]
q1g = [ pair k (length g) | (view -> (k, g)) <- groupWithKey t_empQ tasks ]

-- not equivalent to q1!
q2g :: Q [(Text, Integer)]
q2g = [ pair (t_empQ t)
             (length [ (toQ ()) | t' <- tasks, t_empQ t' == t_empQ t ])
      | t <- tasks ]
