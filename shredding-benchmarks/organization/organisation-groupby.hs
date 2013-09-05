{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

    
import           Data.Text hiding (all, singleton, length)

import qualified Prelude as P
import           Database.DSH
       
import           Records

q1 :: Q [(Text, Integer)]
q1 = [ pair k (length g) | (view -> (k, g)) <- groupWithKey t_empQ tasks ]

-- not equivalent to q1!
q2 :: Q [(Text, Integer)]
q2 = [ pair (t_empQ t) 
            (length [ (toQ ()) | t' <- tasks, t_empQ t' == t_empQ t ])
     | t <- tasks ]
