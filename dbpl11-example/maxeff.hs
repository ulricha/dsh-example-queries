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

-- (id, team, name, pos, eff)
type Player = (Integer, Text, Text, Text, Integer)

players :: Q [Player]
players = table "players"

byTeam :: Q Player -> Q Text
byTeam (view -> (_, t, _, _, _)) = t

byPos :: Q Player -> Q Text
byPos (view -> (_, _, _, p, _)) = p

onlyEff :: Q Player -> Q Integer
onlyEff (view -> (_, _, _, _, e)) = e

maxEfficiency :: Q [(Text, [(Text, Integer)])]
maxEfficiency = [ tuple2 t 
                         [ tuple2 pos (maximum $ map onlyEff pps)
                         | (view -> (pos, pps)) <- groupWithKey byPos tps
			 ]
                | (view -> (t, tps)) <- groupWithKey byTeam players
		]

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'giorgidz' password = '' host = 'localhost' dbname = 'giorgidz'"

runQ :: (Show a,QA a) => Q a -> IO ()
runQ q = getConn P.>>= \conn -> (fromQ conn q P.>>= P.print) P.>> disconnect conn

main :: IO ()
main = runQ maxEfficiency
