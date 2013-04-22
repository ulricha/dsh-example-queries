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

myTeam :: Q Text
myTeam = "t1"

otherTeam :: Q Text
otherTeam = "t2"

byEff :: Q Player -> Q Integer
byEff (view -> (_, _, _, _, e)) = e

teamPlayers :: Q Text -> Q [(Text, Integer)]
teamPlayers t = reverse $ sortWith snd [ tuple2 t e
                                         | (view -> (_, t', n, _, e)) <- players
		                         , t == t'
		                         ]

betterPlayer :: Q ((Text, Integer), (Text, Integer)) -> Q Bool
betterPlayer (view -> (p1, p2)) = snd p1 > snd p2

bestPlayers :: Q [((Text, Integer), (Text, Integer))]
bestPlayers = takeWhile betterPlayer $ zip (teamPlayers myTeam) (teamPlayers otherTeam)

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'giorgidz' password = '' host = 'localhost' dbname = 'giorgidz'"

runQ :: (Show a,QA a) => Q a -> IO ()
runQ q = getConn P.>>= \conn -> (fromQ conn q P.>>= P.print) P.>> disconnect conn

main :: IO ()
main = runQ bestPlayers
