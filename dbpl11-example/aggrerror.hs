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
-- type Player = (Integer, Text, Text, Text, Integer)

data Player = Player { id   :: Integer
                     , team :: Text
		     , name :: Text
		     , pos  :: Text
		     , eff  :: Integer
                     }
		     deriving (Show)

deriveDSH ''Player
deriveTA ''Player

playerTeamQ :: Q Player -> Q Text
playerTeamQ (view -> (_, t, _, _, _)) = t

playerEffQ :: Q Player -> Q Integer
playerEffQ (view -> (_, _, _, _, e)) = e

players :: Q [Player]
players = table "players"

data PCat = Center Text Text Integer
          | Regular Text Text
	  deriving (Show)

deriveDSH ''PCat

data Team = ElitePlayers [Player]
          | EmptyTeam
	  deriving (Show)

deriveDSH ''Team

myTeam :: Q Text
myTeam = "T1"

playersByEff :: Q Text -> Q Integer -> Q [Player]
playersByEff t e = [ p
                   | p <- players
		   , playerTeamQ p == t
		   , playerEffQ p == e
		   ]

safeMaximum :: (Ord a, QA a) => Q [a] -> Q (Maybe a)
safeMaximum as = if null as then nothing else just (maximum as)

bestPlayers :: Q Team
bestPlayers =
    let es = [ playerEffQ p | p <- players, playerTeamQ p == myTeam ]
    in elim (safeMaximum es)
            emptyTeam
	    (\maxEff -> elitePlayers $ playersByEff myTeam maxEff)

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'giorgidz' password = '' host = 'localhost' dbname = 'giorgidz'"

runQ :: (Show a,QA a) => Q a -> IO ()
runQ q = getConn P.>>= \conn -> (fromQ conn q P.>>= P.print) P.>> disconnect conn

main :: IO ()
main = runQ bestPlayers
