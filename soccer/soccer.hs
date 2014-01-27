{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

    
import           Data.Text(Text)

import           Database.DSH
import           Database.DSH.Compiler
import qualified Prelude as P
       
import           Database.HDBC
import           Database.HDBC.PostgreSQL
       
data Team = Team
  { t_city :: Text
  , t_team :: Text
  } deriving (Eq, Ord)

deriveDSH ''Team
deriveTA ''Team
generateTableSelectors ''Team

teams :: Q [Team]
teams = tableWithKeys "teams" [["team"]]

data Match = Match
  { m_goals  :: Text
  , m_opp    :: Text
  , m_points :: Integer
  , m_team   :: Text
  }

deriveDSH ''Match
deriveTA ''Match
generateTableSelectors ''Match

matches :: Q [Match]
matches = tableWithKeys "matches" [["team", "opp"]]

leagueView :: Q [(Team, [(Text, Integer, Text)])]
leagueView =
  [ pair t [ tuple3 (m_oppQ m) (m_pointsQ m) (m_goalsQ m)
           | m <- matches
           , m_teamQ m == t_teamQ t
           ]
  | t <- teams
  ]

finals :: Q [(Team, Integer)]
finals =
  [ pair t (sum [ p | (view -> (_, p, _)) <- ms ])
  | (view -> (t, ms)) <- leagueView
  ]

finalsFlat :: Q [(Team, Integer)]
finalsFlat = 
  let grouped = groupWithKey fst
                $ [ pair t (m_pointsQ m)
                | t <- teams 
                , m <- matches 
                , t_teamQ t == m_teamQ m
                ]
  in [ pair t (sum $ map snd gs)
     | (view -> (t, gs)) <- grouped
     ]
  
getPGConn :: IO Connection
getPGConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' dbname = 'au'"

someQuery :: IO ()
someQuery = getPGConn P.>>= (\conn -> debugTAOpt "finalsFlat" conn finalsFlat)

main :: IO ()
main = someQuery
