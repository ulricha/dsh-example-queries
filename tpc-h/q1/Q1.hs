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

import Records

lineitems :: Q [LineItem]
lineitems = table "lineitem"

withFlagStatus :: Q LineItem -> Q (Text, Text)
withFlagStatus li = tuple2 (l_returnflagQ li) (l_linestatusQ li)

filteredItems :: Q [LineItem]
filteredItems = [ li | li <- lineitems , l_shipdateQ li <= 42 ]

fst9 :: (QA a, QA b, QA c, QA d, QA e, QA f, QA g, QA h, QA i) => Q (a, b, c, d, e, f, g, h, i) -> Q a
fst9 (view -> (a, _, _, _, _, _, _, _, _)) = a

q1 :: Q [((Text, Text), Double, Double, Double, Double, Double, Double, Double, Integer)]
q1 = sortWith fst9 $ 
     [ tuple9
	  k
	  (sum $ map l_quantityQ lis)
	  (sum $ map l_extendedpriceQ lis)
	  (sum $ map (\li -> l_extendedpriceQ li * (1 - l_discountQ li)) lis)
	  (sum $ map (\li -> l_extendedpriceQ li * (1 - l_discountQ li) * (1 + l_taxQ li)) lis)
	  (avg $ map l_quantityQ lis)
	  (avg $ map l_extendedpriceQ lis)
	  (avg $ map l_discountQ lis)
	  (length lis)
      | (view -> (k, lis)) <- groupWithKey withFlagStatus filteredItems
      ]

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' port = '5433' dbname = 'tpch'"

-- getConn :: IO X100Info
-- getConn = P.return $ x100Info "localhost" "48130" Nothing

{-
runQ :: (Show a,QA a) => Q a -> IO ()
runQ q = getConn P.>>= \conn -> fromQX100 conn q P.>>= P.print
-}

debugQ :: (Show a, QA a) => Q a -> IO ()
debugQ q = getConn P.>>= \conn -> debugTA "q1" conn q

main :: IO ()
main = debugQ q1
