{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Queries with nested results over the TPC-H schema.
module Queries.TPCH.NonStandard.Flat where

import           Data.Time.Calendar

import           Data.List.NonEmpty          (NonEmpty ((:|)))

import           Database.DSH
import           Queries.TPCH.BuildingBlocks
import           Schema.TPCH

--------------------------------------------------------------------------------

sq1 :: Q [Supplier]
sq1 = filter (\s -> s_acctbalQ s > 9900.0) suppliers

sq2 :: Q [Text]
sq2 = map s_nameQ $ filter (\s -> s_acctbalQ s > 9900.0) suppliers

sq3 :: Q [Text]
sq3 = [ s_nameQ s | s <- suppliers, s_acctbalQ s > 9900.0 ]

sq4 :: Q [(Text, Integer)]
sq4 = [ tup2 (r_nameQ r) (length [ n_nameQ n | n <- nations, n_regionkeyQ n == r_regionkeyQ r ])
      | r <- regions
      ]

sq5 :: Q [(Text, [Text])]
sq5 = [ tup2 (r_nameQ r) [ n_nameQ n | n <- nations, n_regionkeyQ n == r_regionkeyQ r ]
      | r <- regions
      ]

sq6 :: Q [(Text, [Text])]
sq6 = [ tup2 (r_nameQ r)
             (sortWith id [ n_nameQ n | n <- nations, n_regionkeyQ n == r_regionkeyQ r ])
      | r <- regions
      ]

sq7 :: Q [(Integer, Decimal)]
sq7 = groupAggr s_nationkeyQ s_acctbalQ avg $ filter (\s -> s_acctbalQ s > 9900.0) suppliers

sq8 :: Q [(Integer, Decimal)]
sq8 = [ tup2 k (avg [ s_acctbalQ s | s <- g ])
      | (view -> (k, g)) <- groupWithKey s_nationkeyQ $ filter (\s -> s_acctbalQ s > 9900.0) suppliers
      ]

sq9 :: Q [(Integer, [Decimal])]
sq9 = [ tup2 k [ s_acctbalQ s | s <- g ]
      | (view -> (k, g)) <- groupWithKey s_nationkeyQ $ filter (\s -> s_acctbalQ s > 9900.0) suppliers
      ]

--------------------------------------------------------------------------------

suppliersForPart :: Text -> Q Part -> Q [PartSupp]
suppliersForPart r p =
    [ ps
    | (view -> (s, ps)) <- partSuppliers p
    , n <- nations
    , n_nationkeyQ n == s_nationkeyQ s
    , fromRegion (n_nationkeyQ n) r
    ]

--------------------------------------------------------------------------------

-- This is query Q2 from the paper 'Unnesting Arbitrary Queries' by Neumann and
-- Kemper (Proc. BTW, 2015)
minSuppliers :: Q [(Supplier, Nation)]
minSuppliers =
    [ tup2 s n
    | p <- parts
    , p_sizeQ p == 15
    , p_typeQ p `like` "%BRASS"
    , (view -> (s, ps)) <- partSuppliers p
    , n <- nations
    , n_nationkeyQ n == s_nationkeyQ s
    , fromRegion (n_nationkeyQ n) "EUROPE"
    , (ps_supplycostQ ps == minimum (map ps_supplycostQ $ suppliersForPart "EUROPE" p))
      || ps_availqtyQ ps > 2000
    ]

--------------------------------------------------------------------------------

-- Example 4.1.1 from the paper 'Optimization of Nested Queries Using the NF^2
-- Algebra' by Hölsch, Grossniklaus and Scholl (Proc. SIGMOD, 2016).
indirectCorr :: Q [(Text, Text)]
indirectCorr =
    [ tup2 (p_nameQ p) (s_nameQ s)
    | p <- parts
    , s <- suppliers
    , p_partkeyQ p `elem` [ ps_partkeyQ ps
                          | ps <- partsupps
                          , ps_suppkeyQ ps == s_suppkeyQ s
                          ]
    ]

--------------------------------------------------------------------------------
-- Benchmark queries from the paper 'Optimization of Nested Queries Using the NF^2
-- Algebra' by Hölsch, Grossniklaus and Scholl (Proc. SIGMOD, 2016).

nf2Qa :: Q [(Text, Text)]
nf2Qa =
    [ tup2 (p_nameQ p) (s_nameQ s)
    | p <- parts
    , s <- suppliers
    , supplierFromNations (filter (`inRegion` "AMERICA") nations) s
    , p_partkeyQ p `elem` [ ps_partkeyQ ps
                          | ps <- partsupps
                          , ps_suppkeyQ ps == s_suppkeyQ s
                          ]
    ]

partsFromRegion :: Text -> Q [Part]
partsFromRegion regionName =
    [ p
    | p <- parts
    , (view -> (s, _)) <- partSuppliers p
    , supplierFromNations (filter (`inRegion` regionName) nations) s
    ]

nf2Qb :: Q [Text]
nf2Qb = [ p_nameQ p
        | p <- partsFromRegion "ASIA"
        , p_retailpriceQ p > avg [ p_retailpriceQ p'
                                 | p' <- partsFromRegion "AMERICA"
                                 , p_typeQ p == p_typeQ p'
                                 ]
        ]

nf2Qc :: Q (Decimal, Decimal)
nf2Qc = tup2 (maximum prices) (minimum prices)
  where
    prices = map o_totalpriceQ orders

nf2Qd :: Q [(Integer, Decimal, Decimal)]
nf2Qd = [ tup3 ok m1 m2
        | (view -> (ok, m1)) <- groupAggr o_custkeyQ o_totalpriceQ maximum orders
        , (view -> (ok', m2)) <- groupAggr o_custkeyQ o_totalpriceQ minimum orders
        , ok == ok'
        ]

--------------------------------------------------------------------------------

r1 :: Q [(Integer, Integer, Integer, Integer)]
r1 = table "r1"
           ("a" :| ["b", "c", "d"])
           (defaultHints $ pure $ Key (pure "a"))

r2 :: Q [(Integer, Integer, Integer, Integer, Integer)]
r2 = table "r2"
           ("e" :| ["f", "g", "h", "i"])
           (defaultHints $ pure $ Key (pure "e"))

r3 :: Q [(Integer, Integer, Integer)]
r3 = table "r3"
           ("j" :| ["k", "l"])
           (defaultHints $ pure $ Key (pure "j"))

-- Example query Q1 from the paper 'SQL Query Optimization through Nested
-- Relational Algebra' by Cao and Badia (ACM TODS, 2007).
crossLevelCorr :: Q [(Integer, Integer, Integer)]
crossLevelCorr =
    [ tup3 b c d
    | (view -> (a, b, c, d)) <- r1
    , a > 10
    , b `notElem` [ e
                  | (view -> (e, f, g, h, i)) <- r2
                  , f == 5
                  , g == d
                  , and [ h > j
                        | (view -> (j, k, l)) <- r3
                        , k == c
                        , l /= i
                        ]
                  ]
    ]

--------------------------------------------------------------------------------
ws :: Q [Integer]
ws = table "ws" ("w" :| []) (defaultHints $ pure $ Key (pure "w"))

xs :: Q [Integer]
xs = table "xs" ("x" :| []) (defaultHints $ pure $ Key (pure "x"))

ys :: Q [Integer]
ys = table "ys" ("y" :| []) (defaultHints $ pure $ Key (pure "y"))

zs :: Q [Integer]
zs = table "zs" ("z" :| []) (defaultHints $ pure $ Key (pure "z"))

depthThree :: Q [[[Integer]]]
depthThree =
    [ [ [ x + y + z | z <- zs, y == z ]
      | y <- ys
      , x == y
      ]
    | x <- xs
    ]

depthFour :: Q [[[[Integer]]]]
depthFour =
    [ [ [ [ w + x + y + z | z <- zs, y == z ]
        | y <- ys
        , x == y
        ]
      | x <- xs
      , w == x
      ]
    | w <- ws
    ]

--------------------------------------------------------------------------------

indirectCrossLevelCorr :: Q [[Integer]]
indirectCrossLevelCorr =
    [ [ y | y <- ys, y > sum [ z | z <- zs, x == z, y == z ] ]
    | x <- xs
    ]

indirectCrossLevelCorrEasy :: Q [[Integer]]
indirectCrossLevelCorrEasy =
    [ [ y | y <- ys, y > sum [ z | z <- zs, x == z ] ]
    | x <- xs
    ]

--------------------------------------------------------------------------------


foo :: Q [((Integer, [Integer]), (Integer, [Integer]))]
foo = [ tup2 u v | u <- us, v <- vs, fst u == fst v ]
  where
    us = toQ [(1,[1..5]), (2, [3..6]),(3,[8..12])]
    vs = toQ [(2,[1..5]), (3, [3..6]), (5,[6..9])]

q = take 10 $ filter ((> 42) . length . snd) $ groupWithKey p_typeQ parts

topKs :: Q [[Integer]]
topKs = [ take k xs | k <- ks ]
  where
    xs = toQ [1..10]
    ks = toQ [3..7]

topKss :: Q [[[Integer]]]
topKss = [ [ take k xs | k <- ks ] | ks <- map snd kss ]
  where
    xs = toQ [1..10]
    kss = toQ [('a', [1..3]), ('b', [2..5])]

takedrop :: Q [[Integer]]
takedrop = [ take k2 $ drop k1 xs | (view -> (k1, k2)) <- ks]
  where
    xs = toQ [1..10]
    ks = toQ ([(3,5),(2,4),(8,1)] :: [(Integer, Integer)])


--------------------------------------------------------------------------------

broken :: Q [(Integer, Integer)]
broken = [ pair x y | x <- toQ [0,0], y <- toQ [0,1] ]

broken' :: Q [(Integer, Integer)]
broken' = [ pair x y | x <- fst args, y <- snd args ]
  where
    args = toQ ([0,0],[0,1])

broken3 :: Q [(Integer, Integer, Integer)]
broken3 =
    [ tup3 x
           (sum [ 2 * y | y <- njys, x == y ])
           (length [ y | y <- njys, x == y ])
    | x <- njxs
    , 20 < sum [ 3 + y | y <- njys, x == y ]
    ]
  where
    (view -> (njxs, njys)) = toQ ([1], [-2])

--------------------------------------------------------------------------------
-- Moritz

nestedCartProd :: Q [(Integer, [Decimal])]
nestedCartProd =
    zipWith f xs xs
  where
    xs :: Q [(Integer, [Decimal])]
    xs = [ tup2 (o_orderkeyQ o) [ l_discountQ l
                                | l <- lineitems
                                , l_shipdateQ l > toQ date
                                , l_orderkeyQ l == o_orderkeyQ o
                                ]
         | o <- orders
         ]
    date = fromGregorian 1997 1 1

    f :: Q (Integer, [Decimal]) -> Q (Integer, [Decimal]) -> Q (Integer, [Decimal])
    f (view -> (o, as)) (view -> (_, bs)) = tup2 o
        [ a - b
        | a <- as
        , b <- bs
        ]

nestedAppendEx1 :: Q [Decimal]
nestedAppendEx1 = filter (> 90) $ map sum
                                $ zipWith3 (\a b c -> a ++ b ++ c) xs xs' xs''
  where
    xs   = [ singleton (l_quantityQ li)
           | li <- lineitems
           , l_quantityQ li > 30
           ]
    xs'  = tail xs
    xs'' = tail xs'
