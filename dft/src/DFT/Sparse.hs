module DFT.Sparse where

---------------------------------------------------------------------
-- Sparse vector implementation (unordered)

-- FIXME assume here that groupWith keeps order in partitions stable,
-- i.e. sorts by pos. That might currently not be the case.
-- reshapeSparse :: QA a => Integer -> Q [(Int, a)] -> Q [(Int, [(Int, a)])]
-- reshapeSparse n vec = 
--     -- Generate new indices for the outer vector
--     number 
--     -- Generate new indices for the inner vectors
--     $ map number 
--     -- Throw away old vector indices
--     $ map (map snd) 
--     $ groupWith byPos $ number vec
-- 
--   where
--     byPos :: QA a => Q (Integer, a) -> Q Integer
--     byPos v = ((fst v) - 1) / n

{-

type SparseVector a = [(Integer, a)]

type Row a = (Integer, Integer, a)
 
type SparseMatrix a = [Row a]

row :: QA a => Q (Row a) -> Q Integer
row (view -> (r, _, _)) = r

rowVec :: QA a => Q Integer -> Q (SparseMatrix a) -> Q (SparseVector a)
rowVec r m = [ pair c x | (view -> (r', c, x)) <- m, r == r' ]

reshape2 :: QA a => Integer -> Q (SparseVector a) -> Q (SparseMatrix a)
reshape2 n v = 
  [ tuple3 (p `div` (toQ n)) (p `mod` (toQ n)) x
  | (view -> (p, x)) <- v
  ]

transpose2 :: QA a => Q (SparseMatrix a) -> Q (SparseMatrix a)
transpose2 m =
  [ tuple3 c r x
  | (view -> (r, c, x)) <- m
  ]

dftSparse :: Integer -> Q (SparseVector Double) -> Q (SparseVector Double)
dftSparse dim v = 
  [ pair i (sum $ map snd xs)
  | (view -> (i, xs)) <- groupWithKey fst 
                                   [ pair i (ω dim (i * j) * x)
                                   | i <- (map fst v)
                                   , (view -> (j, x)) <- v
                                   ]
  ]

-- dftFastSparse :: Integer -> Integer -> Q (SparseVector Double) -> Q (SparseVector Double)
-- dftFastSparse m n v
--   [ [ 
--     | (view -> (t, c)) <- dftSparse $ dftrowVec a w
--     ]
--   | d <- toQ [ 0 .. m - 1 ]
--   , let w = transpose2 $ reshape2 n v
--   , a <- nub $ map row w
--   ]

-}
