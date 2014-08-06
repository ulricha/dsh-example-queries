module DFT.Support where

import qualified Database.DSH as Q

indexTable :: String -> Q.Q [Integer]
indexTable n = 
    Q.sortWith Q.id $ Q.table n ( Q.defaultHints { Q.keysHint = [Q.Key ["index"]]
                                                 , Q.nonEmptyHint = Q.NonEmpty
                                                 } )

indexes :: Integer -> Q.Q [Integer]
indexes 16384 = indexTable "idx_16384"
indexes 4096  = indexTable "idx_4096"
indexes 1024  = indexTable "idx_1024"
indexes 256   = indexTable "idx_256"
indexes 64    = indexTable "idx_64"
indexes 16    = indexTable "idx_16"
indexes n     = Q.toQ [0..n-1]
