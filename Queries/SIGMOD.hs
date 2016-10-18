{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE FlexibleInstances   #-}

-- | A set of DSH examples for the demo session at SIGMOD 2015
--
-- This package contains selected examples that demonstrate various aspects of
-- database queries in DSH. The set of examples was used for the demo session at
-- SIGMOD 15.
--
-- Ulrich, Grust: <http://db.inf.uni-tuebingen.de/publications/TheFlatter-theBetter-QueryCompilationBasedontheFlatteningTransformation.html The Flatter the Better> (SIGMOD'15)
module Queries.SIGMOD
    ( module Queries.SIGMOD.Simple
    , module Queries.SIGMOD.Order
    , module Queries.SIGMOD.Layered
    , module Queries.SIGMOD.Nested
    ) where

import           Queries.SIGMOD.Simple
import           Queries.SIGMOD.Order
import           Queries.SIGMOD.Layered
import           Queries.SIGMOD.Nested
