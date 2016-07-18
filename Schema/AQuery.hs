{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Schema.AQuery where

import           Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Prelude            as P

import           Database.DSH

data Trade = Trade
    { t_amount    :: Double
    , t_price     :: Double
    , t_tid       :: Integer
    , t_timestamp :: Integer
    , t_tradeDate :: Day
    }

deriveDSH ''Trade
deriveTA ''Trade
generateTableSelectors ''Trade

data Portfolio = Portfolio
    { po_pid         :: Integer
    , po_tid         :: Integer
    , po_tradedSince :: Integer
    }

deriveDSH ''Portfolio
deriveTA ''Portfolio
generateTableSelectors ''Portfolio

trades :: Q [Trade]
trades = table "trades"
               ("amount" :|
                [ "price"
                , "tid"
                , "ts"
                , "tradeDate"])
               (TableHints (pure $ Key ("tid" :| ["ts"])) NonEmpty)

portfolios :: Q [Portfolio]
portfolios = table "portfolio"
                   ("po_pid" :| ["po_tid", "po_tradedSince"])
                   (TableHints (pure $ Key (pure "po_pid")) NonEmpty)

data Packet = Packet
    { p_dest :: Integer
    , p_len  :: Integer
    , p_pid  :: Integer
    , p_src  :: Integer
    , p_ts   :: Integer
    }

deriveDSH ''Packet
deriveTA ''Packet
generateTableSelectors ''Packet

packets :: Q [Packet]
packets = table "packets"
                ("dst" :|
                 [ "len"
                 , "pid"
                 , "src"
                 , "ts"
                 ])
                (TableHints (pure $ Key (pure "pid")) NonEmpty)
