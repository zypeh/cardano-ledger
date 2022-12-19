{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Ledger.Conway.LedgerState.Types
  ( ConwayUTxOState (..),
  )
where

import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Core (Era (..), EraTxOut (..))
import Cardano.Ledger.Shelley.LedgerState (IncrementalStake)
import Cardano.Ledger.UTxO (UTxO (..))
import Data.Default (Default (..))
import GHC.Generics (Generic)

data ConwayUTxOState era = ConwayUTxOState
  { cutxosUtxo :: !(UTxO era),
    cutxosDeposited :: !Coin,
    cutxosFees :: !Coin,
    cutxosStakeDistr :: !(IncrementalStake (EraCrypto era))
  }
  deriving (Generic)

deriving instance (Era era, Eq (TxOut era)) => Eq (ConwayUTxOState era)

instance Era era => Default (ConwayUTxOState era) where
  def =
    ConwayUTxOState
      { cutxosUtxo = mempty,
        cutxosDeposited = mempty,
        cutxosFees = mempty,
        cutxosStakeDistr = mempty
      }
