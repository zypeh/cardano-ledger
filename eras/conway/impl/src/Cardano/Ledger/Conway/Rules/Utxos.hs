{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Utxos (ConwayUTXOS) where

import Cardano.Ledger.Alonzo.Rules (AlonzoUtxosEvent, AlonzoUtxosPredFailure)
import Cardano.Ledger.Alonzo.Scripts (AlonzoScript)
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Conway.Era (ConwayUTXOS)
import Cardano.Ledger.Conway.TxOut (BabbageTxOut (..))
import Cardano.Ledger.Core (Era (..), EraScript (..), EraTxOut (..), Value)
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Shelley.LedgerState (PPUPPredFailure, PPUPState, UTxOState)
import Cardano.Ledger.Shelley.Rules (UtxoEnv (..))
import Control.State.Transition.Extended (STS (..))
import Data.Default (Default)

instance
  ( EraScript era,
    Eq (PPUPState era),
    Show (PPUPState era),
    TxOut era ~ BabbageTxOut era,
    Value era ~ MaryValue (EraCrypto era),
    Script era ~ AlonzoScript era,
    Eq (PPUPPredFailure era),
    Show (PPUPPredFailure era),
    Default (PPUPState era)
  ) =>
  STS (ConwayUTXOS era)
  where
  type BaseM (ConwayUTXOS era) = ShelleyBase
  type Environment (ConwayUTXOS era) = UtxoEnv era
  type State (ConwayUTXOS era) = UTxOState era
  type Signal (ConwayUTXOS era) = AlonzoTx era
  type PredicateFailure (ConwayUTXOS era) = AlonzoUtxosPredFailure era
  type Event (ConwayUTXOS era) = AlonzoUtxosEvent era

  transitionRules = []
