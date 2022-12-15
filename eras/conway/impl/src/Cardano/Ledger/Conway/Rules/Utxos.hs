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
import Cardano.Ledger.Conway.PParams (BabbagePParamsUpdate)
import Cardano.Ledger.Conway.TxOut (BabbageTxOut (..))
import Cardano.Ledger.Core (Era (..), EraPParams (..), EraRule, EraScript (..), EraTxOut (..), Value)
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Rules.ValidationMode (Inject)
import Cardano.Ledger.Shelley.LedgerState (ShelleyPPUPState (..), ShelleyUTxOState (..), PPUPState)
import Cardano.Ledger.Shelley.Rules (ShelleyPpupPredFailure, UtxoEnv (..))
import Control.State.Transition.Extended (STS (..))

instance
  ( EraScript era,
    PredicateFailure (EraRule "PPUP" era) ~ ShelleyPpupPredFailure era,
    TxOut era ~ BabbageTxOut era,
    Value era ~ MaryValue (EraCrypto era),
    Script era ~ AlonzoScript era,
    PPUPState era ~ ShelleyPPUPState era,
    PParamsUpdate era ~ BabbagePParamsUpdate era,
    Inject (PredicateFailure (EraRule "PPUP" era)) (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  STS (ConwayUTXOS era)
  where
  type BaseM (ConwayUTXOS era) = ShelleyBase
  type Environment (ConwayUTXOS era) = UtxoEnv era
  type State (ConwayUTXOS era) = ShelleyUTxOState era
  type Signal (ConwayUTXOS era) = AlonzoTx era
  type PredicateFailure (ConwayUTXOS era) = AlonzoUtxosPredFailure era
  type Event (ConwayUTXOS era) = AlonzoUtxosEvent era

  transitionRules = []
