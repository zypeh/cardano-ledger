{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Conway.Era
  ( ConwayEra,
  )
where

import Cardano.Ledger.Alonzo.Rules (AlonzoBBODY)
import Cardano.Ledger.Babbage.Rules (BabbageLEDGER, BabbageUTXO, BabbageUTXOS, BabbageUTXOW)
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Mary.Value (MaryValue)
import qualified Cardano.Ledger.Shelley.API as API
import Cardano.Ledger.Shelley.Rules.Epoch (ShelleyEPOCH)
import Cardano.Ledger.Shelley.Rules.Mir (ShelleyMIR)
import Cardano.Ledger.Shelley.Rules.Newpp (ShelleyNEWPP)
import Cardano.Ledger.Shelley.Rules.Rupd (ShelleyRUPD)
import Cardano.Ledger.Shelley.Rules.Snap (ShelleySNAP)
import Cardano.Ledger.Shelley.Rules.Tick
  ( ShelleyTICK,
    ShelleyTICKF,
  )
import Cardano.Ledger.Shelley.Rules.Upec (ShelleyUPEC)

-- =====================================================

-- | The Babbage era
data ConwayEra c

instance CC.Crypto c => Era (ConwayEra c) where
  type Crypto (ConwayEra c) = c
  type ProtVerLow (ConwayEra c) = 8

type instance Value (ConwayEra c) = MaryValue c

-------------------------------------------------------------------------------
-- Era Mapping
-------------------------------------------------------------------------------

-- Rules inherited from Babbage

type instance EraRule "UTXOS" (ConwayEra c) = BabbageUTXOS (ConwayEra c)

type instance EraRule "UTXO" (ConwayEra c) = BabbageUTXO (ConwayEra c)

type instance EraRule "UTXOW" (ConwayEra c) = BabbageUTXOW (ConwayEra c)

type instance EraRule "LEDGER" (ConwayEra c) = BabbageLEDGER (ConwayEra c)

-- Rules inherited from Alonzo

type instance EraRule "BBODY" (ConwayEra c) = AlonzoBBODY (ConwayEra c)

-- Rules inherited from Shelley

type instance EraRule "DELEG" (ConwayEra c) = API.ShelleyDELEG (ConwayEra c)

type instance EraRule "DELEGS" (ConwayEra c) = API.ShelleyDELEGS (ConwayEra c)

type instance EraRule "DELPL" (ConwayEra c) = API.ShelleyDELPL (ConwayEra c)

type instance EraRule "EPOCH" (ConwayEra c) = ShelleyEPOCH (ConwayEra c)

type instance EraRule "LEDGERS" (ConwayEra c) = API.ShelleyLEDGERS (ConwayEra c)

type instance EraRule "MIR" (ConwayEra c) = ShelleyMIR (ConwayEra c)

type instance EraRule "NEWEPOCH" (ConwayEra c) = API.ShelleyNEWEPOCH (ConwayEra c)

type instance EraRule "NEWPP" (ConwayEra c) = ShelleyNEWPP (ConwayEra c)

type instance EraRule "POOL" (ConwayEra c) = API.ShelleyPOOL (ConwayEra c)

type instance EraRule "POOLREAP" (ConwayEra c) = API.ShelleyPOOLREAP (ConwayEra c)

type instance EraRule "PPUP" (ConwayEra c) = API.ShelleyPPUP (ConwayEra c)

type instance EraRule "RUPD" (ConwayEra c) = ShelleyRUPD (ConwayEra c)

type instance EraRule "SNAP" (ConwayEra c) = ShelleySNAP (ConwayEra c)

type instance EraRule "TICK" (ConwayEra c) = ShelleyTICK (ConwayEra c)

type instance EraRule "TICKF" (ConwayEra c) = ShelleyTICKF (ConwayEra c)

type instance EraRule "UPEC" (ConwayEra c) = ShelleyUPEC (ConwayEra c)

-- =================================================
