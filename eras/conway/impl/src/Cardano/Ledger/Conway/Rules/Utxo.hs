{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Utxo
  ( ConwayUTXO,
    ConwayUTxOState,
  )
where

import qualified Cardano.Ledger.Allegra.Rules as Allegra
import Cardano.Ledger.Alonzo.Rules
  ( AlonzoUtxoEvent (..),
    AlonzoUtxoPredFailure (..),
    AlonzoUtxosPredFailure,
    AlonzoUtxowEvent (..),
    validateExUnitsTooBigUTxO,
    validateOutsideForecast,
    validateTooManyCollateralInputs,
    validateWrongNetworkInTxBody, AlonzoUtxosEvent,
  )
import Cardano.Ledger.Alonzo.Scripts (ExUnits (..), AlonzoScript)
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoEraTxWits, AlonzoTxWits (..))
import Cardano.Ledger.Babbage (BabbageTxOut)
import Cardano.Ledger.Babbage.Core (BabbageEraTxBody (..), EraTxBody (..))
import Cardano.Ledger.Babbage.Rules
  ( BabbageUTXOW,
    BabbageUtxoPredFailure (..),
    BabbageUtxowPredFailure (UtxoFailure),
    feesOK,
    validateOutputTooBigUTxO,
    validateOutputTooSmallUTxO,
  )
import Cardano.Ledger.BaseTypes (Globals (..), ShelleyBase)
import Cardano.Ledger.Binary (Sized (..))
import Cardano.Ledger.Conway.Core (ConwayEraTxBody)
import Cardano.Ledger.Conway.Era (ConwayUTXO, ConwayUTXOS)
import Cardano.Ledger.Conway.LedgerState.Types (ConwayUTxOState (..))
import Cardano.Ledger.Conway.Rules.Utxos ()
import Cardano.Ledger.Conway.TxBody (ConwayTxBody (..))
import Cardano.Ledger.Core
  ( EraPParams (..),
    EraRule,
    EraScript (..),
    EraTx (Tx),
    EraTxOut (..),
    EraTxWits (..),
    Value,
  )
import Cardano.Ledger.Era (Era (..))
import Cardano.Ledger.Mary.Value (MaryValue (..))
import Cardano.Ledger.Rules.ValidationMode (runTest, runTestOnSignal, Inject)
import Cardano.Ledger.Shelley.API (Coin (..))
import Cardano.Ledger.Shelley.LedgerState (PPUPPredFailure, PPUPState)
import Cardano.Ledger.Shelley.Rules
  ( ShelleyUtxowEvent (UtxoEvent),
    UtxoEnv (..),
  )
import qualified Cardano.Ledger.Shelley.Rules as Shelley
import Cardano.Ledger.UTxO (EraUTxO)
import Control.Monad.Trans.Reader (asks)
import Control.State.Transition.Extended
  ( Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    liftSTS,
    trans,
  )
import Data.Coerce (coerce)
import Data.Foldable (Foldable (..))
import GHC.Records (HasField)
import Lens.Micro ((^.))
import Numeric.Natural (Natural)

instance
  ( EraTx era,
    EraUTxO era,
    ConwayEraTxBody era,
    AlonzoEraTxWits era,
    Value era ~ MaryValue (EraCrypto era),
    PredicateFailure (EraRule "UTXOS" era) ~ AlonzoUtxosPredFailure era,
    PredicateFailure (EraRule "UTXO" era) ~ BabbageUtxoPredFailure era,
    Environment (EraRule "UTXOS" era) ~ UtxoEnv era,
    TxOut era ~ BabbageTxOut era,
    Tx era ~ AlonzoTx era,
    Signal (EraRule "UTXOS" era) ~ AlonzoTx era,
    TxBody era ~ ConwayTxBody era,
    TxWits era ~ AlonzoTxWits era,
    HasField "_keyDeposit" (PParams era) Coin,
    HasField "_maxTxSize" (PParams era) Natural,
    HasField "_poolDeposit" (PParams era) Coin,
    HasField "_collateralPercentage" (PParams era) Natural,
    HasField "_maxCollateralInputs" (PParams era) Natural,
    HasField "_maxTxExUnits" (PParams era) ExUnits,
    HasField "_maxValSize" (PParams era) Natural,
    Eq (PPUPState era),
    Show (PPUPState era),
    Eq (PPUPPredFailure era),
    Show (PPUPPredFailure era),
    Embed (EraRule "UTXOS" era) (ConwayUTXO era),
    State (EraRule "UTXOS" era) ~ ConwayUTxOState era,
    Inject (PPUPPredFailure era) (AlonzoUtxosPredFailure era)
  ) =>
  STS (ConwayUTXO era)
  where
  type State (ConwayUTXO era) = ConwayUTxOState era
  type Signal (ConwayUTXO era) = AlonzoTx era
  type Environment (ConwayUTXO era) = UtxoEnv era
  type BaseM (ConwayUTXO era) = ShelleyBase
  type PredicateFailure (ConwayUTXO era) = BabbageUtxoPredFailure era
  type Event (ConwayUTXO era) = AlonzoUtxoEvent era

  initialRules = []
  transitionRules = [utxoTransition]

-- | The UTxO transition rule for the Babbage eras.
utxoTransition ::
  forall era.
  ( EraTx era,
    EraUTxO era,
    ConwayEraTxBody era,
    AlonzoEraTxWits era,
    Tx era ~ AlonzoTx era,
    TxBody era ~ ConwayTxBody era,
    TxOut era ~ BabbageTxOut era,
    STS (ConwayUTXO era),
    HasField "_maxTxSize" (PParams era) Natural,
    HasField "_maxValSize" (PParams era) Natural,
    HasField "_maxCollateralInputs" (PParams era) Natural,
    HasField "_maxTxExUnits" (PParams era) ExUnits,
    HasField "_collateralPercentage" (PParams era) Natural,
    HasField "_keyDeposit" (PParams era) Coin,
    HasField "_poolDeposit" (PParams era) Coin,
    -- In this function we we call the UTXOS rule, so we need some assumptions
    Embed (EraRule "UTXOS" era) (ConwayUTXO era),
    Environment (EraRule "UTXOS" era) ~ UtxoEnv era,
    Signal (EraRule "UTXOS" era) ~ Tx era,
    State (EraRule "UTXOS" era) ~ ConwayUTxOState era,
    Inject (PPUPPredFailure era) (PredicateFailure (EraRule "UTXOS" era))
  ) =>
  TransitionRule (ConwayUTXO era)
utxoTransition = do
  TRC (Shelley.UtxoEnv slot pp dpstate _genDelegs, u, tx) <- judgmentContext
  let ConwayUTxOState utxo _deposits _fees _ = u

  {-   txb := txbody tx   -}
  let txBody = body tx
      allInputs = txBody ^. allInputsTxBodyF

  {- ininterval slot (txvld txb) -}
  runTest $ Allegra.validateOutsideValidityIntervalUTxO slot txBody

  sysSt <- liftSTS $ asks systemStart
  ei <- liftSTS $ asks epochInfo

  {- epochInfoSlotToUTCTime epochInfo systemTime i_f ≠ ◇ -}
  runTest $ validateOutsideForecast ei slot sysSt tx

  {-   txins txb ≠ ∅   -}
  runTestOnSignal $ Shelley.validateInputSetEmptyUTxO txBody

  {-   feesOK pp tx utxo   -}
  runTest $ feesOK pp tx utxo -- Generalizes the fee to small from earlier Era's

  {- allInputs = spendInputs txb ∪ collInputs txb ∪ refInputs txb -}
  {- (spendInputs txb ∪ collInputs txb ∪ refInputs txb) ⊆ dom utxo   -}
  runTest $ Shelley.validateBadInputsUTxO utxo allInputs

  {- consumed pp utxo txb = produced pp poolParams txb -}
  runTest $ Shelley.validateValueNotConservedUTxO pp utxo dpstate txBody

  {-   adaID ∉ supp mint tx - check not needed because mint field of type MultiAsset
   cannot contain ada -}

  {-   ∀ txout ∈ allOuts txb, getValue txout ≥ inject (serSize txout ∗ coinsPerUTxOByte pp) -}
  let allSizedOutputs = toList (txBody ^. allSizedOutputsTxBodyF)
  runTest $ validateOutputTooSmallUTxO pp allSizedOutputs

  let allOutputs = fmap sizedValue allSizedOutputs
  {-   ∀ txout ∈ allOuts txb, serSize (getValue txout) ≤ maxValSize pp   -}
  runTest $ validateOutputTooBigUTxO pp allOutputs

  {- ∀ ( _ ↦ (a,_)) ∈ allOuts txb,  a ∈ Addrbootstrap → bootstrapAttrsSize a ≤ 64 -}
  runTestOnSignal $ Shelley.validateOutputBootAddrAttrsTooBig allOutputs

  netId <- liftSTS $ asks networkId

  {- ∀(_ → (a, _)) ∈ allOuts txb, netId a = NetworkId -}
  runTestOnSignal $ Shelley.validateWrongNetwork netId allOutputs

  {- ∀(a → ) ∈ txwdrls txb, netId a = NetworkId -}
  runTestOnSignal $ Shelley.validateWrongNetworkWithdrawal netId txBody

  {- (txnetworkid txb = NetworkId) ∨ (txnetworkid txb = ◇) -}
  runTestOnSignal $ validateWrongNetworkInTxBody netId txBody

  {- txsize tx ≤ maxTxSize pp -}
  runTestOnSignal $ Shelley.validateMaxTxSizeUTxO pp tx

  {-   totExunits tx ≤ maxTxExUnits pp    -}
  runTest $ validateExUnitsTooBigUTxO pp tx

  {-   ‖collateral tx‖  ≤  maxCollInputs pp   -}
  runTest $ validateTooManyCollateralInputs pp txBody

  trans @(EraRule "UTXOS" era) =<< coerce <$> judgmentContext

instance
  ( EraScript era,
    Eq (PPUPState era),
    Show (PPUPState era),
    Eq (PPUPPredFailure era),
    Show (PPUPPredFailure era),
    Value era ~ MaryValue (EraCrypto era),
    TxOut era ~ BabbageTxOut era,
    Script era ~ AlonzoScript era,
    PredicateFailure (EraRule "UTXOS" era) ~ AlonzoUtxosPredFailure era,
    Event (EraRule "UTXOS" era) ~ AlonzoUtxosEvent era
  ) =>
  Embed (ConwayUTXOS era) (ConwayUTXO era)
  where
  wrapFailed = AlonzoInBabbageUtxoPredFailure . UtxosFailure
  wrapEvent = UtxosEvent

instance
  ( Era era,
    STS (ConwayUTXO era),
    PredicateFailure (EraRule "UTXO" era) ~ BabbageUtxoPredFailure era,
    Event (EraRule "UTXO" era) ~ AlonzoUtxoEvent era,
    BaseM (EraRule "UTXOW" era) ~ ShelleyBase,
    PredicateFailure (EraRule "UTXOW" era) ~ BabbageUtxowPredFailure era,
    Event (EraRule "UTXOW" era) ~ AlonzoUtxowEvent era
  ) =>
  Embed (ConwayUTXO era) (BabbageUTXOW era)
  where
  wrapFailed = UtxoFailure
  wrapEvent = WrappedShelleyEraEvent . UtxoEvent
