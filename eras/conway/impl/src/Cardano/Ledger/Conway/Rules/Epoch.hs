{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Epoch
  ( ConwayEPOCH,
    ShelleyEpochEvent (..),
    PredicateFailure,
  )
where

import Cardano.Ledger.BaseTypes (ShelleyBase)
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.Conway.Era (ConwayEPOCH)
import Cardano.Ledger.Core
import Cardano.Ledger.EpochBoundary (SnapShots)
import Cardano.Ledger.Shelley.LedgerState
  ( EpochState,
    LedgerState,
    PState (..),
    UTxOState (..),
    asReserves,
    esAccountState,
    esLState,
    esNonMyopic,
    esPp,
    esPrevPp,
    esSnapshots,
    lsDPState,
    lsUTxOState,
    obligationDPState,
    pattern DPState,
    pattern EpochState, PPUPStateOrUnit,
  )
import Cardano.Ledger.Shelley.Rewards ()
import Cardano.Ledger.Shelley.Rules
  ( ShelleyEpochEvent (..),
    ShelleyPOOLREAP,
    ShelleyPoolreapEvent,
    ShelleyPoolreapPredFailure,
    ShelleyPoolreapState (..), ShelleyEpochPredFailure (..), UpecPredFailure,
  )
import Cardano.Ledger.Slot (EpochNo)
import Control.SetAlgebra (eval, (⨃))
import Control.State.Transition
  ( Embed (..),
    STS (..),
    TRC (..),
    TransitionRule,
    judgmentContext,
    trans,
  )
import Data.Default.Class (Default)
import qualified Data.Map.Strict as Map
import GHC.Records (HasField)

instance
  ( EraTxOut era,
    HasField "_keyDeposit" (PParams era) Coin,
    HasField "_poolDeposit" (PParams era) Coin,
    Embed (EraRule "SNAP" era) (ConwayEPOCH era),
    Environment (EraRule "SNAP" era) ~ LedgerState era,
    State (EraRule "SNAP" era) ~ SnapShots (EraCrypto era),
    Signal (EraRule "SNAP" era) ~ (),
    Embed (EraRule "POOLREAP" era) (ConwayEPOCH era),
    Environment (EraRule "POOLREAP" era) ~ PParams era,
    State (EraRule "POOLREAP" era) ~ ShelleyPoolreapState era,
    Signal (EraRule "POOLREAP" era) ~ EpochNo,
    Default (PParams era),
    Eq (UpecPredFailure era),
    Show (UpecPredFailure era),
    Default (PPUPStateOrUnit era)
  ) =>
  STS (ConwayEPOCH era)
  where
  type State (ConwayEPOCH era) = EpochState era
  type Signal (ConwayEPOCH era) = EpochNo
  type Environment (ConwayEPOCH era) = ()
  type BaseM (ConwayEPOCH era) = ShelleyBase
  type PredicateFailure (ConwayEPOCH era) = ShelleyEpochPredFailure era
  type Event (ConwayEPOCH era) = ShelleyEpochEvent era
  transitionRules = [epochTransition]

epochTransition ::
  forall era.
  ( Embed (EraRule "SNAP" era) (ConwayEPOCH era),
    Environment (EraRule "SNAP" era) ~ LedgerState era,
    State (EraRule "SNAP" era) ~ SnapShots (EraCrypto era),
    Signal (EraRule "SNAP" era) ~ (),
    Embed (EraRule "POOLREAP" era) (ConwayEPOCH era),
    Environment (EraRule "POOLREAP" era) ~ PParams era,
    State (EraRule "POOLREAP" era) ~ ShelleyPoolreapState era,
    Signal (EraRule "POOLREAP" era) ~ EpochNo
  ) =>
  TransitionRule (ConwayEPOCH era)
epochTransition = do
  TRC
    ( _,
      EpochState
        { esAccountState = acnt,
          esSnapshots = ss,
          esLState = ls,
          esPrevPp = pr,
          esPp = pp,
          esNonMyopic = nm
        },
      e
      ) <-
    judgmentContext
  let utxoSt = lsUTxOState ls
  let DPState dstate pstate = lsDPState ls
  ss' <-
    trans @(EraRule "SNAP" era) $ TRC (ls, ss, ())

  let PState pParams fPParams _ _ = pstate
      ppp = eval (pParams ⨃ fPParams)
      pstate' =
        pstate
          { psStakePoolParams = ppp,
            psFutureStakePoolParams = Map.empty
          }
  PoolreapState utxoSt' acnt' dstate' pstate'' <-
    trans @(EraRule "POOLREAP" era) $
      TRC (pp, PoolreapState utxoSt acnt dstate pstate', e)

  let adjustedDPstate = DPState dstate' pstate''
      epochState' =
        EpochState
          acnt'
          ss'
          (ls {lsUTxOState = utxoSt', lsDPState = adjustedDPstate})
          pr
          pp
          nm

  let -- At the epoch boundary refunds are made, so we need to change what
      -- the sutxosDeposited field is. The other two places where deposits are
      -- kept (dsDeposits of DState and psDeposits of PState) are adjusted by
      -- the rules, So we can recompute the sutxosDeposited field using adjustedDPState
      -- since we have the invariant that: obligationDPState dpstate == sutxosDeposited sutxostate
      Coin oblgNew = obligationDPState adjustedDPstate
      Coin reserves = asReserves acnt'
      utxoSt''' = utxoSt' {sutxosDeposited = Coin oblgNew}
      acnt'' = acnt' {asReserves = Coin reserves}
  pure $
    epochState'
      { esAccountState = acnt'',
        esLState = (esLState epochState') {lsUTxOState = utxoSt'''},
        esPrevPp = pp,
        esPp = pp
      }

instance
  ( Era era,
    STS (ShelleyPOOLREAP era),
    PredicateFailure (EraRule "POOLREAP" era) ~ ShelleyPoolreapPredFailure era,
    Event (EraRule "POOLREAP" era) ~ ShelleyPoolreapEvent era
  ) =>
  Embed (ShelleyPOOLREAP era) (ConwayEPOCH era)
  where
  wrapFailed = PoolReapFailure
  wrapEvent = PoolReapEvent
