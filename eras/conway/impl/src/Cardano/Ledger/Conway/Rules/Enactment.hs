{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Conway.Rules.Enactment
  ( ConwayENACTMENT
  )
where

import Cardano.Ledger.Conway.Era (ConwayENACTMENT)
import Control.State.Transition.Extended (STS (..))
import Cardano.Ledger.Era (Era)
import Cardano.Ledger.BaseTypes (EpochNo(..), ShelleyBase)

instance (Era era) => STS (ConwayENACTMENT era) where
  type Environment (ConwayENACTMENT era) = ()
  type PredicateFailure (ConwayENACTMENT era) = ()
  type Signal (ConwayENACTMENT era) = EpochNo
  type State (ConwayENACTMENT era) = ()
  type BaseM (ConwayENACTMENT era) = ShelleyBase

  transitionRules = undefined
