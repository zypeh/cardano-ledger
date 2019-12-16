{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module STS.Up
  ( UP
  , UpdateEnv(..)
  )
where

import           BaseTypes
import           Cardano.Ledger.Shelley.Crypto
import           Cardano.Prelude (NoUnexpectedThunks (..))
import           Control.State.Transition
import           GHC.Generics (Generic)
import           Keys
import           PParams
import           Slot
import           STS.Avup
import           STS.Ppup
import           Updates

data UP crypto

data UpdateEnv crypto
  = UpdateEnv SlotNo PParams (GenDelegs crypto)

instance Crypto crypto => STS (UP crypto) where
  type State (UP crypto) = UpdateState crypto
  type Signal (UP crypto) = Update crypto
  type Environment (UP crypto) = UpdateEnv crypto
  type BaseM (UP crypto) = ShelleyBase
  data PredicateFailure (UP crypto)
    = NonGenesisUpdateUP
    | AvupFailure (PredicateFailure (AVUP crypto))
    | PpupFailure (PredicateFailure (PPUP crypto))
    deriving (Show, Eq, Generic)

  initialRules = []
  transitionRules = [upTransition]

instance NoUnexpectedThunks (PredicateFailure (UP crypto))

upTransition
  :: forall crypto
   . Crypto crypto
  => TransitionRule (UP crypto)
upTransition = do
  TRC ( UpdateEnv _slot pp _genDelegs
      , UpdateState pupS aupS favs avs
      , Update pup _aup) <- judgmentContext

  pup' <- trans @(PPUP crypto) $ TRC (PPUPEnv _slot pp _genDelegs, pupS, pup)
  AVUPState aup' favs' avs' <-
    trans @(AVUP crypto) $ TRC (AVUPEnv _slot _genDelegs, AVUPState aupS favs avs, _aup)

  pure $ UpdateState pup' aup' favs' avs'

instance Crypto crypto => Embed (AVUP crypto) (UP crypto) where
  wrapFailed = AvupFailure

instance Crypto crypto => Embed (PPUP crypto) (UP crypto) where
  wrapFailed = PpupFailure
