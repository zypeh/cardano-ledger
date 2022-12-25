{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Shelley.Scripts
  ( MultiSig
      ( RequireAllOf,
        RequireAnyOf,
        RequireSignature,
        RequireMOf
      ),
    getMultiSigBytes,
    ScriptHash (..),
    nativeMultiSigTag,
  )
where

import Cardano.Binary
  ( Annotator (..),
    FromCBOR (fromCBOR),
    ToCBOR,
  )
import Cardano.Ledger.BaseTypes (invalidKey)
import Cardano.Ledger.Core
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (Witness))
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Serialization (decodeList, decodeRecordSum, encodeFoldable)
import Cardano.Ledger.Shelley.Era
import Control.DeepSeq (NFData)
import qualified Data.ByteString as BS
import Data.ByteString.Short (ShortByteString)
import Data.Coders (Encode (..), (!>))
import Data.MemoBytes
  ( Mem,
    MemoBytes (..),
    memoBytes,
  )
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))

-- | A simple language for expressing conditions under which it is valid to
-- withdraw from a normal UTxO payment address or to use a stake address.
--
-- The use case is for expressing multi-signature payment addresses and
-- multi-signature stake addresses. These can be combined arbitrarily using
-- logical operations:
--
-- * multi-way \"and\";
-- * multi-way \"or\";
-- * multi-way \"N of M\".
--
-- This makes it easy to express multi-signature addresses, and provides an
-- extension point to express other validity conditions, e.g., as needed for
-- locking funds used with lightning.
data MultiSigRaw crypto
  = -- | Require the redeeming transaction be witnessed by the spending key
    --   corresponding to the given verification key hash.
    RequireSignature' !(KeyHash 'Witness crypto)
  | -- | Require all the sub-terms to be satisfied.
    RequireAllOf' ![MultiSig crypto]
  | -- | Require any one of the sub-terms to be satisfied.
    RequireAnyOf' ![MultiSig crypto]
  | -- | Require M of the given sub-terms to be satisfied.
    RequireMOf' !Int ![MultiSig crypto]
  deriving (Show, Eq, Generic)
  deriving anyclass (NoThunks)

instance NFData (MultiSigRaw era)

newtype MultiSig crypto = MultiSigConstr (MemoBytes (MultiSigRaw crypto))
  deriving (Eq, Show, Generic)
  deriving newtype (ToCBOR, NoThunks, SafeToHash)

-- | Magic number "memorialized" in the ValidateScript class under the method:
--   scriptPrefixTag:: Core.Script era -> Bs.ByteString, for the Shelley Era.
nativeMultiSigTag :: BS.ByteString
nativeMultiSigTag = "\00"

type instance SomeScript 'PhaseOne (ShelleyEra c) = MultiSig c

instance CC.Crypto c => EraScript (ShelleyEra c) where
  type Script (ShelleyEra c) = MultiSig c

  -- In the ShelleyEra there is only one kind of Script and its tag is "\x00"
  scriptPrefixTag _script = nativeMultiSigTag
  phaseScript PhaseOneRep multisig = Just (Phase1Script multisig)
  phaseScript PhaseTwoRep _ = Nothing

deriving newtype instance NFData (MultiSig era)

getMultiSigBytes :: MultiSig crypto -> ShortByteString
getMultiSigBytes (MultiSigConstr (Memo _ bytes)) = bytes

deriving via
  Mem (MultiSigRaw crypto)
  instance
    CC.Crypto crypto =>
    FromCBOR (Annotator (MultiSig crypto))

pattern RequireSignature :: CC.Crypto crypto => KeyHash 'Witness crypto -> MultiSig crypto
pattern RequireSignature akh <-
  MultiSigConstr (Memo (RequireSignature' akh) _)
  where
    RequireSignature akh =
      MultiSigConstr $ memoBytes (Sum RequireSignature' 0 !> To akh)

pattern RequireAllOf :: CC.Crypto crypto => [MultiSig crypto] -> MultiSig crypto
pattern RequireAllOf ms <-
  MultiSigConstr (Memo (RequireAllOf' ms) _)
  where
    RequireAllOf ms =
      MultiSigConstr $ memoBytes (Sum RequireAllOf' 1 !> E encodeFoldable ms)

pattern RequireAnyOf :: CC.Crypto crypto => [MultiSig crypto] -> MultiSig crypto
pattern RequireAnyOf ms <-
  MultiSigConstr (Memo (RequireAnyOf' ms) _)
  where
    RequireAnyOf ms =
      MultiSigConstr $ memoBytes (Sum RequireAnyOf' 2 !> E encodeFoldable ms)

pattern RequireMOf :: CC.Crypto crypto => Int -> [MultiSig crypto] -> MultiSig crypto
pattern RequireMOf n ms <-
  MultiSigConstr (Memo (RequireMOf' n ms) _)
  where
    RequireMOf n ms =
      MultiSigConstr $ memoBytes (Sum RequireMOf' 3 !> To n !> E encodeFoldable ms)

{-# COMPLETE RequireSignature, RequireAllOf, RequireAnyOf, RequireMOf #-}

instance
  CC.Crypto crypto =>
  FromCBOR (Annotator (MultiSigRaw crypto))
  where
  fromCBOR = decodeRecordSum "MultiSig" $
    \case
      0 -> (,) 2 . pure . RequireSignature' . KeyHash <$> fromCBOR
      1 -> do
        multiSigs <- sequence <$> decodeList fromCBOR
        pure (2, RequireAllOf' <$> multiSigs)
      2 -> do
        multiSigs <- sequence <$> decodeList fromCBOR
        pure (2, RequireAnyOf' <$> multiSigs)
      3 -> do
        m <- fromCBOR
        multiSigs <- sequence <$> decodeList fromCBOR
        pure (3, RequireMOf' m <$> multiSigs)
      k -> invalidKey k
