{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.Babbage.TxBody
  ( TxOut (TxOut, TxOutCompact, TxOutCompactDH, TxOutCompactDatum, TxOutCompactRefScript),
    TxBody
      ( TxBody,
        inputs,
        collateral,
        referenceInputs,
        outputs,
        collateralReturn,
        totalCollateral,
        txcerts,
        txwdrls,
        txfee,
        txvldt,
        txUpdates,
        reqSignerHashes,
        mint,
        scriptIntegrityHash,
        adHash,
        txnetworkid
      ),
    Datum (..),
    datumDataHash,
    spendInputs',
    collateralInputs',
    referenceInputs',
    outputs',
    collateralReturn',
    totalCollateral',
    certs',
    wdrls',
    txfee',
    vldt',
    update',
    reqSignerHashes',
    mint',
    scriptIntegrityHash',
    adHash',
    txnetworkid',
    getBabbageTxOutEitherAddr,
    BabbageBody,
    EraIndependentScriptIntegrity,
    ScriptIntegrityHash,
  )
where

import Cardano.Binary
  ( DecoderError (..),
    FromCBOR (..),
    ToCBOR (..),
    TokenType (..),
    decodeAnnotator,
    decodeBreakOr,
    decodeListLenOrIndef,
    decodeNestedCborBytes,
    encodeNestedCbor,
    peekTokenType,
  )
import Cardano.Crypto.Hash
import Cardano.Ledger.Address (Addr (..))
import Cardano.Ledger.Alonzo.Data
  ( AuxiliaryDataHash (..),
    BinaryData,
    Data,
    DataHash,
    binaryDataToData,
  )
import Cardano.Ledger.Alonzo.TxBody
  ( Addr28Extra,
    DataHash32,
    decodeAddress28,
    decodeDataHash32,
    encodeAddress28,
    encodeDataHash32,
    getAdaOnly,
  )
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import Cardano.Ledger.BaseTypes
  ( Network (..),
    StrictMaybe (..),
    maybeToStrictMaybe,
  )
import Cardano.Ledger.Coin (Coin (..))
import Cardano.Ledger.CompactAddress
  ( CompactAddr,
    compactAddr,
    decompactAddr,
    fromCborBackwardsBothAddr,
    fromCborBothAddr,
    fromCborRewardAcnt,
  )
import Cardano.Ledger.Compactible
import Cardano.Ledger.Core (PParamsDelta)
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential (Credential (..), StakeReference (..))
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Hashes
  ( EraIndependentScriptIntegrity,
    EraIndependentTxBody,
  )
import Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import Cardano.Ledger.Mary.Value (Value (..), policies, policyID)
import qualified Cardano.Ledger.Mary.Value as Mary
import Cardano.Ledger.SafeHash
  ( HashAnnotated,
    SafeHash,
    SafeToHash,
  )
import Cardano.Ledger.Shelley.Delegation.Certificates (DCert)
import Cardano.Ledger.Shelley.PParams (Update)
import Cardano.Ledger.Shelley.Scripts (ScriptHash (..))
import Cardano.Ledger.Shelley.TxBody (Wdrl (Wdrl), unWdrl)
import Cardano.Ledger.ShelleyMA.Timelocks (ValidityInterval (..))
import Cardano.Ledger.TxIn (TxIn (..))
import Cardano.Ledger.Val
  ( DecodeNonNegative,
    Val (..),
    decodeMint,
    decodeNonNegative,
    encodeMint,
    isZero,
  )
import Control.DeepSeq (NFData (rnf), rwhnf)
import Control.Monad ((<$!>))
import qualified Data.ByteString.Lazy as LBS
import Data.Coders
import Data.Maybe (fromMaybe)
import Data.MemoBytes (Mem, MemoBytes (..), memoBytes)
import Data.Sequence.Strict (StrictSeq)
import qualified Data.Sequence.Strict as StrictSeq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Sharing (FromSharedCBOR (..), Interns, interns)
import qualified Data.Text as T
import Data.Typeable (Proxy (..), Typeable, (:~:) (Refl))
import Data.Word
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import GHC.Stack (HasCallStack)
import NoThunks.Class (InspectHeapNamed (..), NoThunks)
import Prelude hiding (lookup)

data TxOut era
  = TxOutCompact'
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Core.Value era))
  | TxOutCompactDH'
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Core.Value era))
      !(DataHash (Crypto era))
  | TxOutCompactDatum
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Core.Value era))
      {-# UNPACK #-} !(BinaryData era) -- Inline data
  | TxOutCompactRefScript
      {-# UNPACK #-} !(CompactAddr (Crypto era))
      !(CompactForm (Core.Value era))
      !(Datum era)
      !(Core.Script era)
  | TxOut_AddrHash28_AdaOnly
      !(Credential 'Staking (Crypto era))
      {-# UNPACK #-} !Addr28Extra
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
  | TxOut_AddrHash28_AdaOnly_DataHash32
      !(Credential 'Staking (Crypto era))
      {-# UNPACK #-} !Addr28Extra
      {-# UNPACK #-} !(CompactForm Coin) -- Ada value
      {-# UNPACK #-} !DataHash32

deriving stock instance
  ( Eq (Core.Value era),
    Eq (Core.Script era),
    Compactible (Core.Value era)
  ) =>
  Eq (TxOut era)

-- | Already in NF
instance NFData (TxOut era) where
  rnf = rwhnf

viewCompactTxOut ::
  forall era.
  Era era =>
  TxOut era ->
  (CompactAddr (Crypto era), CompactForm (Core.Value era), Datum era, StrictMaybe (Core.Script era))
viewCompactTxOut txOut = case txOut of
  TxOutCompact' addr val -> (addr, val, NoDatum, SNothing)
  TxOutCompactDH' addr val dh -> (addr, val, DatumHash dh, SNothing)
  TxOutCompactDatum addr val datum -> (addr, val, Datum datum, SNothing)
  TxOutCompactRefScript addr val datum rs -> (addr, val, datum, SJust rs)
  TxOut_AddrHash28_AdaOnly stakeRef addr28Extra adaVal ->
    let (a, b, c) = Alonzo.viewCompactTxOut @era $ Alonzo.TxOut_AddrHash28_AdaOnly stakeRef addr28Extra adaVal
     in (a, b, toDatum c, SNothing)
  TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra adaVal dataHash32 ->
    let (a, b, c) =
          Alonzo.viewCompactTxOut @era $
            Alonzo.TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra adaVal dataHash32
     in (a, b, toDatum c, SNothing)
  where
    toDatum = \case
      SNothing -> NoDatum
      SJust dh -> DatumHash dh

viewTxOut ::
  forall era.
  Era era =>
  TxOut era ->
  (Addr (Crypto era), Core.Value era, Datum era, StrictMaybe (Core.Script era))
viewTxOut (TxOutCompact' bs c) = (addr, val, NoDatum, SNothing)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewTxOut (TxOutCompactDH' bs c dh) = (addr, val, DatumHash dh, SNothing)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewTxOut (TxOutCompactDatum bs c d) = (addr, val, Datum d, SNothing)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewTxOut (TxOutCompactRefScript bs c d rs) = (addr, val, d, SJust rs)
  where
    addr = decompactAddr bs
    val = fromCompact c
viewTxOut (TxOut_AddrHash28_AdaOnly stakeRef addr28Extra adaVal) = (addr, val, NoDatum, SNothing)
  where
    (addr, val, _) =
      Alonzo.viewTxOut @era $ Alonzo.TxOut_AddrHash28_AdaOnly stakeRef addr28Extra adaVal
viewTxOut (TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra adaVal dataHash32) =
  case mDataHash of
    SNothing -> (addr, val, NoDatum, SNothing)
    SJust dh -> (addr, val, DatumHash dh, SNothing)
  where
    (addr, val, mDataHash) =
      Alonzo.viewTxOut @era $
        Alonzo.TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra adaVal dataHash32

instance
  ( Era era,
    Show (Core.Value era),
    Show (Core.Script era),
    Show (CompactForm (Core.Value era))
  ) =>
  Show (TxOut era)
  where
  show = show . viewTxOut

deriving via InspectHeapNamed "TxOut" (TxOut era) instance NoThunks (TxOut era)

data Datum era
  = NoDatum
  | DatumHash !(DataHash (Crypto era))
  | Datum !(BinaryData era)
  deriving (Eq, Ord, Show)

instance Era era => ToCBOR (Datum era) where
  toCBOR d = encode $ case d of
    DatumHash dh -> Sum DatumHash 0 !> To dh
    Datum d' -> Sum Datum 1 !> To d'
    NoDatum -> OmitC NoDatum

instance Era era => FromCBOR (Datum era) where
  fromCBOR = decode (Summands "Datum" decodeDatum)
    where
      decodeDatum 0 = SumD DatumHash <! From
      decodeDatum 1 = SumD Datum <! From
      decodeDatum k = Invalid k

datumDataHash :: Datum era -> StrictMaybe (DataHash (Crypto era))
datumDataHash = \case
  NoDatum -> SNothing
  (DatumHash dh) -> SJust dh
  (Datum _) -> SNothing

pattern TxOut ::
  forall era.
  ( Era era,
    Compactible (Core.Value era),
    Val (Core.Value era),
    HasCallStack
  ) =>
  Addr (Crypto era) ->
  Core.Value era ->
  Datum era ->
  StrictMaybe (Core.Script era) ->
  TxOut era
pattern TxOut addr vl datum refScript <-
  (viewTxOut -> (addr, vl, datum, refScript))
  where
    TxOut addr vl datum refScript = mkTxOut addr (compactAddr addr) vl datum refScript

{-# COMPLETE TxOut #-}

-- | Helper function for constructing a TxOut. Both compacted and uncompacted
-- address should be the exact same addrress in different forms.
mkTxOut ::
  forall era.
  (Era era, HasCallStack) =>
  Addr (Crypto era) ->
  CompactAddr (Crypto era) ->
  Core.Value era ->
  Datum era ->
  StrictMaybe (Core.Script era) ->
  TxOut era
mkTxOut addr _cAddr vl NoDatum SNothing
  | Just adaCompact <- getAdaOnly (Proxy @era) vl,
    Addr network paymentCred stakeRef <- addr,
    StakeRefBase stakeCred <- stakeRef,
    Just (Refl, addr28Extra) <- encodeAddress28 network paymentCred =
      TxOut_AddrHash28_AdaOnly stakeCred addr28Extra adaCompact
mkTxOut addr _cAddr vl (DatumHash dh) SNothing
  | Just adaCompact <- getAdaOnly (Proxy @era) vl,
    Addr network paymentCred stakeRef <- addr,
    StakeRefBase stakeCred <- stakeRef,
    Just (Refl, addr28Extra) <- encodeAddress28 network paymentCred,
    Just (Refl, dataHash32) <- encodeDataHash32 dh =
      TxOut_AddrHash28_AdaOnly_DataHash32 stakeCred addr28Extra adaCompact dataHash32
mkTxOut _addr cAddr vl d rs =
  let cVal = fromMaybe (error "Illegal value in txout") $ toCompact vl
   in case rs of
        SNothing -> case d of
          NoDatum -> TxOutCompact' cAddr cVal
          DatumHash dh -> TxOutCompactDH' cAddr cVal dh
          Datum binaryData -> TxOutCompactDatum cAddr cVal binaryData
        SJust rs' -> TxOutCompactRefScript cAddr cVal d rs'

pattern TxOutCompact ::
  ( Era era,
    HasCallStack
  ) =>
  CompactAddr (Crypto era) ->
  CompactForm (Core.Value era) ->
  TxOut era
pattern TxOutCompact addr vl <-
  (viewCompactTxOut -> (addr, vl, NoDatum, SNothing))
  where
    TxOutCompact cAddr cVal
      | isAdaOnlyCompact cVal =
          mkTxOut (decompactAddr cAddr) cAddr (fromCompact cVal) NoDatum SNothing
      | otherwise = TxOutCompact' cAddr cVal

pattern TxOutCompactDH ::
  ( Era era,
    HasCallStack
  ) =>
  CompactAddr (Crypto era) ->
  CompactForm (Core.Value era) ->
  DataHash (Crypto era) ->
  TxOut era
pattern TxOutCompactDH addr vl dh <-
  (viewCompactTxOut -> (addr, vl, DatumHash dh, SNothing))
  where
    TxOutCompactDH cAddr cVal dh
      | isAdaOnlyCompact cVal =
          mkTxOut (decompactAddr cAddr) cAddr (fromCompact cVal) (DatumHash dh) SNothing
      | otherwise = TxOutCompactDH' cAddr cVal dh

{-# COMPLETE TxOutCompact, TxOutCompactDH #-}

-- ======================================

type ScriptIntegrityHash crypto = SafeHash crypto EraIndependentScriptIntegrity

data TxBodyRaw era = TxBodyRaw
  { txSpendInputs :: !(Set (TxIn (Crypto era))),
    txCollateralInputs :: !(Set (TxIn (Crypto era))),
    txReferenceInputs :: !(Set (TxIn (Crypto era))),
    txOutputs :: !(StrictSeq (TxOut era)),
    txCollateralReturn :: !(StrictMaybe (TxOut era)),
    txTotalCollateral :: !(StrictMaybe Coin),
    txCerts :: !(StrictSeq (DCert (Crypto era))),
    txWdrls :: !(Wdrl (Crypto era)),
    txFee :: !Coin,
    txVldt :: !ValidityInterval,
    txUpdate :: !(StrictMaybe (Update era)),
    txReqSignerHashes :: Set (KeyHash 'Witness (Crypto era)),
    txMint :: !(Value (Crypto era)),
    -- The spec makes it clear that the mint field is a
    -- Cardano.Ledger.Mary.Value.Value, not a Core.Value.
    -- Operations on the TxBody in the BabbageEra depend upon this.
    txScriptIntegrityHash :: !(StrictMaybe (ScriptIntegrityHash (Crypto era))),
    txAdHash :: !(StrictMaybe (AuxiliaryDataHash (Crypto era))),
    txNetworkid :: !(StrictMaybe Network)
  }
  deriving (Generic, Typeable)

deriving instance
  ( Eq (Core.Value era),
    Eq (Core.Script era),
    CC.Crypto (Crypto era),
    Compactible (Core.Value era),
    Eq (PParamsDelta era)
  ) =>
  Eq (TxBodyRaw era)

instance
  (Typeable era, NoThunks (Core.Value era), NoThunks (PParamsDelta era)) =>
  NoThunks (TxBodyRaw era)

deriving instance
  ( Era era,
    Show (Core.Value era),
    Show (Core.Script era),
    Show (PParamsDelta era)
  ) =>
  Show (TxBodyRaw era)

newtype TxBody era = TxBodyConstr (MemoBytes (TxBodyRaw era))
  deriving (ToCBOR)
  deriving newtype (SafeToHash)

deriving newtype instance
  ( CC.Crypto (Crypto era)
  ) =>
  Eq (TxBody era)

deriving instance
  ( Typeable era,
    NoThunks (Core.Value era),
    NoThunks (PParamsDelta era)
  ) =>
  NoThunks (TxBody era)

deriving instance
  ( Era era,
    Compactible (Core.Value era),
    Show (Core.Script era),
    Show (Core.Value era),
    Show (PParamsDelta era)
  ) =>
  Show (TxBody era)

deriving via
  (Mem (TxBodyRaw era))
  instance
    ( Era era,
      Typeable (Core.Script era),
      Typeable (Core.AuxiliaryData era),
      Compactible (Core.Value era),
      Show (Core.Value era),
      DecodeNonNegative (Core.Value era),
      FromCBOR (Annotator (Core.Script era)),
      Core.SerialisableData (PParamsDelta era)
    ) =>
    FromCBOR (Annotator (TxBody era))

instance
  ( Era era,
    Typeable (Core.Script era),
    Typeable (Core.AuxiliaryData era),
    Compactible (Core.Value era),
    Show (Core.Value era),
    DecodeNonNegative (Core.Value era),
    FromCBOR (Annotator (Core.Script era)),
    FromCBOR (PParamsDelta era),
    ToCBOR (PParamsDelta era)
  ) =>
  FromCBOR (Annotator (TxBodyRaw era))
  where
  fromCBOR = pure <$> fromCBOR

-- The Set of constraints necessary to use the TxBody pattern
type BabbageBody era =
  ( Era era,
    ToCBOR (Core.Value era),
    ToCBOR (Core.Script era),
    Core.SerialisableData (PParamsDelta era)
  )

pattern TxBody ::
  BabbageBody era =>
  Set (TxIn (Crypto era)) ->
  Set (TxIn (Crypto era)) ->
  Set (TxIn (Crypto era)) ->
  StrictSeq (TxOut era) ->
  StrictMaybe (TxOut era) ->
  StrictMaybe Coin ->
  StrictSeq (DCert (Crypto era)) ->
  Wdrl (Crypto era) ->
  Coin ->
  ValidityInterval ->
  StrictMaybe (Update era) ->
  Set (KeyHash 'Witness (Crypto era)) ->
  Value (Crypto era) ->
  StrictMaybe (ScriptIntegrityHash (Crypto era)) ->
  StrictMaybe (AuxiliaryDataHash (Crypto era)) ->
  StrictMaybe Network ->
  TxBody era
pattern TxBody
  { inputs,
    collateral,
    referenceInputs,
    outputs,
    collateralReturn,
    totalCollateral,
    txcerts,
    txwdrls,
    txfee,
    txvldt,
    txUpdates,
    reqSignerHashes,
    mint,
    scriptIntegrityHash,
    adHash,
    txnetworkid
  } <-
  TxBodyConstr
    ( Memo
        TxBodyRaw
          { txSpendInputs = inputs,
            txCollateralInputs = collateral,
            txReferenceInputs = referenceInputs,
            txOutputs = outputs,
            txCollateralReturn = collateralReturn,
            txTotalCollateral = totalCollateral,
            txCerts = txcerts,
            txWdrls = txwdrls,
            txFee = txfee,
            txVldt = txvldt,
            txUpdate = txUpdates,
            txReqSignerHashes = reqSignerHashes,
            txMint = mint,
            txScriptIntegrityHash = scriptIntegrityHash,
            txAdHash = adHash,
            txNetworkid = txnetworkid
          }
        _
      )
  where
    TxBody
      inputsX
      collateralX
      referenceInputsX
      outputsX
      collateralReturnX
      totalCollateralX
      certsX
      wdrlsX
      txfeeX
      vldtX
      updateX
      reqSignerHashesX
      mintX
      scriptIntegrityHashX
      adHashX
      txnetworkidX =
        TxBodyConstr $
          memoBytes
            ( encodeTxBodyRaw $
                TxBodyRaw
                  inputsX
                  collateralX
                  referenceInputsX
                  outputsX
                  collateralReturnX
                  totalCollateralX
                  certsX
                  wdrlsX
                  txfeeX
                  vldtX
                  updateX
                  reqSignerHashesX
                  mintX
                  scriptIntegrityHashX
                  adHashX
                  txnetworkidX
            )

{-# COMPLETE TxBody #-}

instance (c ~ Crypto era) => HashAnnotated (TxBody era) EraIndependentTxBody c

-- ==============================================================================
-- We define these accessor functions manually, because if we define them using
-- the record syntax in the TxBody pattern, they inherit the (BabbageBody era)
-- constraint as a precondition. This is unnecessary, as one can see below
-- they need not be constrained at all. This should be fixed in the GHC compiler.

spendInputs' :: TxBody era -> Set (TxIn (Crypto era))
collateralInputs' :: TxBody era -> Set (TxIn (Crypto era))
referenceInputs' :: TxBody era -> Set (TxIn (Crypto era))
outputs' :: TxBody era -> StrictSeq (TxOut era)
collateralReturn' :: TxBody era -> StrictMaybe (TxOut era)
totalCollateral' :: TxBody era -> StrictMaybe Coin
certs' :: TxBody era -> StrictSeq (DCert (Crypto era))
txfee' :: TxBody era -> Coin
wdrls' :: TxBody era -> Wdrl (Crypto era)
vldt' :: TxBody era -> ValidityInterval
update' :: TxBody era -> StrictMaybe (Update era)
reqSignerHashes' :: TxBody era -> Set (KeyHash 'Witness (Crypto era))
adHash' :: TxBody era -> StrictMaybe (AuxiliaryDataHash (Crypto era))
mint' :: TxBody era -> Value (Crypto era)
scriptIntegrityHash' :: TxBody era -> StrictMaybe (ScriptIntegrityHash (Crypto era))
spendInputs' (TxBodyConstr (Memo raw _)) = txSpendInputs raw

txnetworkid' :: TxBody era -> StrictMaybe Network

collateralInputs' (TxBodyConstr (Memo raw _)) = txCollateralInputs raw

referenceInputs' (TxBodyConstr (Memo raw _)) = txReferenceInputs raw

outputs' (TxBodyConstr (Memo raw _)) = txOutputs raw

collateralReturn' (TxBodyConstr (Memo raw _)) = txCollateralReturn raw

totalCollateral' (TxBodyConstr (Memo raw _)) = txTotalCollateral raw

certs' (TxBodyConstr (Memo raw _)) = txCerts raw

wdrls' (TxBodyConstr (Memo raw _)) = txWdrls raw

txfee' (TxBodyConstr (Memo raw _)) = txFee raw

vldt' (TxBodyConstr (Memo raw _)) = txVldt raw

update' (TxBodyConstr (Memo raw _)) = txUpdate raw

reqSignerHashes' (TxBodyConstr (Memo raw _)) = txReqSignerHashes raw

adHash' (TxBodyConstr (Memo raw _)) = txAdHash raw

mint' (TxBodyConstr (Memo raw _)) = txMint raw

scriptIntegrityHash' (TxBodyConstr (Memo raw _)) = txScriptIntegrityHash raw

txnetworkid' (TxBodyConstr (Memo raw _)) = txNetworkid raw

--------------------------------------------------------------------------------
-- Serialisation
--------------------------------------------------------------------------------

{-# INLINE encodeTxOut #-}
encodeTxOut ::
  forall era.
  (Era era, ToCBOR (Core.Script era), ToCBOR (Core.Value era)) =>
  CompactAddr (Crypto era) ->
  Core.Value era ->
  Datum era ->
  StrictMaybe (Core.Script era) ->
  Encoding
encodeTxOut addr val datum script =
  encode $
    Keyed (,,,,)
      !> Key 0 (To addr)
      !> Key 1 (To val)
      !> Omit (== NoDatum) (Key 2 (To datum))
      !> encodeKeyedStrictMaybeWith 3 encodeNestedCbor script

data DecodingTxOut era = DecodingTxOut
  { decodingTxOutAddr :: !(StrictMaybe (Addr (Crypto era), CompactAddr (Crypto era))),
    decodingTxOutVal :: !(Core.Value era),
    decodingTxOutDatum :: !(Datum era),
    decodingTxOutScript :: !(StrictMaybe (Core.Script era))
  }
  deriving (Typeable)

{-# INLINE decodeTxOut #-}
decodeTxOut ::
  forall s era.
  ( Era era,
    FromCBOR (Annotator (Core.Script era)),
    DecodeNonNegative (Core.Value era)
  ) =>
  Decoder s (TxOut era)
decodeTxOut = do
  dtxo <- decode $ SparseKeyed "TxOut" initial bodyFields requiredFields
  case dtxo of
    DecodingTxOut SNothing _ _ _ -> cborError $ DecoderErrorCustom "TxOut" "Impossible: no Addr"
    DecodingTxOut (SJust (addr, cAddr)) val d script -> pure $ mkTxOut addr cAddr val d script
  where
    initial :: DecodingTxOut era
    initial =
      DecodingTxOut SNothing mempty NoDatum SNothing
    bodyFields :: (Word -> Field (DecodingTxOut era))
    bodyFields 0 =
      field
        (\x txo -> txo {decodingTxOutAddr = SJust x})
        (D fromCborBothAddr)
    bodyFields 1 =
      field
        (\x txo -> txo {decodingTxOutVal = x})
        (D decodeNonNegative)
    bodyFields 2 =
      field
        (\x txo -> txo {decodingTxOutDatum = x})
        (D fromCBOR)
    bodyFields 3 =
      ofield
        (\x txo -> txo {decodingTxOutScript = x})
        (D $ decodeCIC "Script")
    bodyFields n = field (\_ t -> t) (Invalid n)
    requiredFields =
      [ (0, "addr"),
        (1, "val")
      ]

decodeCIC :: (FromCBOR (Annotator b)) => T.Text -> Decoder s b
decodeCIC s = do
  lbs <- decodeNestedCborBytes
  case decodeAnnotator s fromCBOR (LBS.fromStrict lbs) of
    Left e -> fail $ T.unpack s <> ": " <> show e
    Right x -> pure x

instance
  ( Era era,
    ToCBOR (Core.Value era),
    ToCBOR (Core.Script era)
  ) =>
  ToCBOR (TxOut era)
  where
  toCBOR (TxOut addr v d s) = encodeTxOut (compactAddr addr) v d s

-- FIXME: ^ Starting with Babbage we need to reserialize all Addresses.  It is
-- safe to reserialize an address, because we do not rely on this instance for
-- computing a hash of a transaction and it is only used in storing TxOuts in
-- the ledger state.
--
-- After Vasil Hardfork we can switch it back to a more efficient version below:
--
-- toCBOR (TxOutCompact addr cv) = encodeTxOut @era addr cv NoDatum SNothing
-- toCBOR (TxOutCompactDH addr cv dh) = encodeTxOut @era addr cv (DatumHash dh) SNothing
-- toCBOR (TxOutCompactDatum addr cv d) = encodeTxOut addr cv (Datum d) SNothing
-- toCBOR (TxOutCompactRefScript addr cv d rs) = encodeTxOut addr cv d (SJust rs)

instance
  ( Era era,
    DecodeNonNegative (Core.Value era),
    Show (Core.Value era),
    FromCBOR (Annotator (Core.Script era)),
    Compactible (Core.Value era)
  ) =>
  FromCBOR (TxOut era)
  where
  fromCBOR = fromCborTxOutWithAddr fromCborBackwardsBothAddr

instance
  ( Era era,
    DecodeNonNegative (Core.Value era),
    Show (Core.Value era),
    FromCBOR (Annotator (Core.Script era)),
    Compactible (Core.Value era)
  ) =>
  FromSharedCBOR (TxOut era)
  where
  type Share (TxOut era) = Interns (Credential 'Staking (Crypto era))
  fromSharedCBOR credsInterns =
    internTxOut <$!> fromCborTxOutWithAddr fromCborBackwardsBothAddr
    where
      internTxOut = \case
        TxOut_AddrHash28_AdaOnly cred addr28Extra ada ->
          TxOut_AddrHash28_AdaOnly (interns credsInterns cred) addr28Extra ada
        TxOut_AddrHash28_AdaOnly_DataHash32 cred addr28Extra ada dataHash32 ->
          TxOut_AddrHash28_AdaOnly_DataHash32 (interns credsInterns cred) addr28Extra ada dataHash32
        txOut -> txOut

fromCborTxOutWithAddr ::
  ( Era era,
    FromCBOR (Annotator (Core.Script era)),
    DecodeNonNegative (Core.Value era)
  ) =>
  Decoder s (Addr (Crypto era), CompactAddr (Crypto era)) ->
  Decoder s (TxOut era)
fromCborTxOutWithAddr decAddr = do
  peekTokenType >>= \case
    TypeMapLenIndef -> decodeTxOut
    TypeMapLen -> decodeTxOut
    _ -> oldTxOut
  where
    oldTxOut = do
      lenOrIndef <- decodeListLenOrIndef
      case lenOrIndef of
        Nothing -> do
          (a, ca) <- fromCborBackwardsBothAddr
          v <- decodeNonNegative
          decodeBreakOr >>= \case
            True -> pure $ mkTxOut a ca v NoDatum SNothing
            False -> do
              dh <- fromCBOR
              decodeBreakOr >>= \case
                True -> pure $ mkTxOut a ca v (DatumHash dh) SNothing
                False -> cborError $ DecoderErrorCustom "txout" "Excess terms in txout"
        Just 2 -> do
          (a, ca) <- decAddr
          v <- decodeNonNegative
          pure $ mkTxOut a ca v NoDatum SNothing
        Just 3 -> do
          (a, ca) <- fromCborBackwardsBothAddr
          v <- decodeNonNegative
          dh <- fromCBOR
          pure $ mkTxOut a ca v (DatumHash dh) SNothing
        Just _ -> cborError $ DecoderErrorCustom "txout" "wrong number of terms in txout"

encodeTxBodyRaw ::
  ( Era era,
    ToCBOR (PParamsDelta era),
    ToCBOR (Core.Script era),
    ToCBOR (Core.Value era)
  ) =>
  TxBodyRaw era ->
  Encode ('Closed 'Sparse) (TxBodyRaw era)
encodeTxBodyRaw
  TxBodyRaw
    { txSpendInputs,
      txCollateralInputs,
      txReferenceInputs,
      txOutputs,
      txCollateralReturn,
      txTotalCollateral,
      txCerts,
      txWdrls,
      txFee,
      txVldt = ValidityInterval bot top,
      txUpdate,
      txReqSignerHashes,
      txMint,
      txScriptIntegrityHash,
      txAdHash,
      txNetworkid
    } =
    Keyed
      ( \i ifee ri o cr tc f t c w u b rsh mi sh ah ni ->
          TxBodyRaw i ifee ri o cr tc c w f (ValidityInterval b t) u rsh mi sh ah ni
      )
      !> Key 0 (E encodeFoldable txSpendInputs)
      !> Key 13 (E encodeFoldable txCollateralInputs)
      !> Key 18 (E encodeFoldable txReferenceInputs)
      !> Key 1 (E encodeFoldable txOutputs)
      !> encodeKeyedStrictMaybe 16 txCollateralReturn
      !> encodeKeyedStrictMaybe 17 txTotalCollateral
      !> Key 2 (To txFee)
      !> encodeKeyedStrictMaybe 3 top
      !> Omit null (Key 4 (E encodeFoldable txCerts))
      !> Omit (null . unWdrl) (Key 5 (To txWdrls))
      !> encodeKeyedStrictMaybe 6 txUpdate
      !> encodeKeyedStrictMaybe 8 bot
      !> Key 14 (E encodeFoldable txReqSignerHashes)
      !> Omit isZero (Key 9 (E encodeMint txMint))
      !> encodeKeyedStrictMaybe 11 txScriptIntegrityHash
      !> encodeKeyedStrictMaybe 7 txAdHash
      !> encodeKeyedStrictMaybe 15 txNetworkid

instance
  forall era.
  ( Era era,
    Typeable (Core.Script era),
    Typeable (Core.AuxiliaryData era),
    Compactible (Core.Value era),
    Show (Core.Value era),
    DecodeNonNegative (Core.Value era),
    FromCBOR (Annotator (Core.Script era)),
    FromCBOR (PParamsDelta era),
    ToCBOR (PParamsDelta era)
  ) =>
  FromCBOR (TxBodyRaw era)
  where
  fromCBOR =
    decode $
      SparseKeyed
        "TxBodyRaw"
        initial
        bodyFields
        requiredFields
    where
      initial :: TxBodyRaw era
      initial =
        TxBodyRaw
          mempty
          mempty
          mempty
          StrictSeq.empty
          SNothing
          SNothing
          StrictSeq.empty
          (Wdrl mempty)
          mempty
          (ValidityInterval SNothing SNothing)
          SNothing
          mempty
          mempty
          SNothing
          SNothing
          SNothing
      bodyFields :: (Word -> Field (TxBodyRaw era))
      bodyFields 0 =
        field
          (\x tx -> tx {txSpendInputs = x})
          (D (decodeSet fromCBOR))
      bodyFields 13 =
        field
          (\x tx -> tx {txCollateralInputs = x})
          (D (decodeSet fromCBOR))
      bodyFields 18 =
        field
          (\x tx -> tx {txReferenceInputs = x})
          (D (decodeSet fromCBOR))
      bodyFields 1 =
        field
          (\x tx -> tx {txOutputs = x})
          (D (decodeStrictSeq (fromCborTxOutWithAddr fromCborBothAddr)))
      bodyFields 16 =
        ofield
          (\x tx -> tx {txCollateralReturn = x})
          (D (fromCborTxOutWithAddr fromCborBothAddr))
      bodyFields 17 =
        ofield
          (\x tx -> tx {txTotalCollateral = x})
          From
      bodyFields 2 = field (\x tx -> tx {txFee = x}) From
      bodyFields 3 =
        ofield
          (\x tx -> tx {txVldt = (txVldt tx) {invalidHereafter = x}})
          From
      bodyFields 4 =
        field
          (\x tx -> tx {txCerts = x})
          (D (decodeStrictSeq fromCBOR))
      bodyFields 5 =
        field
          (\x tx -> tx {txWdrls = x})
          (D (Wdrl <$> decodeMap fromCborRewardAcnt fromCBOR))
      bodyFields 6 = ofield (\x tx -> tx {txUpdate = x}) From
      bodyFields 7 = ofield (\x tx -> tx {txAdHash = x}) From
      bodyFields 8 =
        ofield
          (\x tx -> tx {txVldt = (txVldt tx) {invalidBefore = x}})
          From
      bodyFields 9 = field (\x tx -> tx {txMint = x}) (D decodeMint)
      bodyFields 11 = ofield (\x tx -> tx {txScriptIntegrityHash = x}) From
      bodyFields 14 = field (\x tx -> tx {txReqSignerHashes = x}) (D (decodeSet fromCBOR))
      bodyFields 15 = ofield (\x tx -> tx {txNetworkid = x}) From
      bodyFields n = field (\_ t -> t) (Invalid n)
      requiredFields =
        [ (0, "inputs"),
          (1, "outputs"),
          (2, "fee")
        ]

-- ====================================================
-- HasField instances to be consistent with earlier Eras

instance (Crypto era ~ c) => HasField "inputs" (TxBody era) (Set (TxIn c)) where
  getField (TxBodyConstr (Memo m _)) = txSpendInputs m

instance HasField "outputs" (TxBody era) (StrictSeq (TxOut era)) where
  getField (TxBodyConstr (Memo m _)) = txOutputs m

instance Crypto era ~ crypto => HasField "certs" (TxBody era) (StrictSeq (DCert crypto)) where
  getField (TxBodyConstr (Memo m _)) = txCerts m

instance Crypto era ~ crypto => HasField "wdrls" (TxBody era) (Wdrl crypto) where
  getField (TxBodyConstr (Memo m _)) = txWdrls m

instance HasField "txfee" (TxBody era) Coin where
  getField (TxBodyConstr (Memo m _)) = txFee m

instance HasField "update" (TxBody era) (StrictMaybe (Update era)) where
  getField (TxBodyConstr (Memo m _)) = txUpdate m

instance
  (Crypto era ~ c) =>
  HasField "reqSignerHashes" (TxBody era) (Set (KeyHash 'Witness c))
  where
  getField (TxBodyConstr (Memo m _)) = txReqSignerHashes m

instance (Crypto era ~ c) => HasField "mint" (TxBody era) (Mary.Value c) where
  getField (TxBodyConstr (Memo m _)) = txMint m

instance (Crypto era ~ c) => HasField "collateral" (TxBody era) (Set (TxIn c)) where
  getField (TxBodyConstr (Memo m _)) = txCollateralInputs m

instance (Crypto era ~ c) => HasField "referenceInputs" (TxBody era) (Set (TxIn c)) where
  getField (TxBodyConstr (Memo m _)) = txReferenceInputs m

instance HasField "collateralReturn" (TxBody era) (StrictMaybe (TxOut era)) where
  getField (TxBodyConstr (Memo m _)) = txCollateralReturn m

instance HasField "totalCollateral" (TxBody era) (StrictMaybe Coin) where
  getField (TxBodyConstr (Memo m _)) = txTotalCollateral m

instance (Crypto era ~ c) => HasField "minted" (TxBody era) (Set (ScriptHash c)) where
  getField (TxBodyConstr (Memo m _)) = Set.map policyID (policies (txMint m))

instance HasField "vldt" (TxBody era) ValidityInterval where
  getField (TxBodyConstr (Memo m _)) = txVldt m

instance
  c ~ Crypto era =>
  HasField "adHash" (TxBody era) (StrictMaybe (AuxiliaryDataHash c))
  where
  getField (TxBodyConstr (Memo m _)) = txAdHash m

instance
  c ~ Crypto era =>
  HasField "scriptIntegrityHash" (TxBody era) (StrictMaybe (ScriptIntegrityHash c))
  where
  getField (TxBodyConstr (Memo m _)) = txScriptIntegrityHash m

instance HasField "txnetworkid" (TxBody era) (StrictMaybe Network) where
  getField (TxBodyConstr (Memo m _)) = txNetworkid m

instance (Era era, Core.Value era ~ val, Compactible val) => HasField "value" (TxOut era) val where
  getField = \case
    TxOutCompact' _ cv -> fromCompact cv
    TxOutCompactDH' _ cv _ -> fromCompact cv
    TxOutCompactDatum _ cv _ -> fromCompact cv
    TxOutCompactRefScript _ cv _ _ -> fromCompact cv
    TxOut_AddrHash28_AdaOnly _ _ cc -> inject (fromCompact cc)
    TxOut_AddrHash28_AdaOnly_DataHash32 _ _ cc _ -> inject (fromCompact cc)

instance (Era era, c ~ Crypto era) => HasField "datahash" (TxOut era) (StrictMaybe (DataHash c)) where
  getField = maybeToStrictMaybe . txOutDataHash

instance (Era era) => HasField "datum" (TxOut era) (StrictMaybe (Data era)) where
  getField = maybeToStrictMaybe . txOutData

instance (Era era, s ~ Core.Script era) => HasField "referenceScript" (TxOut era) (StrictMaybe s) where
  getField = maybeToStrictMaybe . txOutScript

getBabbageTxOutEitherAddr ::
  HashAlgorithm (CC.ADDRHASH (Crypto era)) =>
  TxOut era ->
  Either (Addr (Crypto era)) (CompactAddr (Crypto era))
getBabbageTxOutEitherAddr = \case
  TxOutCompact' cAddr _ -> Right cAddr
  TxOutCompactDH' cAddr _ _ -> Right cAddr
  TxOutCompactRefScript cAddr _ _ _ -> Right cAddr
  TxOutCompactDatum cAddr _ _ -> Right cAddr
  TxOut_AddrHash28_AdaOnly stakeRef addr28Extra _
    | Just addr <- decodeAddress28 stakeRef addr28Extra -> Left addr
    | otherwise -> error "Impossible: Compacted an address of non-standard size"
  TxOut_AddrHash28_AdaOnly_DataHash32 stakeRef addr28Extra _ _
    | Just addr <- decodeAddress28 stakeRef addr28Extra -> Left addr
    | otherwise -> error "Impossible: Compacted an address or a hash of non-standard size"

txOutData :: TxOut era -> Maybe (Data era)
txOutData = \case
  TxOutCompact' {} -> Nothing
  TxOutCompactDH' {} -> Nothing
  TxOutCompactDatum _ _ binaryData -> Just $! binaryDataToData binaryData
  TxOutCompactRefScript _ _ (Datum binaryData) _ -> Just $! binaryDataToData binaryData
  TxOutCompactRefScript _ _ _ _ -> Nothing
  TxOut_AddrHash28_AdaOnly {} -> Nothing
  TxOut_AddrHash28_AdaOnly_DataHash32 {} -> Nothing

-- | Return the data hash of a given transaction output, if one is present.
--  Note that this function does *not* return the hash of any inline datums
--  that are present.
txOutDataHash :: Era era => TxOut era -> Maybe (DataHash (Crypto era))
txOutDataHash = \case
  TxOutCompact' {} -> Nothing
  TxOutCompactDH' _ _ dh -> Just dh
  TxOutCompactDatum _ _ _ -> Nothing
  TxOutCompactRefScript _ _ datum _ ->
    case datum of
      NoDatum -> Nothing
      DatumHash dh -> Just dh
      Datum _d -> Nothing
  TxOut_AddrHash28_AdaOnly {} -> Nothing
  TxOut_AddrHash28_AdaOnly_DataHash32 _ _ _ dataHash32 -> decodeDataHash32 dataHash32

txOutScript :: TxOut era -> Maybe (Core.Script era)
txOutScript = \case
  TxOutCompact' {} -> Nothing
  TxOutCompactDH' {} -> Nothing
  TxOutCompactDatum {} -> Nothing
  TxOutCompactRefScript _ _ _ s -> Just s
  TxOut_AddrHash28_AdaOnly {} -> Nothing
  TxOut_AddrHash28_AdaOnly_DataHash32 {} -> Nothing
