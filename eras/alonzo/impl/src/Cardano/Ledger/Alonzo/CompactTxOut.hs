{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Ledger.Alonzo.CompactTxOut where

import Cardano.Binary (serialize', unsafeDeserialize')
import Cardano.Crypto.Hash.Class
  ( Hash (),
    HashAlgorithm,
    castHash,
    hash,
    hashFromBytesShort,
    hashToBytes,
    hashWith,
  )
import Cardano.Ledger.Address (Addr (..), BootstrapAddress (..))
import Cardano.Ledger.Alonzo.Data (DataHash (..))
import Cardano.Ledger.Alonzo.TxBody (TxBody, TxOut (..))
import Cardano.Ledger.BaseTypes (Network (..), StrictMaybe (..))
import Cardano.Ledger.Compactible (CompactForm, Compactible (..))
import qualified Cardano.Ledger.Core as Core
import Cardano.Ledger.Credential
  ( Credential (..),
    Ix (..),
    PaymentCredential (..),
    Ptr (..),
    StakeCredential (..),
    StakeReference (..),
  )
import qualified Cardano.Ledger.Crypto as CC
import Cardano.Ledger.Era (Crypto, Era)
import Cardano.Ledger.Hashes (ScriptHash (..))
import Cardano.Ledger.Keys (KeyHash (..))
import Cardano.Ledger.Mary.Value (Value (..))
import Cardano.Ledger.SafeHash (SafeHash (..), extractHash, unsafeMakeSafeHash)
import Cardano.Ledger.Shelley.CompactAddr (CompactAddr (..), compactAddr, decompactAddr)
import Cardano.Slotting.Slot (SlotNo (..))
import Data.Bits
  ( Bits,
    clearBit,
    complement,
    popCount,
    setBit,
    shiftL,
    shiftR,
    testBit,
    unsafeShiftL,
    zeroBits,
    (.&.),
    (.|.),
  )
import Data.ByteString (ByteString, index, pack, unpack)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Word (Word16, Word64, Word8)

-- ==================================================
-- Getting to the ByteString that Underlies each Hash

safeHashToBytes :: SafeHash crypto a -> ByteString
safeHashToBytes = hashToBytes . extractHash

bytesToSafeHash :: HashAlgorithm (CC.HASH c) => ByteString -> SafeHash c a
bytesToSafeHash x = unsafeMakeSafeHash (makeHash x)

makeHash :: HashAlgorithm c => ByteString -> Hash c a
makeHash bs = castHash (hashWith id bs)

-- ===============================================

-- | binary encoding of 'n', least significant bit on the front of the list
binary :: Integral n => n -> [n]
binary 0 = []
binary 1 = [(1)]
binary n = (mod n 2) : binary (div n 2)

-- | Show 'n' as a binary number with most significant bits on the left.
bin :: Integral n => n -> [n]
bin x = reverse (binary x)

-- ========================================================
-- More for documentation, than nything else.
-- What 'Pattern' of construction goes with which Tag

labelDataHash :: StrictMaybe (DataHash crypto) -> Int
labelDataHash SNothing = 0
labelDataHash (SJust mhash) = 1

labelValue :: Value crypto -> Int
labelValue (Value 0 m) | Map.null m = 0
labelValue (Value n m) | Map.null m = 1
labelValue (Value n m) = 2

labelAddrShare :: Addr crypto -> Int
labelAddrShare (Addr Testnet (ScriptHashObj hash1) stake) = 0
labelAddrShare (Addr Testnet (KeyHashObj hash1) stake) = 1
labelAddrShare (Addr Mainnet (ScriptHashObj hash1) stake) = 2
labelAddrShare (Addr Mainnet (KeyHashObj hash1) stake) = 3
labelAddrShare (AddrBootstrap byron) = 4

-- ======================================================
makeTag :: Word8 -> Word8 -> Word8 -> Word8
makeTag addr val dhash
  | (addr >= 0 && addr <= 4) && (val >= 0 && val <= 2) && (dhash >= 0 && dhash <= 1) =
    (addr * 8 + val * 2 + dhash) -- (shiftL addr 3 .|. shiftL val 1 .|. dhash)
  | otherwise = error ("tags are not in the correct ranges " ++ show (addr, val, dhash) ++ "in (0-4,0-2,0-1).")

getTags :: Word8 -> (Word8, Word8, Word8)
getTags tag = (addr, val, dhash)
  where
    dhash = mod tag 2
    val = mod (div tag 2) 4
    addr = mod (div tag 8) 8

testTag = [getTags (makeTag addr val dhash) == (addr, val, dhash) | addr <- [0 .. 4], val <- [0 .. 2], dhash <- [0 .. 1]]

-- ===============================================
--  It should be the case that getAddrBytes  and readAddr  are inverses?

getAddrBytes :: Addr crypto -> (Word8, StakeReference crypto, ByteString)
getAddrBytes (Addr Testnet (ScriptHashObj (ScriptHash hash1)) stake) = (0, stake, hashToBytes hash1)
getAddrBytes (Addr Testnet (KeyHashObj (KeyHash hash1)) stake) = (1, stake, hashToBytes hash1)
getAddrBytes (Addr Mainnet (ScriptHashObj (ScriptHash hash1)) stake) = (2, stake, hashToBytes hash1)
getAddrBytes (Addr Mainnet (KeyHashObj (KeyHash hash1)) stake) = (3, stake, hashToBytes hash1)
getAddrBytes (AddrBootstrap byron) = (4, undefined, undefined)

readAddr :: forall c. HashAlgorithm (CC.ADDRHASH c) => Word8 -> Int -> StakeReference c -> ByteString -> (Int, Addr c)
readAddr 0 i stake bs = (i2, Addr Testnet (ScriptHashObj (ScriptHash (makeHash bs2))) stake)
  where
    (i2, bs2) = readByteString 28 i bs
readAddr 1 i stake bs = (i2, Addr Testnet (KeyHashObj (KeyHash (makeHash bs2))) stake)
  where
    (i2, bs2) = readByteString 28 i bs
readAddr 2 i stake bs = (i2, Addr Mainnet (ScriptHashObj (ScriptHash (makeHash bs2))) stake)
  where
    (i2, bs2) = readByteString 28 i bs
readAddr 3 i stake bs = (i2, Addr Mainnet (KeyHashObj (KeyHash (makeHash bs2))) stake)
  where
    (i2, bs2) = readByteString 28 i bs
readAddr 4 i stake bs = undefined

-- ===============================================
--  It should be the case that getValueBytes  and readVal  are inverses?

getValueBytes :: CC.Crypto crypto => Value crypto -> (Word8, ByteString)
getValueBytes (Value 0 m) | Map.null m = (0, mempty)
getValueBytes (Value n m) | Map.null m = (1, word64ToByteString (fromIntegral n))
getValueBytes (v@(Value _ _)) = (2, word16ToByteString n <> valBytes)
  where
    unJust (Just x) = x
    unJust Nothing = error ("Value does not have compact form.")
    valBytes = serialize' (unJust (toCompact v))
    n :: Word16
    n = fromIntegral (BS.length valBytes)

readVal :: forall crypto. CC.Crypto crypto => Word8 -> Int -> ByteString -> (Int, Value crypto)
readVal 0 i bs = (i, Value 0 Map.empty)
readVal 1 i bs = (j, Value (fromIntegral n) Map.empty)
  where
    (j, n) = readWord64 i bs
readVal 2 i bs = (i3, fromCompact (unsafeDeserialize' bytes))
  where
    (i2, n) = readWord16 i bs
    (i3, bytes) = readByteString (fromIntegral n) i2 bs

-- ===================================================
--  It should be the case that getDataHashBytes  and readDataHash  are inverses?

getDataHashBytes :: StrictMaybe (DataHash crypto) -> (Word8, ByteString)
getDataHashBytes SNothing = (0, mempty)
getDataHashBytes (SJust mhash) = (1, safeHashToBytes mhash)

readDataHash :: forall c. HashAlgorithm (CC.HASH c) => Word8 -> Int -> ByteString -> (Int, StrictMaybe (DataHash c))
readDataHash 0 i bs = (i, SNothing)
readDataHash 1 i bs = (i2, SJust (bytesToSafeHash bs2))
  where
    (i2, bs2) = readByteString 28 i bs

-- ===============================================

data CompactTxOut era
  = PostByron !(StakeReference (Crypto era)) !ByteString
  | Byron !(BootstrapAddress (Crypto era)) !ByteString

transTxOut ::
  ( Era era,
    Show (Core.Value era),
    Core.Value era ~ Value (Crypto era)
  ) =>
  TxOut era ->
  CompactTxOut era
transTxOut (TxOut addr val dhash) =
  case addrTag of
    4 -> Byron undefined (valueBytes <> dhashBytes)
    _ -> PostByron stake (tagBytes <> addrBytes <> valueBytes <> dhashBytes)
  where
    (dhashTag, dhashBytes) = getDataHashBytes dhash
    (valueTag, valueBytes) = getValueBytes val
    (addrTag, stake, addrBytes) = getAddrBytes addr
    tagBytes = pack [makeTag addrTag valueTag dhashTag]

decompactTxOut ::
  forall era.
  ( Era era,
    Core.Value era ~ Value (Crypto era)
  ) =>
  CompactTxOut era ->
  TxOut era
decompactTxOut (PostByron stake bytes) = TxOut addr val dhash
  where
    (i1, (addrtag, valtag, dhashtag)) = readTags 0 bytes
    (i2, addr) = readAddr @(Crypto era) addrtag i1 stake bytes
    (i3, val) = readVal @(Crypto era) valtag i2 bytes
    (i4, dhash) = readDataHash @(Crypto era) dhashtag i3 bytes

-- =============================================
showBS :: ByteString -> String
showBS bs = show (unpack bs)

word64ToByteString :: Word64 -> ByteString
word64ToByteString w64 = pack (loop 8 w64 [])
  where
    loop :: Word64 -> Word64 -> [Word8] -> [Word8]
    loop 0 _ ans = ans
    loop cnt n ans = loop (cnt - 1) (div n 256) ((fromIntegral (mod n 256)) : ans)

word16ToByteString :: Word16 -> ByteString
word16ToByteString w16 = pack (loop 2 w16 [])
  where
    loop :: Word16 -> Word16 -> [Word8] -> [Word8]
    loop 0 _ ans = ans
    loop cnt n ans = loop (cnt - 1) (div n 256) ((fromIntegral (mod n 256)) : ans)

readWord8 :: Int -> ByteString -> (Int, Word8)
readWord8 i bs | i > (BS.length bs -1) = error ("Not enough bytes to read a Word8")
readWord8 i bs = (i + 1, index bs i)

readTags :: Int -> ByteString -> (Int, (Word8, Word8, Word8))
readTags i bs | i > (BS.length bs -1) = error ("Not enough bytes to read the Tags")
readTags i bs = (i + 1, getTags (index bs i))

readWord64 :: Int -> ByteString -> (Int, Word64)
readWord64 i bs | i + 7 > BS.length bs = error ("Not enough bytes to read a Word64")
readWord64 i bs = (i + 8, loop 0 0)
  where
    loop :: Int -> Word64 -> Word64
    loop i ans | i >= 8 = ans
    loop i ans = loop (i + 1) (ans * 256 + fromIntegral (index bs i))

readWord16 :: Int -> ByteString -> (Int, Word16)
readWord16 i bs | i + 2 > BS.length bs = error ("Not enough bytes to read a Word16")
readWord16 i bs = (i + 2, loop 0 0)
  where
    loop :: Int -> Word16 -> Word16
    loop i ans | i >= 2 = ans
    loop i ans = loop (i + 1) (ans * 256 + fromIntegral (index bs i))

-- | Read a (sub) ByteString of length 'len', starting at index 'i' from 'bs'
readByteString :: Int -> Int -> ByteString -> (Int, ByteString)
readByteString len i bs
  | i + len > BS.length bs =
    error ("Not enough bytes to read a ByteString of length " ++ show len)
readByteString len i bs = (i + len, BS.take len (BS.drop i bs))
