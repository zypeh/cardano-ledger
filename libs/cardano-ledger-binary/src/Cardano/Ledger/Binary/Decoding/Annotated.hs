{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Ledger.Binary.Decoding.Annotated
  ( C.Annotated (..),
    C.ByteSpan (..),
    C.Decoded (..),
    C.annotationBytes,
    annotatedDecoder,
    C.slice,
    fromCBORAnnotated,
    decodeFullAnnotatedBytes,
    reAnnotate,
    C.Annotator (..),
    annotatorSlice,
    decodeAnnotator,
    withSlice,
    C.FullByteString (..),
    C.serializeEncoding,
    encodePreEncoded,
  )
where

import qualified Cardano.Binary as C
import Cardano.Ledger.Binary.Decoding.FromVCBOR (FromVCBOR (..))
import Cardano.Ledger.Binary.Decoding.VDecoder
  ( DecoderError,
    VDecoder,
    decodeFullDecoder,
    fromVDecoder,
    toVDecoder,
  )
import Codec.CBOR.Encoding (encodePreEncoded)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Functor ((<&>))
import Data.Text (Text)
import GHC.TypeLits (KnownNat)
import Numeric.Natural (Natural)

-- | A decoder for a value paired with an annotation specifying the start and end
-- of the consumed bytes.
annotatedDecoder :: KnownNat v => VDecoder v s a -> VDecoder v s (C.Annotated a C.ByteSpan)
annotatedDecoder vd =
  toVDecoder
    ( C.decodeWithByteSpan (fromVDecoder vd)
        <&> \(x, start, end) -> C.Annotated x (C.ByteSpan start end)
    )

-- | A decoder for a value paired with an annotation specifying the start and end
-- of the consumed bytes.
fromCBORAnnotated :: (KnownNat v, FromVCBOR a) => VDecoder v s (C.Annotated a C.ByteSpan)
fromCBORAnnotated = annotatedDecoder fromVCBOR

-- | Decodes a value from a ByteString, requiring that the full ByteString is consumed, and
-- replaces ByteSpan annotations with the corresponding substrings of the input string.
decodeFullAnnotatedBytes ::
  Functor f =>
  Natural ->
  Text ->
  (forall v s. KnownNat v => VDecoder v s (f C.ByteSpan)) ->
  BSL.ByteString ->
  Either DecoderError (f BS.ByteString)
decodeFullAnnotatedBytes v lbl decoder bytes =
  C.annotationBytes bytes <$> decodeFullDecoder v lbl decoder bytes

-- | Reconstruct an annotation by re-serialising the payload to a ByteString.
reAnnotate :: C.ToCBOR a => C.Annotated a b -> C.Annotated a BS.ByteString
reAnnotate (C.Annotated x _) = C.Annotated x (C.serialize' x)

-------------------------------------------------------------------------
-- Annotator
-------------------------------------------------------------------------

-- | The argument is a decoder for a annotator that needs access to the bytes that
-- | were decoded. This function constructs and supplies the relevant piece.
annotatorSlice ::
  KnownNat v =>
  VDecoder v s (C.Annotator (BSL.ByteString -> a)) ->
  VDecoder v s (C.Annotator a)
annotatorSlice dec = do
  (k, bytes) <- withSlice dec
  pure $ k <*> bytes

-- | Pairs the decoder result with an annotator that can be used to construct the exact bytes used to decode the result.
withSlice :: KnownNat v => VDecoder v s a -> VDecoder v s (a, C.Annotator BSL.ByteString)
withSlice dec = do
  C.Annotated r byteSpan <- annotatedDecoder dec
  return (r, C.Annotator (\(C.Full bsl) -> C.slice bsl byteSpan))

-- | Supplies the bytestring argument to both the decoder and the produced annotator.
decodeAnnotator ::
  Natural ->
  Text ->
  (forall v s. KnownNat v => VDecoder v s (C.Annotator a)) ->
  BSL.ByteString ->
  Either DecoderError a
decodeAnnotator v label' decoder bytes =
  (\x -> C.runAnnotator x (C.Full bytes)) <$> decodeFullDecoder v label' decoder bytes