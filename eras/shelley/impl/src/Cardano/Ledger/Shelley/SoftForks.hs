{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Cardano.Ledger.Shelley.SoftForks
  ( validMetadata,
    restrictPoolMetadataHash,
  )
where

import Cardano.Ledger.BaseTypes (ProtVer (..))
import GHC.Records

validMetadata ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
validMetadata _ = True

restrictPoolMetadataHash ::
  (HasField "_protocolVersion" pp ProtVer) =>
  pp ->
  Bool
restrictPoolMetadataHash pp = getField @"_protocolVersion" pp > ProtVer 4 0
