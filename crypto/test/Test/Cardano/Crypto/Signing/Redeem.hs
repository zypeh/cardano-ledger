{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Crypto.Signing.Redeem
  ( tests
  )
where

import Cardano.Prelude

import Hedgehog
  (Gen, Property, assert, checkParallel, discover, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Cardano.Crypto.Signing (SignTag(..))
import Cardano.Crypto.Signing.Redeem
  (redeemSign, redeemToPublic, verifyRedeemSig)

import Test.Cardano.Crypto.Dummy (dummyProtocolMagicId)
import Test.Cardano.Crypto.Gen
  (genRedeemKeypair, genRedeemPublicKey, genRedeemSecretKey)


--------------------------------------------------------------------------------
-- Main Test Action
--------------------------------------------------------------------------------

tests :: IO Bool
tests = checkParallel $$discover


--------------------------------------------------------------------------------
-- Redeem Signature Properties
--------------------------------------------------------------------------------

-- | Signing and verification with a redeem keys works
prop_redeemSign :: Property
prop_redeemSign = property $ do
  (pk, sk) <- forAll genRedeemKeypair
  a        <- forAll genData

  assert
    $ verifyRedeemSig dummyProtocolMagicId SignForTestingOnly pk a
    $ redeemSign dummyProtocolMagicId SignForTestingOnly sk a

-- | Signing fails when the wrong 'RedeemPublicKey' is used
prop_redeemSignDifferentKey :: Property
prop_redeemSignDifferentKey = property $ do
  sk <- forAll genRedeemSecretKey
  pk <- forAll $ Gen.filter (/= redeemToPublic sk) genRedeemPublicKey
  a  <- forAll genData

  assert
    . not
    $ verifyRedeemSig dummyProtocolMagicId SignForTestingOnly pk a
    $ redeemSign dummyProtocolMagicId SignForTestingOnly sk a

-- | Signing fails when then wrong signature data is used
prop_redeemSignDifferentData :: Property
prop_redeemSignDifferentData = property $ do
  (pk, sk) <- forAll genRedeemKeypair
  a        <- forAll genData
  b        <- forAll $ Gen.filter (/= a) genData

  assert
    . not
    $ verifyRedeemSig dummyProtocolMagicId SignForTestingOnly pk b
    $ redeemSign dummyProtocolMagicId SignForTestingOnly sk a

genData :: Gen [Int32]
genData = Gen.list (Range.constant 0 50) (Gen.int32 Range.constantBounded)
