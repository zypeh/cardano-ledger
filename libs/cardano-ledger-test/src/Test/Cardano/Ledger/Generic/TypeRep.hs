{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Ledger.Generic.TypeRep where


-- import Cardano.Ledger.Shelley.LedgerState
import Cardano.Ledger.Keys(KeyHash,KeyRole(Staking,StakePool))
import Cardano.Ledger.Credential(Credential)
import Cardano.Ledger.Coin(Coin(..))
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import Data.Set(Set)
import Cardano.Ledger.Era(Era(EraCrypto))
import Data.Word(Word64)
import Test.QuickCheck.Gen(Gen)
import Test.QuickCheck(Arbitrary(..))

import Test.Cardano.Ledger.Core.Arbitrary()
import Test.Cardano.Ledger.Shelley.Serialisation.EraIndepGenerators()
import Cardano.Ledger.Crypto (StandardCrypto)
import Test.Cardano.Ledger.Generic.Proof(Proof(..))
import Test.Cardano.Ledger.Generic.PrettyCore(credSummary,keyHashSummary)
import Cardano.Ledger.BaseTypes(EpochNo)
import Data.Ratio(Rational)
import Cardano.Ledger.PoolParams(PoolParams(ppId))

data Rep era t where
  RationalR :: Rep era Rational
  CoinR :: Rep era Coin
  EpochR :: Rep era EpochNo
  (:->) :: Rep era a -> Rep era b -> Rep era (a -> b)
  MapR :: Rep era a -> Rep era b -> Rep era (Map a b)
  SetR :: Rep era a -> Rep era (Set a)
  CredR :: Rep era (Credential 'Staking (EraCrypto era))
  PoolR :: Rep era (KeyHash 'StakePool (EraCrypto era))
  PoolParamsR :: Rep era (PoolParams (EraCrypto era))
  Word64R :: Rep era Word64

data Known era t where
  Mono :: t -> Known era t
  PolyC :: (t (EraCrypto era)) -> Known era (t (EraCrypto era))
  PolyE :: t era -> Known era (t era)

data V era t where V :: String -> Rep era t -> V era t

data Spec era t where
  Fixed :: Rep era t -> Known era t -> Spec era t
  Generate :: Rep era t -> Gen t -> Spec era t
  Var :: V era t -> Spec era t
  MapDomEql :: Spec era (Map dom r1) -> Spec era (Map dom r2) -> Spec era ()
  DomMapSubset :: Spec era (Map dom r1) -> Spec era (Map dom r2) -> Spec era ()
  DomSetSubset :: Spec era (Map dom r1) -> Spec era (Set dom) -> Spec era ()
  SumsTo :: Summable r => Spec era r -> [Sum era r] -> Spec era ()
  Sized :: Sizeable t => Spec era Word64 -> Spec era t -> Spec era ()

-- ===========================================================
-- Constraints

constraints :: [Spec era ()]
constraints =
 [ SumsTo deposits [SumMap keyDeposits, SumMap poolDeposits]
 , MapDomEql rewards keyDeposits
 , DomMapSubset delegations rewards
 , MapDomEql regPools poolDeposits
 , DomMapSubset retiring regPools
 , MapDomEql regPools poolDistr
 , SumsTo (Fixed RationalR (Mono 1)) [SumMap poolDistr]
 , SumsTo totalAda
     [One utxoCoin,
      One treasury,
      One reserves,
      One fees,
      SumMap keyDeposits,
      SumMap poolDeposits,
      SumMap rewards]    
  , DomSetSubset rewards creds
  , DomSetSubset delegations creds
  , DomSetSubset keyDeposits creds
  , DomMapSubset regPools pools
  , DomMapSubset retiring pools
  , DomMapSubset poolDeposits pools
  , DomMapSubset poolDistr pools
  , Sized (Fixed Word64R (Mono 10)) creds
  , Sized (Fixed Word64R (Mono 10)) pools
  ]

-- ===========================================================

treasury :: Spec era Coin
treasury = Var $ V "treasury" CoinR

reserves :: Spec era Coin
reserves = Var $ V "reserves" CoinR

-- utxo = Var $ V "utxo" (MapR TxInR TxOutR)

deposits :: Spec era Coin
deposits = Var $ V "deposits" CoinR

fees :: Spec era Coin
fees = Var $ V "fees" CoinR

rewards :: Spec era (Map (Credential 'Staking (EraCrypto era)) Coin)
rewards = Var $ V "rewards" (MapR CredR CoinR)

delegations :: Spec era (Map (Credential 'Staking (EraCrypto era)) (KeyHash 'StakePool (EraCrypto era)))
delegations = Var $ V "delegations" (MapR CredR PoolR)

keyDeposits :: Spec era (Map (Credential 'Staking (EraCrypto era)) Coin)
keyDeposits = Var $ V "keyDeposits" (MapR CredR CoinR)

poolDeposits :: Spec era (Map (KeyHash 'StakePool (EraCrypto era)) Coin)
poolDeposits = Var $ V "poolDeposits" (MapR PoolR CoinR)

poolDistr:: Spec era (Map (KeyHash 'StakePool (EraCrypto era)) Rational)
poolDistr = Var $ V "poolDistr" (MapR PoolR RationalR)

retiring :: Spec era (Map (KeyHash 'StakePool (EraCrypto era)) EpochNo)
retiring = Var $ V "retiring" (MapR PoolR EpochR)

regPools :: Spec era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
regPools = Var $ V "regPools" (MapR PoolR PoolParamsR)

totalAda :: Spec era Coin
totalAda = Var $ V "totalAda" CoinR

utxoCoin :: Spec era Coin
utxoCoin = Var $ V "utxoCoin" CoinR

creds :: Spec era (Set (Credential 'Staking (EraCrypto era)))
creds = Var $ V "creds" (SetR CredR)

pools :: Spec era (Map (KeyHash 'StakePool (EraCrypto era)) (PoolParams (EraCrypto era)))
pools = Var $ V "pools" (MapR PoolR PoolParamsR)


-- ===========================================================
-- Proof of Rep equality

data Eql a b where Refl :: Eql a a

testEql :: Rep e a -> Rep e b -> Maybe (Eql a b)
testEql CoinR CoinR = Just Refl
testEql (a :-> b) (x :-> y) = do
  Refl <- testEql a x
  Refl <- testEql b y
  Just Refl
testEql (MapR a b) (MapR x y) = do
  Refl <- testEql a x
  Refl <- testEql b y
  Just Refl
testEql (SetR a) (SetR b) = do
  Refl <- testEql a b
  Just Refl  
testEql CredR CredR = Just Refl
testEql EpochR EpochR = Just Refl
testEql RationalR RationalR = Just Refl
testEql PoolR PoolR = Just Refl
testEql PoolParamsR PoolParamsR = Just Refl
testEql Word64R Word64R = Just Refl
testEql _ _ = Nothing

-- ============================================================
-- Show instances

instance Show (Rep era t) where
  show CoinR = "Coin"
  show (a :-> b) = show a++" -> "++show b
  show (MapR a b) = "(Map "++show a++" "++show b++")"
  show (SetR a) = "(Set "++show a++" "++")"
  show CredR = "Cred" -- "(Credential 'Staking c)"
  show PoolR = "Pool" -- "(KeyHash 'StakePool c)"
  show PoolParamsR = "(PoolParams c)"
  show EpochR = "EpochNo"
  show RationalR = "Rational"
  show Word64R = "Word64"

synopsis :: forall e t. Rep e t -> t -> String
synopsis RationalR r = show r
synopsis CoinR c = show c
synopsis EpochR e = show e
synopsis (a :-> b) _ = "(Arrow "++show a++" "++show b++")"
synopsis Word64R w = show w
synopsis (MapR a b) mp = case Map.toList mp of
  [] -> "(empty::Map "++show a++" "++show b++")"
  ((d,r):_) -> "{"++synopsis a d++" -> "++synopsis b r++" | "++show(Map.size mp)++"}"
synopsis CredR c = show(credSummary c)
synopsis PoolR k = "(KeyHash 'PoolStake "++show(keyHashSummary k)++")"
synopsis PoolParamsR pp = "(PoolParams "++synopsis @e PoolR (ppId pp)++")"

instance Show (Spec era t) where
   show (Fixed r (Mono k)) = "(Fixed "++synopsis r k++")"
   show (Fixed r (PolyC k)) = "(Fixed "++synopsis r k++")"
   show (Fixed r (PolyE k)) = "(Fixed "++synopsis r k++")"      
   show (Generate r _) = "(Gen "++show r++")"
   show (Var (V nm rep)) = nm++"::"++show rep
   show (MapDomEql x y) = "(dom "++show x++" == dom "++show y++")"
   show (DomMapSubset x y) = "(dom "++show x++" ⊆ dom "++show y++")"
   show (DomSetSubset x y) = "(dom "++show x++" ⊆ "++show y++")"
   show (Sized n t) = "(Sized "++show n++" "++show t++")"
   show (SumsTo c m) = "("++show c++" == "++showList m++")"
     where showList [] = ""
           showList [t]  = show t
           showList (t : ts) = show t++" + " ++ showList ts         
   showList xs ans = unlines (ans : (map show xs))


-- ===========================================================
-- Sum Class
  
class Show t => Summable t where
  zeroSum :: t
  plusSum :: t -> t -> t

instance Summable Int where
  zeroSum = 0
  plusSum = (+)

instance Summable Coin where
  zeroSum = Coin 0
  plusSum = (<>)

instance Summable Rational where
  zeroSum = 0
  plusSum = (+)

data Sum era t where
  SumMap :: Summable rng => Spec era (Map dom rng) -> Sum era rng
  SumSet :: Summable rng => Spec era (Set rng) -> Sum era rng
  One :: Summable t => Spec era t -> Sum era t

instance Show (Sum era t) where
  show (SumMap x) = "sum "++show x
  show (SumSet x) = "sum "++show x
  show (One x) = show x

-- ===========================================================
-- Sizeable Class
  
class Show t => Sizeable t where

instance (Show dom, Show rng) => Sizeable (Map dom rng) where

instance Show t => Sizeable (Set t) where