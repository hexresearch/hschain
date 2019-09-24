{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

import qualified Codec.Serialise as CBOR
import Control.Monad
import qualified Data.ByteString         as BS
import qualified Data.ByteArray          as BA
import Data.Coerce
import qualified Crypto.ECC.Edwards25519 as Ed
import qualified Crypto.Hash.Algorithms  as Hash
import qualified Crypto.Hash             as Hash
import Crypto.Error
import GHC.Generics (Generic)


----------------------------------------------------------------
-- Operations with elliptic curves
----------------------------------------------------------------

-- | Operations with elliptic curve
class EC a where
  data ECPoint  a
  data ECScalar a
  generateScalar :: IO (ECScalar a)
  fromGenerator :: ECScalar a -> ECPoint a
  fromChallenge :: Challenge  -> ECScalar a
  (.+.)   :: ECScalar a -> ECScalar a -> ECScalar a
  (.*.)   :: ECScalar a -> ECScalar a -> ECScalar a

  (.*^)   :: ECScalar a -> ECPoint  a -> ECPoint  a
  (^+^)   :: ECPoint  a -> ECPoint  a -> ECPoint  a
  negateP :: ECPoint a -> ECPoint a


data Ed25519

instance EC Ed25519 where
  newtype ECPoint  Ed25519 = ECPoint25519  Ed.Point
  newtype ECScalar Ed25519 = ECScalar25519 Ed.Scalar
  generateScalar = coerce (Ed.scalarGenerate @IO)
  fromGenerator  = coerce Ed.toPoint
  -- FIXME: We need to maintain that challenge is less than group
  --        module, right?
  fromChallenge (Challenge bs) =
    case Ed.scalarDecodeLong $ BS.take 31 bs of
      CryptoPassed x -> ECScalar25519 x
      CryptoFailed e -> error (show e)
  (.+.)   = coerce Ed.scalarAdd
  (.*.)   = coerce Ed.scalarMul
  (^+^)   = coerce Ed.pointAdd
  (.*^)   = coerce Ed.pointMul
  negateP = coerce Ed.pointNegate

newtype Challenge = Challenge BS.ByteString
  deriving (Show)

deriving instance Show (ECPoint  Ed25519)
deriving instance Show (ECScalar Ed25519)
deriving instance Eq   (ECPoint Ed25519)
instance CBOR.Serialise (ECPoint Ed25519) where
  encode = undefined
  decode = undefined

generateChallenge :: IO Challenge
generateChallenge = undefined

xorChallenge :: Challenge -> Challenge -> Challenge
xorChallenge = undefined

fiatShamirCommitment :: (CBOR.Serialise a) => a -> Challenge
fiatShamirCommitment = undefined

----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

newtype Secret    a = Secret    { unSecret    :: ECScalar a }
newtype PublicKey a = PublicKey { unPublicKey :: ECPoint  a }

data KeyPair a = KeyPair
  { secretKey :: Secret a
  , publicKey :: PublicKey a
  }

deriving stock   instance Show (ECPoint a) => Show (PublicKey a)
deriving stock   instance Eq   (ECPoint a) => Eq   (PublicKey a)
deriving newtype instance (CBOR.Serialise (ECPoint a)) => CBOR.Serialise (PublicKey a)

generateSecretKey :: EC a => IO (Secret a)
generateSecretKey = coerce generateScalar

getPublicKey :: EC a => Secret a -> PublicKey a
getPublicKey = coerce fromGenerator

generateKeyPair :: EC a => IO (KeyPair a)
generateKeyPair = do
  s <- generateSecretKey
  return $ KeyPair s (getPublicKey s)


----------------------------------------------------------------
-- Σ-expression
----------------------------------------------------------------

-- | Expression that should be proven
data SigmaE k a
  = Leaf k a
    -- ^ Proof of possession of discrete logarithm of point at
    --   elliptic curve
  | AND k [SigmaE k a]
    -- ^ AND connective
  | OR  k [SigmaE k a]
  deriving (Functor, Show)

sexprAnn :: SigmaE k a -> k
sexprAnn = \case
  Leaf k _ -> k
  AND     k _ -> k
  OR      k _ -> k

-- | Proof of sigma expression
data SigmaProof a
  = SProofDL  !(ProofDL a)
  | SProofAND
  | SProofOR

-- | Set of known keys
newtype Env a = Env [KeyPair a]

----------------------------------------------------------------
-- Intermediate data types
----------------------------------------------------------------

-- Whether we create real proof or simulate it
data ProofVar
  = Real
  | Simulated
  deriving (Show,Eq)

-- Mark all nodes according to whether we can produce proof for them
markTree :: (EC a, Eq (ECPoint a)) => Env a -> SigmaE () (PublicKey a) -> SigmaE ProofVar (PublicKey a)
markTree (Env env) = check
  where
    -- Select nodes for which we could provide proof. We may mark more nodes that necessary
    check = \case
      Leaf () k  -> Leaf (if k `elem` knownPK then Real else Simulated) k
      AND  () es -> AND k es'
        where
          es'  = map check es
          k | all ((==Real) . sexprAnn) es' = Real
            | otherwise                     = Simulated
      OR   () es -> OR k es'
        where
          es'  = map check es
          k | any ((==Real) . sexprAnn) es' = Real
            | otherwise                     = Simulated
    knownPK = publicKey <$> env
    -- Reduce proof so that OR node has only necessary number of real children
    clean expr = case expr of
      Leaf{}           -> expr
      AND Simulated es -> AND Simulated $ markSim <$> es
      AND Real      es -> AND Real      $ clean   <$> es
      OR  Simulated es -> OR  Simulated $ markSim <$> es
      OR  Real      es -> OR  Real      $ splitOR es
    -- Mark all nodes as simulated
    markSim = \case
      Leaf _ a  -> Leaf Simulated   a
      AND  _ es -> AND     Simulated $ markSim <$> es
      OR   _ es -> OR      Simulated $ markSim <$> es
    -- Only leave one leaf of OR as real
    splitOR []     = error "Impossible"
    splitOR (e:es) = case sexprAnn e of
      Simulated -> markSim e : splitOR es
      Real      -> clean   e : fmap markSim es

-- Genererate simalated proofs and commitments for real proofs
generateCommitments
  :: (EC a)
  => SigmaE ProofVar (PublicKey a)
  -> IO (SigmaE ProofVar (Either (PartialProof a) (ProofDL a)))
generateCommitments = goReal
  where
    -- Go down expecting real node
    goReal = \case
      Leaf Real k -> do r <- generateScalar
                        return $ Leaf Real $ Left $ PartialProof
                          { pproofPK = k
                          , pproofA  = fromGenerator r
                          }
      AND Real es -> AND Real <$> traverse goReal     es
      OR  Real es -> OR  Real <$> traverse simulateOR es
        where
          simulateOR e = case sexprAnn e of
            Real      -> goReal e
            Simulated -> do ch <- generateChallenge
                            goSim ch e
      _ -> error "Simulated node!"
    --
    goSim ch = \case
      Leaf Simulated k  -> Leaf Simulated . Right <$> simulateProofDL k ch
      AND  Simulated es -> AND  Simulated <$> traverse (goSim ch) es
      OR   Simulated es -> undefined
      _ -> error "Real node"

toFiatShamir
  :: SigmaE k (Either (PartialProof a) (ProofDL a))
  -> FiatShamir a
toFiatShamir = \case
  Leaf _ (Left  PartialProof{..}) -> FSDLog pproofPK pproofA
  Leaf _ (Right ProofDL{..})      -> FSDLog publicK  commitmentA
  AND  _ es -> FSAnd (toFiatShamir <$> es)
  OR   _ es -> FSOr  (toFiatShamir <$> es)

generateProofs
  :: (EC a, CBOR.Serialise (ECPoint a))
  => SigmaE ProofVar (Either (PartialProof a) (ProofDL a))
  -> IO (SigmaE () (ProofDL a))
generateProofs expr0 = goReal ch0 expr0
  where
    ch0 = fiatShamirCommitment $ toFiatShamir expr0
    --
    goReal ch = \case
      Leaf Real      (Left PartialProof{..}) -> undefined -- Compute proof
      Leaf Simulated (Right e)               -> return $ Leaf () e
      AND  Real es -> AND () <$> traverse (goReal ch) es
      AND  Simulated es -> undefined
      OR   Real es -> undefined
      OR   Simulated -> undefined

-- Partial proof of possession of discrete logarithm
data PartialProof a = PartialProof
  { pproofPK :: PublicKey a
  , pproofA  :: ECPoint   a
  }

-- | Proof of knowledge of discrete logarithm
data ProofDL a = ProofDL
  { publicK     :: PublicKey a
  , commitmentA :: ECPoint  a
  , responceZ   :: ECScalar a
  , challengeE  :: Challenge
  }

deriving instance ( Show (ECPoint   a)
                  , Show (ECScalar  a)
                  ) => Show (ProofDL a)

data SimTree a
  = SimDL  (Either (ProofDL a) (PublicKey a))
  | SimOR  [SimTree a] (SimTree a) [SimTree a]
  | SimAND [SimTree a]

-- | Tree that is used as input to Fiat-Shamir hash function
data FiatShamir a
  = FSDLog (PublicKey a) (ECPoint a)
  | FSAnd [FiatShamir a]
  | FSOr  [FiatShamir a]
  deriving (Generic)

instance ( CBOR.Serialise (ECPoint a)
         ) => CBOR.Serialise (FiatShamir a)



----------------------------------------------------------------
-- Primitives for Σ-expressions
----------------------------------------------------------------



-- Simulate proof of posession of discrete logarithm for given
-- challenge
simulateProofDL :: EC a => PublicKey a -> Challenge -> IO (ProofDL a)
simulateProofDL pk e = do
  z <- generateScalar
  return ProofDL
    { publicK     = pk
    , commitmentA = fromGenerator z ^+^ negateP (fromChallenge e .*^ unPublicKey pk)
    , responceZ   = z
    , challengeE  = e
    }

-- -- Generate proof of posession of discrete logarithm for given
-- -- challenge
-- generateProofDL :: EC a => KeyPair a -> Challenge -> IO (ProofDL a)
-- ge

go = do
  kp :: KeyPair Ed25519 <- generateKeyPair
  ProofDL{..} <- simulateProofDL (publicKey kp) (Challenge "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC")
  --
  let z = responceZ
      a = commitmentA
  print $ fromGenerator z == (a ^+^ (fromChallenge challengeE .*^ unPublicKey publicK))


-- --   = Ed.toPoint responceZ == (unShare commitmentA `Ed.pointAdd` (e `Ed.pointMul` unShare publicK))
  return ()
-- generateProofDL :: EC a => [Env a] -> PublicKey a -> IO (ProofDL a)

-- -- Generate proof of posession of discrete logarithm with given
-- -- challenge
-- generateProofDL :: KeyPair a -> Challenge -> IO (ProofDL a)
-- generateProofDL (KeyPair sk pk) ch = do

-- generateProof :: EC a => KeyPair a -> IO (ProofDL a)
-- generateProof (KeyPair sk pk) = do
--   -- Choose commitment
--   r <- generateScalar
--   let a = fromGenerator r
--   -- Compute challenge
--   let ch = undefined :: Challenge
--   -- let ch :: Hash.Digest Hash.SHA256
--       -- ch = undefined
--       e = fromChallenge ch
--       z = r .+. (unSecret sk .*. e)
--   return ProofDL
--     { publicK     = pk
--     , commitmentA = a
--     , responceZ   = z
--     }
-- --       ch = Hash.hashFinalize
-- --          $ flip Hash.hashUpdate (Ed.pointEncode a   :: BA.Bytes)
-- --          $ flip Hash.hashUpdate (Ed.pointEncode pub :: BA.Bytes)
-- --          $ Hash.hashInit
-- --   let CryptoPassed e = Ed.scalarDecodeLong $ BS.tail $ BA.convert ch
-- --       z              = r `Ed.scalarAdd` (w `Ed.scalarMul` e)
-- --   return ProofDL
-- --     { publicK     = Share pub
-- --     , commitmentA = Share a
-- --     , responceZ   = z
-- --     }
-- --   where
-- --     pub = Ed.toPoint w


-- -- -- | Secret possessed by prover
-- -- newtype Secret = Secret Ed.Scalar

-- -- -- | Value published by prover
-- -- newtype Share = Share { unShare :: Ed.Point }

-- -- -- | Proof of knowledge of discrete logarithm
-- -- data ProofDL = ProofDL
-- --   { publicK     :: Share        --
-- --   , commitmentA :: Share        --
-- --   , responceZ   :: Ed.Scalar
-- --   }



-- -- verifyProof :: ProofDL -> Bool
-- -- verifyProof ProofDL{..}
-- --   = Ed.toPoint responceZ == (unShare commitmentA `Ed.pointAdd` (e `Ed.pointMul` unShare publicK))
-- --   where
-- --     -- Recompute challenge
-- --     ch :: Hash.Digest Hash.SHA256
-- --     ch = Hash.hashFinalize
-- --        $ flip Hash.hashUpdate (Ed.pointEncode (unShare commitmentA) :: BA.Bytes)
-- --        $ flip Hash.hashUpdate (Ed.pointEncode (unShare publicK)     :: BA.Bytes)
-- --        $ Hash.hashInit
-- --     CryptoPassed e = Ed.scalarDecodeLong $ BS.tail $ BA.convert ch


-- -- go = do
-- --   sk <- Ed.scalarGenerate
-- --   proof <- generateProof (Secret sk)
-- --   print $ verifyProof proof
