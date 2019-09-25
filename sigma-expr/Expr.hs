{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.Bits
import Data.List (find)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteArray          as BA
import Data.Coerce
import qualified Crypto.ECC.Edwards25519 as Ed
import qualified Crypto.Hash.Algorithms  as Hash
import qualified Crypto.Hash             as Hash
import qualified Crypto.Random.Types     as RND
import Crypto.Error
import GHC.Generics (Generic)

import Text.Groom

----------------------------------------------------------------
-- Operations with elliptic curves
----------------------------------------------------------------

-- | Operations with elliptic curve
class EC a where
  data ECPoint   a
  data ECScalar  a
  data Challenge a
  -- Challenge part
  generateChallenge :: IO (Challenge a)
  randomOracle      :: BS.ByteString -> Challenge a
  xorChallenge      :: Challenge a -> Challenge a -> Challenge a

  generateScalar    :: IO (ECScalar a)
  fromGenerator     :: ECScalar  a -> ECPoint  a
  fromChallenge     :: Challenge a -> ECScalar a
  (.+.)   :: ECScalar a -> ECScalar a -> ECScalar a
  (.*.)   :: ECScalar a -> ECScalar a -> ECScalar a

  (.*^)   :: ECScalar a -> ECPoint  a -> ECPoint  a
  -- Group operations
  (^+^)   :: ECPoint  a -> ECPoint  a -> ECPoint  a
  negateP :: ECPoint a -> ECPoint a


data Ed25519

instance EC Ed25519 where
  newtype ECPoint   Ed25519 = ECPoint25519  Ed.Point
  newtype ECScalar  Ed25519 = ECScalar25519 Ed.Scalar
  newtype Challenge Ed25519 = ChallengeEd25519 BS.ByteString
  generateChallenge = ChallengeEd25519 <$> RND.getRandomBytes 31
  randomOracle
    = ChallengeEd25519
    . BS.take 31
    . BA.convert
    . Hash.hash @_ @Hash.SHA256
  xorChallenge (ChallengeEd25519 a) (ChallengeEd25519 b)
    = ChallengeEd25519
    $ BS.pack
    $ BS.zipWith xor a b

  generateScalar    = coerce (Ed.scalarGenerate @IO)
  fromGenerator     = coerce Ed.toPoint
  -- FIXME: We need to maintain that challenge is less than group
  --        module, right?
  fromChallenge (ChallengeEd25519 bs) =
    case Ed.scalarDecodeLong $ BS.take 31 bs of
      CryptoPassed x -> ECScalar25519 x
      CryptoFailed e -> error (show e)
  (.+.)   = coerce Ed.scalarAdd
  (.*.)   = coerce Ed.scalarMul
  (^+^)   = coerce Ed.pointAdd
  (.*^)   = coerce Ed.pointMul
  negateP = coerce Ed.pointNegate



deriving instance Show (ECPoint   Ed25519)
deriving instance Show (ECScalar  Ed25519)
deriving instance Show (Challenge Ed25519)
deriving instance Show (Secret    Ed25519)
deriving instance Eq   (ECPoint Ed25519)
instance CBOR.Serialise (ECPoint Ed25519) where
  encode = CBOR.encode . id @BS.ByteString . Ed.pointEncode . coerce
  decode = undefined

fiatShamirCommitment :: (EC a, CBOR.Serialise b) => b -> Challenge a
fiatShamirCommitment = randomOracle . BL.toStrict . CBOR.serialise

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
                          , pproofR  = r
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
      Leaf Simulated k      -> Leaf Simulated . Right <$> simulateProofDL k ch
      AND  Simulated es     -> AND  Simulated <$> traverse (goSim ch) es
      OR   Simulated []     -> error "Empty OR"
      OR   Simulated (e:es) -> do esWithCh <- forM es $ \x -> (,x) <$> generateChallenge
                                  let ch0 = foldl xorChallenge ch $ map fst esWithCh
                                  OR Simulated <$> traverse (uncurry goSim) ((ch0,e) : esWithCh)
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
  :: forall a. (EC a, Eq (ECPoint a), CBOR.Serialise (ECPoint a))
  => Env a
  -> SigmaE ProofVar (Either (PartialProof a) (ProofDL a))
  -> IO (SigmaE () (ProofDL a))
generateProofs (Env env) expr0 = goReal ch0 expr0
  where
    ch0 :: Challenge a
    ch0 = fiatShamirCommitment $ toFiatShamir expr0
    --
    goReal ch = \case
      Leaf Real (Left PartialProof{..}) -> do
        let e = fromChallenge ch
            [Secret sk] = [ secretKey | KeyPair{..} <- env
                                      , pproofPK == publicKey
                                      ]
            z = pproofR .+. (sk .*. e)
        return $ Leaf () $ ProofDL { publicK     = pproofPK
                                   , commitmentA = pproofA
                                   , challengeE  = ch
                                   , responceZ   = z
                                   }
      Leaf Simulated (Right e)               -> return $ Leaf () e
      AND  Real      es -> AND () <$> traverse (goReal ch) es
      AND  Simulated es -> undefined
      OR   Real      es -> OR  () <$> undefined
      OR   Simulated es -> undefined

-- Partial proof of possession of discrete logarithm
data PartialProof a = PartialProof
  { pproofPK :: PublicKey a
  , pproofR  :: ECScalar  a
  , pproofA  :: ECPoint   a
  }
deriving instance ( Show (ECPoint   a)
                  , Show (Secret    a)
                  , Show (ECScalar a)
                  , Show (Challenge a)
                  ) => Show (PartialProof a)

-- | Proof of knowledge of discrete logarithm
data ProofDL a = ProofDL
  { publicK     :: PublicKey a
  , commitmentA :: ECPoint   a
  , responceZ   :: ECScalar  a
  , challengeE  :: Challenge a
  }

deriving instance ( Show (ECPoint   a)
                  , Show (ECScalar  a)
                  , Show (Challenge a)
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
simulateProofDL :: EC a => PublicKey a -> Challenge a -> IO (ProofDL a)
simulateProofDL pk e = do
  z <- generateScalar
  return ProofDL
    { publicK     = pk
    , commitmentA = fromGenerator z ^+^ negateP (fromChallenge e .*^ unPublicKey pk)
    , responceZ   = z
    , challengeE  = e
    }

verifyProofDL :: (EC a, Eq (ECPoint a)) => ProofDL a -> Bool
verifyProofDL ProofDL{..}
  = fromGenerator responceZ == (commitmentA ^+^ (fromChallenge challengeE .*^ unPublicKey publicK))
--   where
--     -- Recompute challenge
--     ch :: Hash.Digest Hash.SHA256
--     ch = Hash.hashFinalize
--        $ flip Hash.hashUpdate (Ed.pointEncode (unShare commitmentA) :: BA.Bytes)
--        $ flip Hash.hashUpdate (Ed.pointEncode (unShare publicK)     :: BA.Bytes)
--        $ Hash.hashInit
--     CryptoPassed e = Ed.scalarDecodeLong $ BS.tail $ BA.convert ch

verifyProof
  :: (EC a, Eq (ECPoint a), CBOR.Serialise (ECPoint a))
  => SigmaE () (ProofDL a) -> Bool
-- FIXME: we don't verify that challenge comes from Fiat-Shamir oracle
verifyProof = \case
  Leaf () p  -> verifyProofDL p
  OR   () es -> all verifyProof es
  AND  () es -> all verifyProof es



go = do
  kp1 :: KeyPair Ed25519 <- generateKeyPair
  kp2 :: KeyPair Ed25519 <- generateKeyPair
  kp3 :: KeyPair Ed25519 <- generateKeyPair
  kp4 :: KeyPair Ed25519 <- generateKeyPair
  -- Expression to prove
  let leaf = Leaf () . publicKey
  let expr = OR () [ leaf kp1
                    , leaf kp2
                    ]
      env  = Env [kp1,kp2,kp3]
  -- Generating proof
  let marked = markTree env expr
  commited <- generateCommitments marked
  proof    <- generateProofs env commited
  --
  putStrLn $ groom expr
  putStrLn ""
  putStrLn $ groom marked
  putStrLn ""
  putStrLn $ groom commited
  putStrLn ""
  putStrLn $ groom proof
  putStrLn ""
  putStrLn $ groom $ verifyProof proof

