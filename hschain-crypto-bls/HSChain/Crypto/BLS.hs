{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module HSChain.Crypto.BLS where


import Control.DeepSeq         (NFData(..))
import Control.Monad.IO.Class
import Data.ByteString         (ByteString)
import Data.Data               (Data)
import Data.Maybe
import Data.Ord                (comparing)
import System.Entropy

import qualified Crypto.Bls as Bls
import HSChain.Crypto


data BLS deriving (Data)

newtype instance PrivKey   BLS = PrivKey   Bls.PrivateKey
newtype instance PublicKey BLS = PublicKey Bls.PublicKey


instance ByteReprSized (PublicKey BLS) where
  type ByteSize (PublicKey BLS) = 48
instance ByteReprSized (PrivKey BLS) where
  type ByteSize (PrivKey BLS)   = 32
instance ByteReprSized (Signature BLS) where
  type ByteSize (Signature BLS) = 96
instance ByteReprSized (Hash BLS) where
    type ByteSize (Hash BLS)    = 32


instance CryptoAggregabble BLS where
    type AggregatedPublicKey BLS = PublicKey BLS
    type AggregatedPrivKey BLS   = PrivKey BLS
    type AggregatedSignature BLS = Signature BLS
    aggregatePublicKeys keys =
        PublicKey
            $ Bls.publicKeyInsecureAggregate
            $ map (\(PublicKey pk) -> pk) keys
    aggregatePrivKeys keys =
        PrivKey
            $ Bls.privateKeyInsecureAggregate
            $ map (\(PrivKey pk) -> pk) keys
    aggregateSignatures sigs =
        Signature
            $ Bls.insecureSignatureSerialize
            $ Bls.insecureSignatureAggregate
            $ map (\(Signature sig) -> fromJust $ Bls.insecureSignatureDeserialize sig) sigs


instance CryptoAsymmetric BLS where
    publicKey (PrivKey privKey) = PublicKey $ Bls.privateKeyGetPublicKey privKey
    generatePrivKey = PrivKey <$> liftIO (Bls.privateKeyFromSeed <$> (getEntropy 32))
    asymmKeyAlgorithmName = "BLS"


instance CryptoSignHashed BLS where
    signHash (PrivKey privKey) (Hash hashbs) =
        Signature $ Bls.insecureSignatureSerialize
                  $ Bls.signInsecurePrehashed privKey (Bls.Hash256 hashbs)
    verifyHashSignature (PublicKey pubKey) (Hash hashbs) (Signature bssig) =
        let sig = fromJust $ Bls.insecureSignatureDeserialize bssig
        in Bls.insecureSignatureVerify sig [Bls.Hash256 hashbs] [pubKey]


instance CryptoSign BLS where
    signBlob pk blob = signHash pk (hashBlobBLS blob)
    verifyBlobSignature pubK blob sig = verifyHashSignature pubK (hashBlobBLS blob) sig


-- FIXME: decide what to do with BLS hashing
--
-- instance CryptoHash BLS where
--     hashBlob = Hash . Bls.unHash256 . unsafePerformIO . Bls.hash256
hashBlobBLS :: ByteString -> Hash BLS
hashBlobBLS = Hash . Bls.unHash256 . Bls.hash256


instance Eq (PrivKey BLS) where
    (PrivKey pk1) == (PrivKey pk2) = Bls.privateKeyEq pk1 pk2


instance Eq (PublicKey BLS) where
    (PublicKey pk1) == (PublicKey pk2) = Bls.publicKeyEq pk1 pk2


instance Ord (PrivKey BLS) where
  compare = comparing encodeToBS


instance Ord (PublicKey BLS) where
  compare = comparing encodeToBS

instance NFData (PrivKey BLS) where
  rnf k = k `seq` () -- TODO ???


instance NFData (PublicKey BLS) where
    rnf k = k `seq` ()


instance (Ord (PrivKey BLS)) => ByteRepr (PrivKey BLS) where
    decodeFromBS bs = PrivKey <$> Bls.privateKeyDeserialize bs
    encodeToBS (PrivKey privKey) = Bls.privateKeySerialize privKey


instance (Ord (PublicKey BLS)) => ByteRepr (PublicKey BLS) where
    decodeFromBS bs = PublicKey <$> Bls.publicKeyDeserialize bs
    encodeToBS (PublicKey pubKey) = Bls.publicKeySerialize pubKey

