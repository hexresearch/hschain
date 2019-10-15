{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module HSChain.Crypto.BLS where


import Control.DeepSeq         (NFData(..))
import Control.Monad.IO.Class
import Data.Data               (Data)
import Data.Ord                (comparing)
import System.Entropy
import System.IO.Unsafe

import qualified Crypto.Bls as Bls
import HSChain.Crypto


data BLS     deriving (Data)

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
        PublicKey $ unsafePerformIO $ Bls.aggregateInsecurePublicKeyList (map (\(PublicKey pk) -> pk) keys)
    aggregatePrivKeys keys =
        PrivKey   $ unsafePerformIO $ Bls.aggregateInsecurePrivateKeyList (map (\(PrivKey pk) -> pk) keys)
    aggregateSignatures sigs =
        Signature $ unsafePerformIO $
            mapM (\(Signature sig) -> Bls.deserializeInsecureSignature sig) sigs
            >>= Bls.aggregateInsecureSignaturesList
            >>= Bls.serializeInsecureSignature


instance CryptoAsymmetric BLS where
    publicKey (PrivKey privKey) = PublicKey $ unsafePerformIO $ Bls.getPublicKey privKey
    generatePrivKey = PrivKey <$> liftIO (getEntropy 32 >>= Bls.fromSeed)


instance CryptoSignHashed BLS where
    signHash (PrivKey privKey) (Hash hashbs) = Signature $
        unsafePerformIO $
            Bls.signInsecurePrehashed privKey (Bls.Hash256 hashbs)
            >>= Bls.serializeInsecureSignature
    verifyHashSignature (PublicKey pubKey) (Hash hashbs) (Signature bssig) =
        unsafePerformIO $ do
            sig <- Bls.deserializeInsecureSignature bssig
            Bls.verifyInsecure1 sig (Bls.Hash256 hashbs) pubKey


instance CryptoSign BLS where
    signBlob pk blob = signHash pk (hashBlob blob)
    verifyBlobSignature pubK blob sig = verifyHashSignature pubK (hashBlob blob) sig


instance CryptoHash BLS where
    hashBlob = Hash . Bls.unHash256 . unsafePerformIO . Bls.hash256


instance Eq (PrivKey BLS) where
    (PrivKey pk1) == (PrivKey pk2) = Bls.equalPrivateKey pk1 pk2


instance Eq (PublicKey BLS) where
    (PublicKey pk1) == (PublicKey pk2) = Bls.equalPublicKey pk1 pk2

instance Ord (PrivKey BLS) where
  compare = comparing encodeToBS


instance Ord (PublicKey BLS) where
  compare = comparing encodeToBS

instance NFData (PrivKey BLS) where
  rnf k = k `seq` () -- TODO ???


instance NFData (PublicKey BLS) where
    rnf k = k `seq` ()


instance (Ord (PrivKey BLS)) => ByteRepr (PrivKey BLS) where
    decodeFromBS bs = unsafePerformIO $ (Just . PrivKey) <$> Bls.deserializePrivateKey bs
    encodeToBS (PrivKey privKey) = unsafePerformIO $ Bls.serializePrivateKey privKey


instance (Ord (PublicKey BLS)) => ByteRepr (PublicKey BLS) where
    decodeFromBS bs = unsafePerformIO $ (Just . PublicKey) <$> Bls.deserializePublicKey bs
    encodeToBS (PublicKey pubKey) = unsafePerformIO $ Bls.serializePublicKey pubKey

