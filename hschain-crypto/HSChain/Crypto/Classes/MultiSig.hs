-- |
-- Attempt to create different API for multisignatures
--
-- Signature schemes which we want to implement:
--
--  * Ed25519 - trivial multisigntures
--
--  * BLS - multisignature aggregation, initial version
--
--  * BLS - multisignature aggregation, later improvement
module HSChain.Crypto.Classes.MultiSig where


import HSChain.Crypto.Classes


-- | N-of-N multisignatures. It's simply N signatures by N known
--   public keys. As such every signature scheme supports N-of-N
--   multisig trivially by bundling signatures. Some signature schemes
--   (e.g. BLS) support space-efficient signature aggregation.
--
--   M-of-N multisignatures could be build from N-of-N multisignatures
--   by aggregating only part of keys and providing set of aggregated
--   keys (N-bit for N keys in signing set)
class CryptoSignNofN alg where
  -- | Set of N signing keys.
  type SigningSet alg
  -- | Aggregated signature.
  type AggSignature alg
  -- | Aggregated public key
  type AggPublicKey alg
  -- | Aggregate signatures
  --
  -- FIXME: We need to ensure that we provided
  --        1) All signatures
  --        2) In required order
  --
  -- FIXME: Do we want to allow incremental aggregation?
  aggregateSignatures :: SigningSet alg -> [Signature alg] -> AggSignature alg
  -- | Aggregate public keys
  aggregatePublibKeys :: SigningSet alg -> AggPublicKey alg
  -- | Create signing set
  --
  -- FIXME: Deal with nonemptyness
  --        - Do we allow empty empty signing set?
  --        - What is semantic of empty set if we do?
  makeSigningSet :: [PublicKey alg] -> SigningSet alg



-- FIXME: typeclass above tries to encode multisignature where N keys
--        are used to sign _same_ message. BLS also has scheme where N
--        keys are used to sign N _different_ messages. Question is
--        whether we can accomodate such scheme? Does it fit into
--        typeclass API?
