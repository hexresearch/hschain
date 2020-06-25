{ roots = [ "^Main.main$"
          , "^Paths_.*"
          , "HSChain.Crypto.*Size$"
          , "HSChain.Crypto.make.*Box"
          , "HSChain.Crypto.open.*Box"
          , "HSChain.Crypto.signHashed"
          , "HSChain.Crypto.verifySignatureHashed"
          , "HSChain.Crypto.Classes.Hash.genericHashStep.{0,2}"
          , "HSChain.Crypto.Classes.Hash.hashed"
          , "HSChain.Crypto.PBKDF2Simple.sha512PBKDF2"

          , "HSChain.Types.Merkle.Block.merklize"
          , "HSChain.Types.Merkle.Block.concatTree"
          , "HSChain.Types.Merkle.Block.checkMerkleTree"
          , "HSChain.Types.Merkle.Tree.createMerkleTree"
          , "HSChain.Types.Merkle.Tree.merkleBlockTreeHash"
          , "HSChain.Types.Merkle.Tree.isBalanced"
          , "HSChain.Types.Merkle.Tree.checkMerkleProof"
          , "HSChain.Types.Merkle.Tree.createMerkleProof"
          , "HSChain.Types.Merkle.Types.mapMerkleNode"
          , "HSChain.Types.Merkle.Types.merkleHashed"
          , "HSChain.Types.Merkle.Types.toHashedNode"


          , "HSChain.Types.BFTTime.commitTime"
          , "HSChain.Types.Blockchain.blockHash"
          , "HSChain.Types.Blockchain.getCurrentTime"
          , "HSChain.Types.Blockchain.timeToUTC"
          , "HSChain.Types.Blockchain.toHeader"
          , "HSChain.Types.Blockchain.toHeader"
          , "HSChain.Types.Blockchain.signedValue"
          , "HSChain.Types.Blockchain.signedKeyInfo"
          , "HSChain.Types.Blockchain.signValue"
          , "HSChain.Types.Blockchain.verifySignature"
          , "HSChain.Types.Blockchain.unverifySignature"
          , "HSChain.Types.Validators.emptyValidatorSet"
          , "HSChain.Types.Validators.makeValidatorSet"
          , "HSChain.Types.Validators.totalVotingPower"
          , "HSChain.Types.Validators.validatorSetSize"
          , "HSChain.Types.Validators.validatorByIndex"
          , "HSChain.Types.Validators.asValidatorList"
          , "HSChain.Types.Validators.indexByValidator"
          , "HSChain.Types.Validators.getValidatorIntSet"
          , "HSChain.Types.Validators.insertValidatorIdx"
          , "HSChain.Types.Validators.emptyValidatorISet"
          , "HSChain.Types.Validators.emptyValidatorISetFromSize"
          , "HSChain.Types.Validators.changeValidators"
          , "HSChain.Types.Validators.validatorsDifference"
          , "HSChain.Types.Validators.indexByIntervalPoint"
          ], type-class-roots = True }
