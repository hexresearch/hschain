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
          ], type-class-roots = True }
