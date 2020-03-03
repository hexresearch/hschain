-- |
-- Data types for network
module HSChain.PoW.Network.Types where


data GossipMsg b
  = GossipPEX  !MsgPEX
  | GossipTX   !(MsgTX b)
  | GossipReq  !(MsgRequest b)
  | GossipResp !(MsgResponce b)
-- | Messages used for peer exchange (PEX)
data MsgPEX
  = PexAskAddr
  | PexAddrList [NetAddr]

-- | Messages used for transmitting unconfirmed transactions
data MsgTX b
  = MsgTxFwd (TX b)


-- | Messages used for exchanging information about blockchain: blocks, headers
data MsgRequest b
  = Req'BestHead
  -- ^ Request hash of head of blockchoin with most work. If node
  --   performs full block validation it should reply with hashes of
  --   completely verified blocks
  | Req'Locator  (BlockID b)
  -- ^ Request locator for given block.
  | Req'Headers  !(BlockID b) !(Maybe (BlockID b))
  -- ^ Request headers from  best chain 
  | Req'Block    (BlockID b)
  -- ^ Request full block with given hash

  

data MsgResponce b
  = Resp'BestHead !(BlockID b)
  | Resp'Locator  !(Locator b)
  | Resp'Headers  [Headers b]
  | Resp'Block    !(Block b)
  | Resp'NACK




data MessageRX b
  = RX'Headers [Header b] -- ^ New headers
  | RX'NewTip  ???
  | RX'Block

data MessageTX b
  = TX'AskBlock
  | TX'Ask
