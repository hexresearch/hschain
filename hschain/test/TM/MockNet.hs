-- |
module TM.MockNet ( mockNetPair) where


import HSChain.P2P
import HSChain.P2P.Network

----------------------------------------------------------------

mockNetPair :: IO ( (NetAddr, NetworkAPI)
                  , (NetAddr, NetworkAPI))
mockNetPair = do
  network <- newMockNet
  return ( (a1, createMockNode network a1)
         , (a2, createMockNode network a2)
         )
  where
    a1 = NetAddrV4 40001 2222
    a2 = NetAddrV4 10004 2222
