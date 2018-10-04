-- |
module TM.MockNet ( mockNetPair) where


import Thundermint.P2P.Network

import qualified Network.Socket as Net


----------------------------------------------------------------

mockNetPair :: IO ( ((Int, Net.ServiceName), NetworkAPI (Int, Net.ServiceName))
                  , ((Int, Net.ServiceName), NetworkAPI (Int, Net.ServiceName)))
mockNetPair = do
  network <- newMockNet
  return ( ((1,"3000"), createMockNode network "3000" 1)
         , ((2,"3000"), createMockNode network "3000" 2)
         )
