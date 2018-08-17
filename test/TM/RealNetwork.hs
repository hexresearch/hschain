-- |
module TM.RealNetwork ( realNetPair ) where

import System.Random
import Thundermint.P2P.Network

import qualified Network.Socket as Net

----------------------------------------------------------------

realNetPair :: Net.HostName
            -> IO ((Net.SockAddr, NetworkAPI Net.SockAddr),
                   (Net.SockAddr, NetworkAPI Net.SockAddr))
realNetPair host = do
    n <- randomRIO (0, 99 :: Int)
    let port1 = "300" ++ show  n
        port2 = "300" ++ show  (n+1)
        server = realNetwork port1
        client = realNetwork port2
        hints = Net.defaultHints  { Net.addrSocketType = Net.Stream }
    addr1:_ <- Net.getAddrInfo (Just hints) (Just host) (Just port1)
    addr2:_ <- Net.getAddrInfo (Just hints) (Just host) (Just port2)

    let sockAddr1 = Net.addrAddress addr1
    let sockAddr2 = Net.addrAddress addr2

    return ( (sockAddr1, server)
           , (sockAddr2, client)
           )
