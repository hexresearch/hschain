-- |
module TM.RealNetwork ( realNetPair
                      , realTlsNetPair ) where

import System.Random
import Thundermint.P2P.Network
import Thundermint.P2P.NetworkTls

import qualified Network.Socket as Net
import qualified Network.TLS    as TLS
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


credentials :: IO (Either String TLS.Credential)
credentials = TLS.credentialLoadX509 "certs/certificate.pem" "certs/key.pem"


realTlsNetPair :: Net.HostName
               -> IO ((Net.SockAddr, NetworkAPI Net.SockAddr),
                      (Net.SockAddr, NetworkAPI Net.SockAddr))
realTlsNetPair  host = do
    n <- randomRIO (200, 299 :: Int)
    cs <- credentials
    let cred = case cs of
                  Right c -> c
                  Left e  -> error $ "error on loading credentials: " ++ e
    let port1 = "300" ++ show  n
        port2 = "300" ++ show  (n+1)
        server = realNetworkTls cred port1
        client = realNetworkTls cred port2
        hints = Net.defaultHints  { Net.addrSocketType = Net.Stream }
    addr1:_ <- Net.getAddrInfo (Just hints) (Just host) (Just port1)
    addr2:_ <- Net.getAddrInfo (Just hints) (Just host) (Just port2)

    let sockAddr1 = Net.addrAddress addr1
    let sockAddr2 = Net.addrAddress addr2

    return ( (sockAddr1, server)
           , (sockAddr2, client)
           )
