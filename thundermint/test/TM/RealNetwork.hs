{-# LANGUAGE OverloadedStrings #-}
-- |
module TM.RealNetwork ( realNetPair
                      , realTlsNetPair ) where

import System.Random

import Thundermint.P2P.Network

import qualified Data.ByteString as BS
import qualified Network.Socket  as Net
----------------------------------------------------------------

realNetPair :: Bool
            -> Net.HostName
            -> IO ((Net.SockAddr, NetworkAPI Net.SockAddr),
                   (Net.SockAddr, NetworkAPI Net.SockAddr))
realNetPair useUDP host = do
    n <- randomRIO (10, 89 :: Int)
    m <- randomRIO (1, 9 :: Int)
    let port1 = concat ["30", show m, show  n]
        port2 = concat ["31", show m, show (mod (n + m) 100)]
        realNet = if not useUDP then return . realNetwork else realNetworkUdp
        hints = Net.defaultHints  { Net.addrSocketType = Net.Stream }
    addr1:_ <- Net.getAddrInfo (Just hints) (Just host) (Just port1)
    addr2:_ <- Net.getAddrInfo (Just hints) (Just host) (Just port2)
    server <- realNet port1
    client <- realNet port2

    let sockAddr1 = Net.addrAddress addr1
    let sockAddr2 = Net.addrAddress addr2

    return ( (sockAddr1, server)
           , (sockAddr2, client)
           )


realTlsNetPair :: Net.HostName
               -> IO ((Net.SockAddr, NetworkAPI Net.SockAddr),
                      (Net.SockAddr, NetworkAPI Net.SockAddr))
realTlsNetPair  host = do
    n <- randomRIO (10, 99 :: Int)
    m <- randomRIO (1, 9 :: Int)
    let port1 = concat ["32", show  m,  show  n]
        port2 = concat ["33", show  m, show  (mod (n + m) 100)]
        credential = getCredentialFromBuffer certificatePem keyPem
        server = realNetworkTls credential port1
        client = realNetworkTls credential port2
        hints = Net.defaultHints  { Net.addrSocketType = Net.Stream }
    addr1:_ <- Net.getAddrInfo (Just hints) (Just host) (Just port1)
    addr2:_ <- Net.getAddrInfo (Just hints) (Just host) (Just port2)

    let sockAddr1 = Net.addrAddress addr1
    let sockAddr2 = Net.addrAddress addr2

    return ( (sockAddr1, server)
           , (sockAddr2, client)
           )


-- pem buffers




certificatePem :: BS.ByteString
certificatePem = BS.concat
                 ["-----BEGIN CERTIFICATE-----\n"
                  , "MIIDEjCCAfoCCQDQd3y7+5OM0DANBgkqhkiG9w0BAQsFADBLMQswCQYDVQQGEwJS\n"
                  , "VTEPMA0GA1UEBwwGTW9zY293MQwwCgYDVQQKDANIWFIxDDAKBgNVBAsMA2h4cjEP\n"
                  , "MA0GA1UEAwwGYXJ0aHVyMB4XDTE4MDkyNDE1MDQzN1oXDTI4MDkyMTE1MDQzN1ow\n"
                  , "SzELMAkGA1UEBhMCUlUxDzANBgNVBAcMBk1vc2NvdzEMMAoGA1UECgwDSFhSMQww\n"
                  , "CgYDVQQLDANoeHIxDzANBgNVBAMMBmFydGh1cjCCASIwDQYJKoZIhvcNAQEBBQAD\n"
                  , "ggEPADCCAQoCggEBALlXRge+3lijUuHs90TCwHzsp81ZBMCIeqSRwjtn6+EcS+si\n"
                  , "qW+gZcoWGyiPVSu4bqe2769BB8Rum9vXxUmYbaFJC+itgFQoioFL4vHCNvw3ebzC\n"
                  , "G+or2F+zOPI6NJErkq5kiYlARAzYaF8svkI6tFZtXdV1s7zB+NBvS6B7BdVRFbpx\n"
                  , "l9x4hYlIDfiDqVvtNAGTRwwdG59Yf5Ena79U6l+XM/CLZ43mdhG+/OxWjIG/8okZ\n"
                  , "VcZ/ui2T/o5lCw8rEe6mlrvgrAi21xO0kJmusrTF3s9zd3wGl3xaIiOgaStA8z35\n"
                  , "O5GSWPaa+mHuE3I0XGOHLNOzLdOemnB+edcH8X0CAwEAATANBgkqhkiG9w0BAQsF\n"
                  , "AAOCAQEArk2P+77dX0/KpR51oKQKyPoFG1WncNBBsbNVPQk+T5fmGuOQisC8b1uO\n"
                  , "S+cQ6v7jPesMMCVhJWByTUYo3pX4WPhNeUrhrOr7Mm7YUqNwOwBLM4O5ep2OY3eq\n"
                  , "YR8QdQfdto+Q9BZVtNDkndNWIp7l3nohCag7GBd/s13htxKDe/Zvx7RIl62jjuLl\n"
                  , "5E6R0XxUnmerbcBqC131ZEWlkp3nUew8+dprI3Ks+7mLZoqsNVk8SpcBJE00vamn\n"
                  , "LG6603x5lTSmVLXb2+Epo5/oOeQG6xM4FpiltwPAdNiV/YfMxV+N104/cxIgAV6u\n"
                  , "qYdhDExx1kRna4NxPU3XLY1cMxmbtg==\n"
                  , "-----END CERTIFICATE-----"]

keyPem :: BS.ByteString
keyPem = BS.concat
         ["-----BEGIN RSA PRIVATE KEY-----\n"
         ,"MIIEpAIBAAKCAQEAuVdGB77eWKNS4ez3RMLAfOynzVkEwIh6pJHCO2fr4RxL6yKp\n"
         ,"b6BlyhYbKI9VK7hup7bvr0EHxG6b29fFSZhtoUkL6K2AVCiKgUvi8cI2/Dd5vMIb\n"
         ,"6ivYX7M48jo0kSuSrmSJiUBEDNhoXyy+Qjq0Vm1d1XWzvMH40G9LoHsF1VEVunGX\n"
         ,"3HiFiUgN+IOpW+00AZNHDB0bn1h/kSdrv1TqX5cz8ItnjeZ2Eb787FaMgb/yiRlV\n"
         ,"xn+6LZP+jmULDysR7qaWu+CsCLbXE7SQma6ytMXez3N3fAaXfFoiI6BpK0DzPfk7\n"
         ,"kZJY9pr6Ye4TcjRcY4cs07Mt056acH551wfxfQIDAQABAoIBAQCWkrOfOU60XkPI\n"
         ,"rlaYs0vkcfNFnguBS5+vsYdF8gCtSurWpbtXp8zqaBk6U3ATa+viL3n8PG3OBTp0\n"
         ,"WZC7YK24OXsGVmaKeMFFl9xRIsK/F2a8lkpNbb+EyDr8d5MbWXX6Xk1qhx6B/RKW\n"
         ,"N+h+L+kwyftQnyYdtMyqsbCUOT4c9TgjytfakBt5xlp/ad1zoPt4V8x7ExSPz5qV\n"
         ,"PVjOx5adWqaKtT7x7spBTY/Q2oJwsfipfer2NkDNhV8vs1eRUjSN2PBcBy5LqeFh\n"
         ,"35U+o/NdKqHZWXJCUS5ORJSzfK9B6Cbl8XP6YPT9HFmuGn89YYksgnKk23t5avQG\n"
         ,"4bwmO6zBAoGBAN02XqQwVhXjJpdlYJNIQ3DZ+ihfA0iXOYrYQZ4jpiVSJEwOMyww\n"
         ,"XcopZp2f0N34GsO8Bn5g9W3C4ATIMrPUTKSzG8Qm/oqechcCSGHL+67p0xaKH8QT\n"
         ,"ZbHDL7lEZAnChzGfc0WJ0VLCJ4yLIJu0rY6nco3sNR7DJA2RqvnkF20NAoGBANZ8\n"
         ,"x7h0DqyqNDudFVckno8SQOQoKGZKkFlsveeq7C/jx7tINz/sMVmTykHj62HaAup1\n"
         ,"8Ah2j13AUiBlziR9wYt2DNpgMDLkufBd9q5LkOCBVyvlSGnF6za+QNNLjoB3j8NP\n"
         ,"rXrE/3/erUHyujqMNKnYs1ARh4d/ze5T1V/0q9oxAoGASzvfijXKQV1k1Z6r7stn\n"
         ,"7GymEK7nvft4nYkZIAB91DmQkLFPpBq6+zODa95bufJUn/dFuhudvPvHFrlDzTdJ\n"
         ,"7cXgjqC4d0qWAM9M8L3gVke9++90CEqUjpWUmIzy/QLE2hKhsKfDokLYEouKxon3\n"
         ,"cQnwAuav1kuF6nt9S8nQUc0CgYEAyLWFcz7vSHYRYMOS57P1yC4tmRNXErmtsHDI\n"
         ,"mJRiZkIBcmiWgvO0Dn3044mzhyKqXdq1HkZLzFAeygBaUd14EtgKGQOvtsBAmwRi\n"
         ,"xSEjVmLSPtpU094ISHbqVI88r0yys/LIhLz51h97A95ulEEFOvb0fPWaXJqlCuNa\n"
         ,"JWvgsNECgYB6dAz+axFM8fyU5RM4hI9atmTpf5+WfzF+3OvY52r9xw2UqGb/AMor\n"
         ,"yUChI0ywz8ml3Jku4Cdxsf0Cgm0hFOoDGBPZhU9iPjV5VuHxUfVjn6xZZfb+9/gp\n"
         ,"5TzSwQ4wWFTULMrkeSakQc21R3p9fgSE0/xSfrfxibn+GCtyhbkYFQ==\n"
         ,"-----END RSA PRIVATE KEY-----" ]
