{-# LANGUAGE OverloadedStrings #-}
-- |
module TM.RealNetwork ( realNetPair
                      , realTlsNetPair ) where

import System.Random
import Thundermint.P2P.Network
import Thundermint.P2P.NetworkTls

import qualified Data.ByteString as BS
import qualified Network.Socket  as Net
import qualified Network.TLS     as TLS
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


realTlsNetPair :: Net.HostName
               -> IO ((Net.SockAddr, NetworkAPI Net.SockAddr),
                      (Net.SockAddr, NetworkAPI Net.SockAddr))
realTlsNetPair  host = do
    n <- randomRIO (0, 99 :: Int)
    credential <- getCredential "certs/certificate.pem" "certs/key.pem"
    let port1 = "301" ++ show  n
        port2 = "301" ++ show  (n+1)
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


bufferCredential :: TLS.Credential
bufferCredential = let cs = TLS.credentialLoadX509FromMemory certificatePem keyPem
                   in case cs of
                         Right cred -> cred
                         Left err   -> error err

getCredential :: FilePath -> FilePath -> IO TLS.Credential
getCredential certFile keyFile = do
         cred <- TLS.credentialLoadX509 certFile keyFile
         return $ case cred of
                    Right c  -> c
                    Left err -> error err

-------------------------------------------------------------------------------
-- pem buffers

certificatePem = BS.concat
                 [ "-----BEGIN CERTIFICATE-----\n"
                 ,"MIIDEjCCAfoCCQDG1T3JOqXNTjANBgkqhkiG9w0BAQsFADBLMQswCQYDVQQGEwJS\n"
                 ,"VTEPMA0GA1UEBwwGTW9zY293MQwwCgYDVQQKDANIWFIxDDAKBgNVBAsMA2h4cjEP\n"
                 ,"MA0GA1UEAwwGYXJ0aHVyMB4XDTE4MDgyMzA4NDUyM1oXDTE4MDkyMjA4NDUyM1ow\n"
                 ,"SzELMAkGA1UEBhMCUlUxDzANBgNVBAcMBk1vc2NvdzEMMAoGA1UECgwDSFhSMQww\n"
                 ,"CgYDVQQLDANoeHIxDzANBgNVBAMMBmFydGh1cjCCASIwDQYJKoZIhvcNAQEBBQAD\n"
                 ,"ggEPADCCAQoCggEBALlXRge+3lijUuHs90TCwHzsp81ZBMCIeqSRwjtn6+EcS+si\n"
                 ,"qW+gZcoWGyiPVSu4bqe2769BB8Rum9vXxUmYbaFJC+itgFQoioFL4vHCNvw3ebzC\n"
                 ,"G+or2F+zOPI6NJErkq5kiYlARAzYaF8svkI6tFZtXdV1s7zB+NBvS6B7BdVRFbpx\n"
                 ,"l9x4hYlIDfiDqVvtNAGTRwwdG59Yf5Ena79U6l+XM/CLZ43mdhG+/OxWjIG/8okZ\n"
                 ,"VcZ/ui2T/o5lCw8rEe6mlrvgrAi21xO0kJmusrTF3s9zd3wGl3xaIiOgaStA8z35\n"
                 ,"O5GSWPaa+mHuE3I0XGOHLNOzLdOemnB+edcH8X0CAwEAATANBgkqhkiG9w0BAQsF\n"
                 ,"AAOCAQEAjM2k9ciihSQDmIMzXXA58l3RbdG6T7bHsEAQdapZMw3n8wKE3zkxB11x\n"
                 ,"zFXqdphXSEOYdqCaes/ML0/u7VUksZa9gea1SSrufgkdam06a2A5bJG4ZxUtWZ1+\n"
                 ,"NVxKVQc1xRdUoqwNM+hE6L7LgbIexyRZtQLA+zOfvml1ttLuVxIBMw177rJRE7Ku\n"
                 ,"pxbZWYGVmiVoETdAsaXOaQfj781q0Baui2OF6wnhZLlceOPlnyJqC3d4kXUYpqQS\n"
                 ,"MfyYe6u7+EHWfcsK5OpWyVuCGMueRQ16PwKD0Pwmkp/sOhfkUD9s1W0/yo+T5BeW\n"
                 ,"kkIY3ztnVpfq7BgN+AA10sV1Q52sXg==\n"
                 ,"-----END CERTIFICATE-----"]


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
         ,"-----END RSA PRIVATE KEY-----"
         ]
