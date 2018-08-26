{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Abstract API for network which support
module Thundermint.P2P.Tls
    ( mkClientParams
    , mkServerParams
    ) where

import Data.Default.Class (def)

import qualified Data.ByteString.Char8 as BC8 (pack)
import qualified Network.Socket        as Net
import qualified Network.TLS           as TLS
import qualified Network.TLS.Extra     as TLSExtra


mkClientParams :: Net.HostName ->  Net.ServiceName -> TLS.Credential -> TLS.ClientParams
mkClientParams host port credentails =
    TLS.ClientParams{
             clientServerIdentification= (host, "") -- BC8.pack port)
           , clientUseMaxFragmentLength = Nothing
           , clientUseServerNameIndication = False
           , clientWantSessionResume       = Nothing
           , clientShared = ignoreCerts -- clientShared credentails
           , clientDebug = def
           , clientHooks = clientHooks credentails
           , clientSupported = def {
                                 TLS.supportedVersions = tlsVersions
                               , TLS.supportedCiphers  = ciphers
                               }
             }




mkServerParams :: TLS.Credential -> TLS.ServerParams
mkServerParams cred = def {
                        TLS.serverWantClientCert = False
                      , TLS.serverSupported = def {
                                                TLS.supportedVersions = tlsVersions
                                              , TLS.supportedCiphers  = ciphers
                                              }
                      , TLS.serverShared = def {
                                             TLS.sharedCredentials = TLS.Credentials [cred]
                                           }
                      }




-------------------------------------------------------------------------------
-- Exercise

-- |Insecure mode.
ignoreCerts :: TLS.Shared
ignoreCerts  = def
        {  TLS.sharedValidationCache = TLS.ValidationCache (\_ _ _ -> pure TLS.ValidationCachePass) (\_ _ _ -> pure ())
        }

clientShared :: TLS.Credential -> TLS.Shared
clientShared cs = def
        { TLS.sharedCredentials     = TLS.Credentials [cs]
        , TLS.sharedValidationCache = def
        }

clientHooks :: TLS.Credential -> TLS.ClientHooks
clientHooks cs = def
        { TLS.onCertificateRequest = const . return . Just $ cs
        }


tlsVersions :: [TLS.Version]
tlsVersions = [TLS.TLS12,TLS.TLS11,TLS.TLS10]

ciphers :: [TLS.Cipher]
ciphers =
    [ TLSExtra.cipher_ECDHE_RSA_AES128GCM_SHA256
    , TLSExtra.cipher_DHE_RSA_AES128GCM_SHA256
    , TLSExtra.cipher_DHE_RSA_AES256_SHA256
    , TLSExtra.cipher_DHE_RSA_AES128_SHA256
    , TLSExtra.cipher_DHE_RSA_AES256_SHA1
    , TLSExtra.cipher_DHE_RSA_AES128_SHA1
    , TLSExtra.cipher_DHE_DSS_AES128_SHA1
    , TLSExtra.cipher_DHE_DSS_AES256_SHA1
    , TLSExtra.cipher_DHE_DSS_RC4_SHA1
    , TLSExtra.cipher_AES128_SHA1
    , TLSExtra.cipher_AES256_SHA1
    , TLSExtra.cipher_RC4_128_MD5
    , TLSExtra.cipher_RC4_128_SHA1
    ]



-------------------------------------------------------------------------------
-- TODO: after review will be desicde how to pass certificates and etc params
--
-------------------------------------------------------------------------------

{-
data TLSSettings = TLSSettings {
      tlsClientCertFile  :: FilePath -- ^ File containing the certificate.
    , tlsPrivKeyFile     :: FilePath -- ^ File containing the key
    , tlsAllowedVersions :: [TLS.Version]
    , tlsCiphers         :: [TLS.Cipher]
    , serverHost         :: String
    , serverPort         :: Int
  }

defaultTlsSettings :: TLSSettings
defaultTlsSettings = TLSSettings {
    tlsClientCertFile = "certs/certificate.pem"
  , tlsPrivKeyFile = "certs/key.pem"
  , tlsAllowedVersions = [TLS.TLS12,TLS.TLS11,TLS.TLS10]
  , tlsCiphers = ciphers
  }

mkServerSettings :: TLS.Credential -> TLS.ServerParams
mkServerSettings = mkServerSettings' defaultTlsSettings

mkServerSettings' :: TLSSettings -> TLS.Credential -> TLS.ServerParams
mkServerSettings' TLSSettings{..} cred = def {
        TLS.serverWantClientCert = False
      , TLS.serverSupported = def {
          TLS.supportedVersions = tlsAllowedVersions
        , TLS.supportedCiphers  = tlsCiphers
        }
      , TLS.serverShared = def {
          TLS.sharedCredentials = TLS.Credentials [cred]
        }
      }

mkClientSettings :: Net.HostName -> Net.ServiceName -> TLS.ClientParams
mkClientSettings = mkClientSettings' defaultTlsSettings



mkClientSettings' :: TLSSettings -> Net.HostName -> Net.ServiceName -> TLS.ClientParams
mkClientSettings' TLSSettings{..} hostname port = TLS.ClientParams{
        TLS.clientUseMaxFragmentLength= Nothing
      , TLS.clientServerIdentification= (hostname, BC8.pack port)
      , TLS.clientUseServerNameIndication = False
      , TLS.clientWantSessionResume = Nothing
      , TLS.clientShared  = ignoreCerts
      , TLS.clientHooks = def
      , clientDebug = def
      , TLS.clientSupported = def {
                                TLS.supportedVersions = tlsAllowedVersions
                              , TLS.supportedCiphers  = tlsCiphers
                              }
      }

mkClientSettings'IO :: Net.HostName -> Net.ServiceName -> IO TLS.ClientParams
mkClientSettings'IO = mkClientSettingsIO defaultTlsSettings

mkClientSettingsIO :: TLSSettings -> Net.HostName -> Net.ServiceName -> IO TLS.ClientParams
mkClientSettingsIO TLSSettings{..} hostname port = do
  cred <- getCredentials tlsClientCertFile tlsPrivKeyFile
  return TLS.ClientParams{
               TLS.clientServerIdentification= (hostname, BC8.pack port)
             , TLS.clientUseMaxFragmentLength = Nothing
             , TLS.clientUseServerNameIndication = False
             , TLS.clientWantSessionResume       = Nothing
             , TLS.clientShared  = clientShared cred
             , TLS.clientDebug = def
             , TLS.clientHooks = clientHooks cred
             , TLS.clientSupported = def {
                                       TLS.supportedVersions = tlsAllowedVersions
                                     , TLS.supportedCiphers  = tlsCiphers
                                     }
             }


-- mkClientParams :: TLSSettings -> Net.HostName -> Net.ServiceName -> IO TLS.ClientParams


getCredentials :: FilePath -> FilePath -> IO TLS.Credential
getCredentials certFile keyFile = do
         cred <- TLS.credentialLoadX509 certFile keyFile
         return $ case cred of
                    Right c  -> c
                    Left err -> error err
-}
