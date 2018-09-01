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
import Data.List          (intersect)
import Data.Maybe         (isJust)

import qualified Data.ByteString.Char8      as BC8 (pack, unpack)
import qualified Data.X509                  as X
import qualified Data.X509.CertificateStore as X
import qualified Data.X509.Validation       as X
import qualified Network.Socket             as Net
import qualified Network.TLS                as TLS
import qualified Network.TLS.Extra          as TLSExtra


mkClientParams
  :: Net.HostName
     -> String
     -> (X.CertificateChain, TLS.PrivKey)
     -> X.CertificateStore
     -> TLS.ClientParams
mkClientParams host port creds cStore =
    TLS.ClientParams{
             clientServerIdentification= (host, BC8.pack port)
           , clientUseMaxFragmentLength = Nothing
           , clientUseServerNameIndication = False
           , clientWantSessionResume       = Nothing
           , clientShared = def {
                              TLS.sharedCAStore = cStore
--                            , TLS.sharedValidationCache =  valCache host port  -- ignoreCertsCache
                            }
           , clientDebug = def
           , clientSupported = def {
                                 TLS.supportedVersions = tlsVersions
                               , TLS.supportedCiphers  = ciphers
                               }
           , clientHooks = clientHooks creds
             }

clientHooks :: (X.CertificateChain, TLS.PrivKey) -> TLS.ClientHooks
clientHooks  creds = def
        {
          TLS.onServerCertificate  = X.validate X.HashSHA256 valHooks valChecks
        , TLS.onCertificateRequest = \_ -> return $ Just creds

        }


-- the only way to ignore SelfSign validation is filter the error list
valHooks :: TLS.ValidationHooks
valHooks = def {
             X.hookFilterReason = filter $ \res ->
                                      case res of
                                        X.NameMismatch _ -> False
                                        X.SelfSigned     -> False
                                        _                -> True
          }

valChecks :: TLS.ValidationChecks
valChecks = def { X.checkFQHN = False
                , X.checkLeafV3 = False
                }

-------------------------------------------------------------------------------
valCache :: Show a => TLS.HostName -> a -> TLS.ValidationCache
valCache host port =
    let fpSHA256 = "C6:64:1F:3A:6F:60:E4:1F:FC:6A:EC:D1:01:25:1F:92:B2:1B:EF:1B:D6:E0:41:0C:F8:66:C3:6C:8A:6D:4F:E4"
        fpSHA1   = "F5:4A:0D:22:30:06:9D:F7:63:AD:7E:78:37:27:42:4A:73:50:87:76"
        fp = "\198d\US:o`\228\US\252j\236\209\SOH%\US\146\178\ESC\239\ESC\214\224A\f\248f\195l\138mO\228"
    in TLS.exceptionValidationCache [ ((host, BC8.pack $ show port), X.Fingerprint $ BC8.pack fp) ]

-------------------------------------------------------------------------------

mkServerParams
  :: TLS.Credential -> Maybe X.CertificateStore -> TLS.ServerParams
mkServerParams cred store = def {
                 TLS.serverWantClientCert =  isJust store
                 -- TLS.serverWantClientCert = False
                 , TLS.serverSupported = def {
                                           TLS.supportedVersions = tlsVersions
                                         , TLS.supportedCiphers  = ciphers
                                         }
                 , TLS.serverShared = def {
                                             TLS.sharedCredentials = TLS.Credentials [cred]
                                           }

                 , TLS.serverHooks = def
                                      {
                                        TLS.onClientCertificate = clientCertsCheck
                                      , TLS.onCipherChoosing = chooseCipher }
                            }
 where
    clientCertsCheck :: X.CertificateChain -> IO TLS.CertificateUsage
    clientCertsCheck certs = case store of
      Nothing -> return TLS.CertificateUsageAccept
      Just cs -> do
        let checks = X.defaultChecks { X.checkFQHN = False, X.checkLeafV3 = False }
        es <- X.validate X.HashSHA256 valHooks checks cs def ("","") certs
        case es of
          [] -> pure TLS.CertificateUsageAccept
          errs' -> pure (TLS.CertificateUsageReject (TLS.CertificateRejectOther
                            ("Unacceptable client cert: " ++ show errs')))
    -- Ciphers prefered by the server take precedence.
    chooseCipher :: TLS.Version -> [TLS.Cipher] -> TLS.Cipher
    chooseCipher _ cCiphs = head (intersect TLSExtra.ciphersuite_strong cCiphs)

-------------------------------------------------------------------------------
--
-- |Insecure mode.
ignoreCerts :: TLS.ValidationCache
ignoreCerts = TLS.ValidationCache
              (\_ _ _ -> pure TLS.ValidationCachePass)
              (\_ _ _ -> pure ())


-- print debug info
ignoreCerts' :: TLS.ValidationCache
ignoreCerts' = TLS.ValidationCache
                      (\serviceID (X.Fingerprint  fingerprin) _ -> do
                         print serviceID >> (print $ BC8.unpack fingerprin)
                         pure TLS.ValidationCachePass)
                      (\_ _ _ -> pure ())



clientShared :: TLS.Credential -> TLS.Shared
clientShared cs = def
        { TLS.sharedCredentials     = TLS.Credentials [cs]
        , TLS.sharedValidationCache = def
        }

clientHooks' :: TLS.Credential -> TLS.ClientHooks
clientHooks' cs = def
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
      , TLS.clientShared  = def {  TLS.sharedValidationCache = ignoreCerts}
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
             , TLS.clientHooks = clientHooks' cred
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
