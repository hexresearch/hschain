{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Abstract API for network which support
module HSChain.Network.Parameters
    ( mkClientParams
    , mkServerParams
    , getCredential
    , getCredentialFromBuffer
    ) where


import Data.ByteString.Internal (ByteString(..))
import Data.Default.Class       (def)
import Data.List                (intersect)
import Data.Maybe               (isJust)

import qualified Data.ByteString.Char8      as BC8 (pack)
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

getCredential :: FilePath -> FilePath -> IO TLS.Credential
getCredential certFile keyFile = do
         cred <- TLS.credentialLoadX509 certFile keyFile
         return $ case cred of
                    Right c  -> c
                    Left err -> error err


getCredentialFromBuffer :: ByteString -> ByteString -> TLS.Credential
getCredentialFromBuffer certPem keyPem  = let cs = TLS.credentialLoadX509FromMemory certPem keyPem
                                          in case cs of
                                               Right cred -> cred
                                               Left err   -> error err
