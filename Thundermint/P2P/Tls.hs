{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Abstract API for network which support
module Thundermint.P2P.Tls
    ( TLSSettings(..)
    , mkClientSettings
    , mkServerSettings
    ) where

import Data.Default.Class (def)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC8 (pack)
import qualified Network.Socket        as Net
import qualified Network.TLS           as TLS
import qualified Network.TLS.Extra     as TLSExtra

import qualified Data.ByteString.Lazy as LBS

-- import Thundermint.P2P.Types

-- | An action when a plain HTTP comes to HTTP over TLS/SSL port.
data OnInsecure = DenyInsecure LBS.ByteString
                | AllowInsecure

data TLSSettings = TLSSettings {
    certFile           :: FilePath
    -- ^ File containing the certificate.
  , keyFile            :: FilePath
  , certMemory         :: Maybe BS.ByteString
  , keyMemory          :: Maybe BS.ByteString
    -- ^ File containing the key
  , onInsecure         :: OnInsecure
    -- ^ Do we allow insecure connections with this server as well? Default
    -- is a simple text response stating that a secure connection is required.
--  , tlsLogging         :: TLS.Logging
    -- ^ The level of logging to turn on.
    --
    -- Default: 'TLS.defaultLogging'.
  , tlsAllowedVersions :: [TLS.Version]
    -- ^ The TLS versions this server accepts.
    --
    -- Default: '[TLS.TLS10,TLS.TLS11,TLS.TLS12]'.
    --
    -- Since 1.4.2
  , tlsCiphers         :: [TLS.Cipher]
    -- ^ The TLS ciphers this server accepts.
  }

tlsSettings :: FilePath -- * Certificate file
            -> FilePath -- * Key file
            -> TLSSettings
tlsSettings cert key = defaultTlsSettings {
    certFile = cert
  , keyFile = key
  }


defaultTlsSettings :: TLSSettings
defaultTlsSettings = TLSSettings {
    certFile = "certificate.pem"
  , keyFile = "key.pem"
  , certMemory = Nothing
  , keyMemory  = Nothing
  , onInsecure = DenyInsecure "This socket only accepts secure connections."
  -- , tlsLogging = def
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
      , TLS.clientShared  = def
      , TLS.clientHooks = def
      , clientDebug = def
      , TLS.clientSupported = def {
                                TLS.supportedVersions = tlsAllowedVersions
                              , TLS.supportedCiphers  = tlsCiphers
                              }
      }


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
