{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
module TM.RealNetwork
  ( realNetPair
  , NetPair
  , withTimeOut
  , withRetry
  ) where

import Control.Exception (IOException)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Retry  (constantDelay, limitRetries, recovering)
import qualified Network.Socket  as Net
import System.Random
import System.Timeout

import HSChain.Network.Types
import HSChain.Network.TCP
import HSChain.Network.UDP


----------------------------------------------------------------

type NetPair = ((NetAddr, NetworkAPI), (NetAddr, NetworkAPI))

realNetPair :: Maybe (Maybe Int)
            -> Net.HostName
            -> IO NetPair
realNetPair udpPortSpec host = do
    let useUDP = udpPortSpec /= Nothing
    n <- case udpPortSpec of
      Just (Just p) -> return p
      _ -> randomRIO (1, 9999 :: Int)
    let suffix = reverse $ take 4 $ (reverse $ show n) ++ repeat '0'
        port1 = concat ["3", suffix]
        port2 = concat ["4", suffix]
        realNet p = if not useUDP
          then return (newNetworkTcp port)
          else newNetworkUdp port
          where
            port = read p
        hints = Net.defaultHints  { Net.addrSocketType = if useUDP then Net.Datagram else Net.Stream }
    addr1:_ <- Net.getAddrInfo (Just hints) (Just host) (Just port1)
    addr2:_ <- Net.getAddrInfo (Just hints) (Just host) (Just port2)
    server <- realNet port1
    client <- realNet port2

    let sockAddr1 = Net.addrAddress addr1
    let sockAddr2 = Net.addrAddress addr2

    return ( (sockAddrToNetAddr sockAddr1, server)
           , (sockAddrToNetAddr sockAddr2, client)
           )

withRetry :: MonadIO m => IO a -> m a
withRetry
  = liftIO
  . recovering (constantDelay 500 <> limitRetries 20) hs
  . const
  where
    -- exceptions list to trigger the recovery logic
    hs :: [a -> Handler IO Bool]
    hs = [const $ Handler (\(_::IOException) -> return True)]

-- | Exception for aborting the execution of test
data AbortTest = AbortTest
                 deriving Show

instance Exception AbortTest

withTimeOut :: Int -> IO a -> IO a
withTimeOut t act = timeout t act >>= \case
  Just n  -> pure n
  Nothing -> throwM AbortTest
