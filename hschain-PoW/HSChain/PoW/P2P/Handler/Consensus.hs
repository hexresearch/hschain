{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
-- |
module HSChain.PoW.P2P.Handler.Consensus
  ( ConsensusCh(..)
  , threadConsensus
  ) where

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Set (Set)
import Lens.Micro
import Lens.Micro.Mtl
import Katip (sl)

import HSChain.Control.Channels
import HSChain.PoW.Consensus
import HSChain.PoW.P2P.Types
import HSChain.PoW.Types
import HSChain.PoW.Logger


-- | Channels for sending data to and from consensus thread
data ConsensusCh m b = ConsensusCh
  { bcastAnnounce   :: Sink (MsgAnn b)
  , sinkConsensusSt :: Sink (Consensus m b)
  , sinkReqBlocks   :: Sink (Set (BlockID b))
  , srcRX           :: Src  (BoxRX m b)
  }

-- | Thread that reacts to messages from peers and updates consensus
--   accordingly
threadConsensus
  :: (MonadIO m, MonadLogger m, BlockData b, MonadCatch m)
  => BlockDB m b
  -> Consensus m b
  -> ConsensusCh m b
  -> m x
threadConsensus db consensus0 ConsensusCh{..} = descendNamespace "cns" $ logOnException $ do
  logger InfoS "Staring consensus" ()
  flip evalStateT consensus0
    $ forever
    $ do bh <- use $ bestHead . _1
         consensusMonitor db =<< awaitIO srcRX
         sinkIO sinkConsensusSt =<< get
         sinkIO sinkReqBlocks   =<< use requiredBlocks
         bh' <- use $ bestHead . _1
         when (bhBID bh /= bhBID bh') $ sinkIO bcastAnnounce $ AnnBestHead $ asHeader bh'


-- Handler for messages coming from peer.
consensusMonitor
  :: (MonadLogger m, BlockData b, MonadIO m)
  => BlockDB m b
  -> BoxRX m b
  -> StateT (Consensus m b) m ()
consensusMonitor db (BoxRX message)
  = message $ logR <=< \case
      RxAnn     m  -> handleAnnounce m
      RxBlock   b  -> do
        lift $ logger DebugS "Got RxBlock" (sl "bid" (blockID b))
        evalError $ handleBlockError $ processBlock db b
      RxMined   b  -> do
        lift $ logger DebugS "Got RxMined" (sl "bid" (blockID b))
        evalError $ do handleHeaderError $ processHeader (toHeader b)
                       handleBlockError  $ processBlock  db b
      RxHeaders hs -> do
        lift $ logger DebugS "Got RxHeaders" (sl "bid" (blockID <$> hs))
        evalError $ mapM_ (handleHeaderError . processHeader) hs
  where
    -- Log out responce to peer
    logR m = do lift $ logger DebugS "Resp" (sl "v" (show m))
                return m
    -- Handler for announces coming from peers (they come unrequested)
    handleAnnounce (AnnBestHead h) = do
      lift $ logger DebugS "Got AnnBestHead" (  sl "bid" (blockID h)
                                             <> sl "H"   (blockHeight h)
                                             )
      runExceptT (processHeader h) >>= \case
        Right () -> return Peer'Noop
        Left  e  -> case e of
          ErrH'KnownHeader       -> return Peer'Noop
          ErrH'HeightMismatch    -> return $ Peer'Punish $ toException e
          ErrH'ValidationFailure -> return $ Peer'Punish $ toException e
          ErrH'BadParent         -> return $ Peer'Punish $ toException e
          -- We got announce that we couldn't attach to block tree. So
          -- we need that peer to catch up
          ErrH'UnknownParent     -> return Peer'EnterCatchup
    -- Handle error conditions which happen when we process new block
    -- Note that we explicitly requrest blocks by their BIDs so we
    -- shouln't punish peers
    handleBlockError = mapExceptT $ fmap $ \case
      Right               () -> Right ()
      Left ErrB'UnknownBlock -> error "Impossible: we should'n get unknown block"
      Left ErrB'InvalidBlock -> Right ()
    -- Handle errors during header processing. Not that KnownHeader is
    -- not really an error
    handleHeaderError = mapExceptT $ fmap $ \case
      Right () -> Right ()
      Left  e  -> case e of
        ErrH'KnownHeader       -> Right ()
        ErrH'HeightMismatch    -> Left $ Peer'Punish $ toException e
        ErrH'UnknownParent     -> Left $ Peer'Punish $ toException e
        ErrH'ValidationFailure -> Left $ Peer'Punish $ toException e
        ErrH'BadParent         -> Left $ Peer'Punish $ toException e
    -- Convert error from ExceptT
    evalError action = runExceptT action >>= \case
      Right () -> return Peer'Noop
      Left  e  -> return e
