{-# LANGUAGE PolyKinds #-}
-- |
module HSChain.PoW.P2P.Handler.Consensus
  ( ConsensusCh(..)
  , threadConsensus
    -- * Light consensus
  , LightConsensusCh(..)
  , threadLightConsensus
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Set (Set)
import Katip (sl)

import HSChain.Control.Channels
import HSChain.PoW.Consensus
import HSChain.PoW.P2P.Types
import HSChain.PoW.Types
import HSChain.Logger


----------------------------------------------------------------
-- Full consensus
----------------------------------------------------------------

-- | Channels for sending data to and from consensus thread
data ConsensusCh view = ConsensusCh
  { bcastAnnounce    :: Sink (MsgAnn (BlockType view))
    -- ^ Broadcast channel for gossip announcements (we got new best head)
  , bcastChainUpdate :: Sink (BHOf view, view)
    -- ^ Broadcast channel for announcing new head for mining etc.
  , sinkConsensusSt  :: Sink (Consensus view)
    -- ^ Updating state of consensus
  , sinkReqBlocks    :: Sink (Set (BlockIdOf view))
    -- ^ Channel for signalling to gossip what block should be fetched
  , srcRX            :: Src  (BoxRX (MonadOf view) (BlockType view))
    -- ^ Messages from gossip
  }

-- | Thread that reacts to messages from peers and updates consensus
--   accordingly
threadConsensus
  :: (MonadIO m, MonadLogger m, MonadCatch m, StateView' view m b)
  => BlockDB m b
  -> Consensus   view
  -> ConsensusCh view
  -> m x
threadConsensus db consensus0 ConsensusCh{..} = descendNamespace "cns" $ logOnException $ do
  logger InfoS "Staring consensus" ()
  flip evalStateT consensus0
    $ forever
    $ do bh <- use $ bestHead . _1 . to stateBH
         consensusMonitor db =<< awaitIO srcRX
         sinkIO sinkConsensusSt =<< get
         sinkIO sinkReqBlocks   =<< use requiredBlocks
         bh' <- use $ bestHead . _1 . to stateBH
         when (bhBID bh /= bhBID bh') $ do
           logger InfoS "New head" ( sl "h"   (bhHeight bh')
                                  <> sl "bid" (bhBID bh')
                                   )
           st <- lift . flushState =<< use (bestHead . _1)
           bestHead . _1 .= st
           sinkIO bcastAnnounce $ AnnBestHead $ asHeader bh'
           sinkIO bcastChainUpdate (bh, st)


-- Handler for messages coming from peer.
consensusMonitor
  :: (MonadLogger m, MonadIO m, StateView' view m b)
  => BlockDB m b
  -> BoxRX m b
  -> StateT (Consensus view) m ()
consensusMonitor db (BoxRX message)
  = message $ logR <=< \case
      RxAnn     m  -> handleAnnounce m
      RxBlock   b  -> do
        lift $ logger DebugS "Got RxBlock" (sl "bid" (blockID b))
        evalError $ handleBlockError $ processBlock db b
      RxMined   b  -> do
        let bid = blockID b
        lift $ logger DebugS "Got RxMined" (sl "bid" bid)
        evalError $ do handleHeaderError   $ processHeader (toHeader b)
                       handleMineError bid $ processBlock  db b
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
          ErrH'KnownHeader         -> return Peer'Noop
          ErrH'HeightMismatch      -> punish
          ErrH'ValidationFailure{} -> punish
          ErrH'BadParent           -> punish
          -- We got announce that we couldn't attach to block tree. So
          -- we need that peer to catch up
          ErrH'UnknownParent     -> return Peer'EnterCatchup
          where
            punish = do logger WarningS "Bad AnnBestHead" (sl "err" e)
                        return $ Peer'Punish $ toException e
    -- Handle error conditions which happen when we process new block
    -- Note that we explicitly requrest blocks by their BIDs so we
    -- shouln't punish peers
    handleBlockError = mapExceptT $ fmap $ \case
      Right _                  -> Right ()
      Left ErrB'UnknownBlock   -> error "Impossible: we should'n get unknown block"
      Left ErrB'InvalidBlock{} -> Right ()
    -- For mined block we _do_ want to communicate back that mined
    -- block is invalid. That's very useful when debugging
    handleMineError bid = mapExceptT $ \action -> action >>= \case
      Right errs
        | [e] <- [ e | (i,e) <- errs
                     , bid == i
                     ]           -> do logger ErrorS "Bad mined block (state)" (sl "err" e)
                                       return $ Left $ Peer'Punish $ toException e
        | otherwise              -> return $ Right ()
      Left ErrB'UnknownBlock     -> error "Impossible: we should'n get unknown block"
      Left (ErrB'InvalidBlock e) -> do logger ErrorS "Bad mined block (context-free)" (sl "err" e)
                                       return $ Left $ Peer'Punish $ toException e
    -- Handle errors during header processing. Not that KnownHeader is
    -- not really an error
    handleHeaderError = mapExceptT $ \action -> action >>= \case
      Right () -> return $ Right ()
      Left  e  -> case e of
        ErrH'KnownHeader         -> return $ Right ()
        ErrH'HeightMismatch      -> punish
        ErrH'UnknownParent       -> punish
        ErrH'ValidationFailure{} -> punish
        ErrH'BadParent           -> punish
        where
          punish = do logger WarningS "Bad header" (sl "err" e)
                      return $ Left $ Peer'Punish $ toException e
    -- Convert error from ExceptT
    evalError action = runExceptT action >>= \case
      Right () -> return Peer'Noop
      Left  e  -> return e


----------------------------------------------------------------
-- Light consensus
----------------------------------------------------------------

-- | Channels for sending data to and from consensus thread
data LightConsensusCh m b = LightConsensusCh
  { lcAnnounce :: Sink (MsgAnn b)
    -- ^ Broadcast channel for gossip announcements (we got new best head)
  , lcUpdate   :: Sink (LightConsensus b)
    -- ^ Broadcast channel for announcing new head for mining etc.
  , lcRX       :: Src  (BoxRX m b)
    -- ^ Messages from gossip
  }


-- | Thread that reacts to messages from peers and updates consensus
--   accordingly
threadLightConsensus
  :: (MonadIO m, MonadLogger m, MonadCatch m, BlockData b)
  => BlockDB m b
  -> LightConsensus b
  -> LightConsensusCh m b
  -> m x
threadLightConsensus db consensus0 LightConsensusCh{..} = descendNamespace "cns" $ logOnException $ do
  logger InfoS "Staring consensus" ()
  flip evalStateT consensus0
    $ forever
    $ do bh <- use $ bestLightHead . _1
         lightConsensusMonitor db =<< awaitIO lcRX
         bh' <- use $ bestLightHead . _1
         when (bhBID bh /= bhBID bh') $ do
           logger InfoS "New head" ( sl "h"   (bhHeight bh')
                                  <> sl "bid" (bhBID    bh')
                                   )
           sinkIO lcAnnounce $ AnnBestHead $ asHeader bh'
           sinkIO lcUpdate =<< get


-- Handler for messages coming from peer.
lightConsensusMonitor
  :: (MonadLogger m, MonadIO m, BlockData b)
  => BlockDB m b
  -> BoxRX m b
  -> StateT (LightConsensus b) m ()
lightConsensusMonitor db (BoxRX message)
  = message $ logR <=< \case
      RxAnn     m  -> handleAnnounce m
      RxHeaders hs -> do
        lift $ logger DebugS "Got RxHeaders" (sl "bid" (blockID <$> hs))
        evalError $ mapM_ (handleHeaderError . processLightHeader db) hs
      -- Block messages are simply ignored
      RxBlock _ -> pure Peer'Noop
      RxMined _ -> pure Peer'Noop
  where
    -- Log out responce to peer
    logR m = do lift $ logger DebugS "Resp" (sl "v" (show m))
                return m
    -- Handler for announces coming from peers (they come unrequested)
    handleAnnounce (AnnBestHead h) = do
      lift $ logger DebugS "Got AnnBestHead" (  sl "bid" (blockID h)
                                             <> sl "H"   (blockHeight h)
                                             )
      runExceptT (processLightHeader db h) >>= \case
        Right () -> return Peer'Noop
        Left  e  -> case e of
          ErrH'KnownHeader         -> return Peer'Noop
          ErrH'HeightMismatch      -> punish
          ErrH'ValidationFailure{} -> punish
          ErrH'BadParent           -> punish
          -- We got announce that we couldn't attach to block tree. So
          -- we need that peer to catch up
          ErrH'UnknownParent     -> return Peer'EnterCatchup
          where
            punish = do logger WarningS "Bad AnnBestHead" (sl "err" e)
                        return $ Peer'Punish $ toException e   
    -- Handle errors during header processing. Not that KnownHeader is
    -- not really an error
    handleHeaderError = mapExceptT $ \action -> action >>= \case
      Right () -> return $ Right ()
      Left  e  -> case e of
        ErrH'KnownHeader         -> return $ Right ()
        ErrH'HeightMismatch      -> punish
        ErrH'UnknownParent       -> punish
        ErrH'ValidationFailure{} -> punish
        ErrH'BadParent           -> punish
        where
          punish = do logger WarningS "Bad header" (sl "err" e)
                      return $ Left $ Peer'Punish $ toException e
    -- Convert error from ExceptT
    evalError action = runExceptT action >>= \case
      Right () -> return Peer'Noop
      Left  e  -> return e
