{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent.Async
import Control.Concurrent.Barrier
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad
import System.IO.Unsafe
import Prelude hiding (round)
import System.Random.Shuffle
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector as V


import Crypto.Bls.PrivateKey as PrivateKey
import Crypto.Bls.PublicKey as PublicKey
import Crypto.Bls.Signature as Signature
import Crypto.Bls.Threshold as Threshold
import Crypto.Bls.Util


-- | Number of players in the set
numberOfPlayers :: Int
numberOfPlayers = 200


-- | Number of signatures to sign
numberOfSignatures :: Int
numberOfSignatures = 100


data ChannelMessage = PublicFragment    PublicKey.PublicKey
                    | PrivateFragment   PrivateKey.PrivateKey
                    | SignatureFragment Signature.InsecureSignature Int


-- | Player processor
player :: Barrier                           -- ^ Barrier for synchronous round start
       -> IO ChannelMessage                 -- ^ Action for receiving messages
       -> [(ChannelMessage -> IO (), Int)]  -- ^ Actions for sending messages and relevant player id
       -> Int                               -- ^ Player id
       -> IO ()
player newRoundBarrier recv sends playerId = do
    pp "I'm started"
    pp "Preparing secret fragments..."
    (_secretKey, publicFrags, secretFrags) <- Threshold.create numberOfSignatures numberOfPlayers
    --
    --
    pp "Exchange public fragments for creating master key"
    let myMasterPublicKeyFrag = V.head publicFrags
    mapM_ (($ PublicFragment myMasterPublicKeyFrag) . fst) sends
    otherMasterPublicKeyFrags <- fmap (map (\(PublicFragment pubk) -> pubk)) $ replicateM (numberOfPlayers - 1) recv
    let masterPublicKeyFrags = myMasterPublicKeyFrag : otherMasterPublicKeyFrags
    masterPublicKey <- aggregateInsecurePublicKey (V.fromList masterPublicKeyFrags)
    serializePublicKey masterPublicKey >>= \ss -> pp $ "Master key: <" ++ hexifyBs' ss ++ ">"
    --
    --
    pp "Exchange secret fragments for creating secret shares"
    let mySecretShareFrag = secretFrags V.! playerId
    mapM_ (\(sndr, i) -> sndr (PrivateFragment $ secretFrags V.! i)) sends
    otherSecretSharesFrags <- fmap (map (\(PrivateFragment sk) -> sk)) $ replicateM (numberOfPlayers - 1) recv
    let secretSharedFrags = mySecretShareFrag : otherSecretSharesFrags
    secretShare <- aggregateInsecurePrivateKey $ V.fromList secretSharedFrags
    serializePrivateKey secretShare >>= \ss -> pp $ "Secret share: <" ++ hexifyBs' ss ++ ">"
    --
    --
    pp $ "OK, ready for signing --------"
    --
    --
    forM_ [0..1000] $ \round -> do
        newRoundBarrier
        let p = putStrLnExcl playerId (Just $ "Round: " ++ show round)
            leaderId = round `mod` numberOfPlayers
        -- Data to sign
        --
        let dataToSign = "Data To Sign for round " <> (BSC.pack $ show round) <> (BSC.pack $ concat $ replicate 10 "some_data")
        dataToSignHash <- hash256 dataToSign
        -- All players sign data
        --
        mySig <- signInsecure secretShare dataToSign
        -- Then exchange with leader
        --
        if playerId == leaderId then do
            -- Leader gets signature fragments, unite it in one signature and also verify result
            --
            p $ "I'm leader! Waiting for signatures fragments..."
            otherSigs <- fmap (map (\(SignatureFragment sig pid) -> (sig, pid))) $ replicateM (numberOfPlayers - 1) recv
            -- Get only part of signatures (we emulate situation of lost/slow connection)
            otherSigs' <- take (numberOfSignatures - 1) <$> shuffleM otherSigs
            p $ "Ok, signature fragments given from players: " ++ (show $ map snd otherSigs')
            -- Create unit signature
            let allSigs = mySig : map fst otherSigs'
                allNums    = map succ $ playerId : map snd otherSigs'
            p $ "Unite all signature fragments and verify signature"
            unitSignature <- aggregateUnitSigs (V.fromList allSigs) dataToSign allNums
            result <- verifyInsecure unitSignature (V.fromList [dataToSignHash]) (V.fromList [masterPublicKey])
            p $ "Result of verifying: " ++ show result
            unless result $ error "FAIL TO VERIFY"
        else do
            -- Other players just sign data and send to leader
            --
            p "Sign data and send signature fragment to leader"
            forM_ sends $ \(send, i) ->
                when (i == leaderId) $
                    send (SignatureFragment mySig playerId)
  where
    pp = putStrLnExcl playerId Nothing


main :: IO ()
main = do
    newRoundBarrier <- barrier numberOfPlayers
    channels <- replicateM numberOfPlayers newChan
    void $ mapConcurrently (\playerId -> player newRoundBarrier
                                                (readChan $ channels !! playerId)
                                                (filter (\(_,i) -> i /= playerId) (zip (map writeChan channels) [0..]))
                                                playerId
                               ) [0..(numberOfPlayers - 1)]


-- * ##########################################################################

consoleLock :: MVar ()
consoleLock = unsafePerformIO $ newMVar ()
{-# NOINLINE consoleLock #-}


-- | Printing message ...
putStrLnExcl :: Int -> Maybe String -> String -> IO ()
putStrLnExcl playerId prefix msg =
    withMVar consoleLock $ const $ putStrLn $ "[Player: " ++ show playerId ++ (maybe "" (", " ++ ) prefix) ++ "] " ++ msg


hexifyBs :: BS.ByteString -> BS.ByteString
hexifyBs = BSL.toStrict . BS.toLazyByteString . BS.byteStringHex


hexifyBs' :: BS.ByteString -> String
hexifyBs' bs = BSC.unpack $ hexifyBs bs
