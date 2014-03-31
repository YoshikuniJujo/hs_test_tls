{-# LANGUAGE RankNTypes, KindSignatures, PackageImports #-}

module Getter (getter) where

import Prelude hiding (mapM_)

import Data.Maybe
import Data.IORef
import Control.Applicative
import Control.Monad (unless)
import Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import "crypto-random" Crypto.Random

import Data.Conduit (
	ConduitM, runResourceT,
	yield, await, leftover, ($$), ($$+), ($$++))
import Data.Conduit.List (peek, mapM_)
import Data.Conduit.Binary (sourceFileRange)
import Data.Conduit.Network (sourceSocket, sinkSocket)

import Network.Socket (Socket, SockAddr, accept, sClose)
import Network.TLS (
	Params, TLSParams,
	Backend(Backend, backendSend, backendRecv, backendFlush, backendClose),
	contextNew, handshake, sendData, recvData, bye)
import Network.Wai.Handler.Warp (
	Connection(
		Connection, connSendMany, connSendAll, connSendFile,
		connRecv, connClose),
	socketConnection)

getter :: Params -> Socket -> IO (Connection, SockAddr)
getter params sock = do
	(s, sa) <- accept sock
	handle (retry s params sock) $ do
		(fromClient, firstBS) <- sourceSocket s $$+ peek
		let toClient = sinkSocket s
		ifromClient <- newIORef fromClient
		let getNext sink = do
			fromClient' <- readIORef ifromClient
			(fromClient'', bs) <- fromClient' $$++ sink
			writeIORef ifromClient fromClient''
			return bs
		if maybe False ((== 0x16) . fst) (firstBS >>= B.uncons)
		then do	gen <- cprgCreate <$> createEntropyPool
			ctx <- contextNew Backend {
				backendFlush = return (),
				backendClose = return (),
				backendSend = \bs -> yield bs $$ toClient,
				backendRecv = getNext . takeMost
			 } params (gen :: SystemRNG)
			handshake ctx
			let conn = Connection {
				connSendMany = sendData ctx . L.fromChunks,
				connSendAll = sendData ctx . L.fromChunks . return,
				connSendFile = \fp offset len _th headers _cleaner -> do
					sendData ctx $ L.fromChunks headers
					runResourceT $ sourceFileRange fp (Just offset) (Just len) $$ mapM_ (sendData ctx . L.fromChunks . return),
				connClose = bye ctx >> sClose s,
				connRecv = recvData ctx
			 }
			return (conn, sa)
		else do	let conn = (socketConnection s) {
				connRecv = getNext $
					fmap (fromMaybe B.empty) await
			 }
			return (conn, sa)

retry :: Socket -> TLSParams -> Socket -> SomeException ->
	IO (Connection, SockAddr)
retry s a b _ = sClose s >> getter a b

takeMost :: forall (m :: * -> *) o . Monad m => Int -> ConduitM B.ByteString o m B.ByteString
takeMost i = await >>= maybe (return B.empty) go
	where
	go bs = do
		unless (B.null y) $ leftover y
		return x
		where
		(x, y) = B.splitAt i bs
