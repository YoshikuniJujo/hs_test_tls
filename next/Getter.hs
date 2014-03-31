{-# LANGUAGE RankNTypes, KindSignatures, PackageImports, ScopedTypeVariables #-}

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
	ConduitM, ResumableSource, Sink,
	runResourceT, yield, await, leftover, ($$), ($$+), ($$++))
import Data.Conduit.List (peek, mapM_)
import Data.Conduit.Binary (sourceFileRange)
import Data.Conduit.Network (sourceSocket, sinkSocket)

import Network.Socket (Socket, SockAddr, accept, sClose)
import Network.TLS (
	Params,
	Backend(Backend, backendSend, backendRecv, backendFlush, backendClose),
	contextNew, handshake, sendData, recvData, bye)
import Network.Wai.Handler.Warp (
	Connection(
		Connection, connSendMany, connSendAll, connSendFile,
		connSendFileOverride, connBufferSize, connBuffer,
		connRecv, connClose),
	ConnSendFileOverride(NotOverride),
	socketConnection)
import Network.Wai.Handler.Warp.Buffer (allocateBuffer, freeBuffer)

getter :: Params -> Socket -> IO (Connection, SockAddr)
getter params sock = do
	(s, sa) <- accept sock
	buf <- allocateBuffer 256
	handle (\(_ :: SomeException) -> sClose s >> getter params sock) $ do
		(fromClient, firstBS) <- sourceSocket s $$+ peek
		ifromClient <- newIORef fromClient
		if maybe False ((== 0x16) . fst) (firstBS >>= B.uncons)
		then do	gen <- cprgCreate <$> createEntropyPool
			ctx <- contextNew Backend {
				backendFlush = return (),
				backendClose = return (),
				backendSend = \bs -> yield bs $$ mkToClient s,
				backendRecv = getNext ifromClient . takeMost
			 } params (gen :: SystemRNG)
			handshake ctx
			let conn = Connection {
				connSendMany = sendData ctx . L.fromChunks,
				connSendAll = sendData ctx . L.fromChunks . return,
				connSendFile = \fp offset len _th headers -> do
					sendData ctx $ L.fromChunks headers
					runResourceT $ sourceFileRange fp (Just offset) (Just len) $$ mapM_ (sendData ctx . L.fromChunks . return),
				connSendFileOverride = NotOverride,
				connBufferSize = 256,
				connBuffer = buf,
				connClose = bye ctx >> sClose s,
				connRecv = recvData ctx
			 }
			return (conn, sa)
		else do	cs <- socketConnection s
			let conn = cs {
				connRecv = getNext ifromClient $
					fmap (fromMaybe B.empty) await
			 }
			return (conn, sa)

getNext :: IORef (ResumableSource IO a) -> Sink a IO b -> IO b
getNext ifromClient sink = do
	fromClient <- readIORef ifromClient
	(fromClient', bs) <- fromClient $$++ sink
	writeIORef ifromClient fromClient'
	return bs

takeMost :: forall (m :: * -> *) o . Monad m =>
	Int -> ConduitM B.ByteString o m B.ByteString
takeMost i = await >>= maybe (return B.empty) go
	where
	go bs = do
		unless (B.null y) $ leftover y
		return x
		where
		(x, y) = B.splitAt i bs

mkToClient :: Socket -> ConduitM B.ByteString o IO ()
mkToClient = sinkSocket
