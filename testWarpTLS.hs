{-# LANGUAGE PackageImports, OverloadedStrings, RankNTypes, KindSignatures #-}

import Params

import Data.Maybe
import "crypto-random" Crypto.Random

import Data.Conduit
import Data.Conduit.List as CL
import Data.Conduit.Network
import Data.Conduit.Binary

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Network.TLS
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Exception
import Control.Monad
import Control.Applicative
import "monads-tf" Control.Monad.Trans

import Network.HTTP.Types.Status
import Network.Socket

import Data.IORef

crt, key :: String
crt = "/home/tatsuya/ssl/csr/2013crt_cross_owl_skami2.pem"
key = "/home/tatsuya/ssl/csr/mydomain.key"

main :: IO ()
main = do
	bracket (bindPort 3000 HostAny) sClose $ \sock -> do
		params <- getParams crt key
		runSettingsConnection defaultSettings (getter params sock) app
--		runTLSSocket tlss defaultSettings sock app
		
app :: MonadTrans t => a -> t IO Response
app _ = lift $ do
	print ("hello" :: String)
	return $ responseLBS status200 [("Content-Type", "text/plain")] "PONG"

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
					runResourceT $ sourceFileRange fp (Just offset) (Just len) $$ CL.mapM_ (sendData ctx . L.fromChunks . return),
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
