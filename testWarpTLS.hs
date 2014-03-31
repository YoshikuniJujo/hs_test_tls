{-# LANGUAGE PackageImports, OverloadedStrings #-}

import Data.Conduit.Network
import Network.Wai
import Network.Wai.Handler.WarpTLS
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.RequestLogger (mkRequestLogger)
import Data.Default (def)
import Control.Exception
import Network
import "monads-tf" Control.Monad.Trans

import Control.Concurrent
import Network.HTTP.Types.Status

import Blaze.ByteString.Builder (copyByteString)

tlss = tlsSettings
	"/home/tatsuya/ssl/csr/2013crt_cross_owl_skami2.pem"
	"/home/tatsuya/ssl/csr/mydomain.key"
--	"/home/tatsuya/ssl/csr/2013key_owl_skami2.pem"

main :: IO ()
main = do
	bracket (bindPort 3000 HostAny) sClose $ \socket -> do
--		app <- mkRequestLogger def
		runTLSSocket tlss defaultSettings socket app
		
app _ = lift $ do
	print "hello"
	return $ responseLBS status200 [("Content-Type", "text/plain")] "PONG"
