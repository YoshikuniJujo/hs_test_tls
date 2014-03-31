{-# LANGUAGE PackageImports, OverloadedStrings, RankNTypes, KindSignatures #-}

import Params (getParams)
import Getter (getter)

import "monads-tf" Control.Monad.Trans
import Control.Exception

import Data.Conduit.Network (bindPort)

import Network.Socket (sClose)
import Network.HTTP.Types.Status (status200)
import Network.Wai (Response, responseLBS)
import Network.Wai.Handler.Warp (
	HostPreference(HostAny), runSettingsConnection, defaultSettings)

crt, key :: String
crt = "/home/tatsuya/ssl/csr/2013crt_cross_owl_skami2.pem"
key = "/home/tatsuya/ssl/csr/mydomain.key"

main :: IO ()
main = bracket (bindPort 3000 HostAny) sClose $ \sock -> do
	params <- getParams crt key
	runSettingsConnection defaultSettings (getter params sock) app
		
app :: MonadTrans t => a -> t IO Response
app _ = lift $ do
	print ("hello" :: String)
	return $ responseLBS status200 [("Content-Type", "text/plain")] "PONG"
