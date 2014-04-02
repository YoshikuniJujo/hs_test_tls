{-# LANGUAGE PackageImports, OverloadedStrings, RankNTypes, KindSignatures #-}

import Data.Function (on)

import Network.Wai.Handler.WarpTLS.UID (runTLSSocketWithID, tlsSettings)
-- import Network.Wai.Handler.WarpTLS (runTLSSocket, tlsSettings)

import Data.Char
import Control.Applicative
import Control.Exception
import System.Environment

import Data.Conduit.Network (bindPort)

import Network.Socket (sClose)
import Network.HTTP.Types.Status (status200)
import Network.Wai (Response, responseLBS, Application)
import Network.Wai.Handler.Warp (HostPreference(HostAny), defaultSettings)

cutSpaces :: String -> String
cutSpaces = takeWhile $ not . isSpace

guid :: (String, String)
guid = ("yesod", "yesod")

main :: IO ()
main = do
	args <- getArgs
	tlss <- case args of
		[crt, key] -> return $ tlsSettings crt key
		[] -> (tlsSettings `on` cutSpaces)
			<$> readFile "crt.path"
			<*> readFile "key.path"
		_ -> error "wrong argument number"
	bracket (bindPort 3000 HostAny) sClose $ \sock ->
		runTLSSocketWithID tlss defaultSettings sock guid $ \_ -> do
				print ("hello" :: String)
				return $ responseLBS status200
					[("Content-Type", "text/plain")] "PONG"
