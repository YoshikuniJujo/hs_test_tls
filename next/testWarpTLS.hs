{-# LANGUAGE PackageImports, OverloadedStrings, RankNTypes, KindSignatures #-}

import TLSSocket (runTLSSocket)

import Data.Char
import Control.Applicative
import "monads-tf" Control.Monad.Trans
import Control.Exception
import System.Environment

import Data.Conduit.Network (bindPort)

import Network.Socket (sClose)
import Network.HTTP.Types.Status (status200)
import Network.Wai (Response, responseLBS)
import Network.Wai.Handler.Warp (HostPreference(HostAny), defaultSettings)

main :: IO ()
main = do
	args <- getArgs
	[crt, key] <- case args of
		[_, _] -> return args
		[] -> (\c k -> map (takeWhile $ not . isSpace) [c, k])
			<$> readFile "crt.path"
			<*> readFile "key.path"
		_ -> error "wrong argument number"
	bracket (bindPort 3000 HostAny) sClose $ \sock ->
		runTLSSocket crt key defaultSettings sock pong
		
pong :: a -> IO Response
pong _ = do
	print ("hello" :: String)
	return $ responseLBS status200 [("Content-Type", "text/plain")] "PONG"
