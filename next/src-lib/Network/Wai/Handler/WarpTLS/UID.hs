{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.WarpTLS.UID (runTLSSocket, runTLSSocketWithID, tlsSettings, Group, User) where

import Network.Wai.Handler.WarpTLS.Params (makeParams)
import Network.Wai.Handler.WarpTLS.Getter (getter)

import Control.Applicative
import qualified Data.ByteString as B

import Network.Socket (Socket)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (Settings, runSettingsConnection)

import System.Posix

data TLSSettings = TLSSettings FilePath FilePath

tlsSettings :: FilePath -> FilePath -> TLSSettings
tlsSettings = TLSSettings

runTLSSocket ::
	FilePath -> FilePath -> Settings -> Socket -> Application -> IO ()
runTLSSocket crt key set sock app = do
	params <- makeParams <$> B.readFile crt <*> B.readFile key
	runSettingsConnection set (getter params sock) app

type Group = String
type User = String

runTLSSocketWithID :: TLSSettings -> Settings -> Socket ->
	(Group, User) -> Application -> IO ()
runTLSSocketWithID (TLSSettings crt key) set sock (gid, uid) app = do
	!c <- B.readFile crt
	!k <- B.readFile key
	getGroupEntryForName gid >>= setGroupID . groupID
	getUserEntryForName uid >>= setUserID . userID
	runSettingsConnection set (getter (makeParams c k) sock) app

runTLSSocketCnt ::
	B.ByteString -> B.ByteString -> Settings -> Socket -> Application -> IO ()
runTLSSocketCnt crt key set sock app =
	runSettingsConnection set (getter (makeParams crt key) sock) app
