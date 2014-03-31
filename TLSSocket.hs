module TLSSocket (runTLSSocket) where

import Params (getParams)
import Getter (getter)

import Network.Socket (Socket)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (Settings, runSettingsConnection)

runTLSSocket ::
	FilePath -> FilePath -> Settings -> Socket -> Application -> IO ()
runTLSSocket crt key set sock app = do
	params <- getParams crt key
	runSettingsConnection set (getter params sock) app
