module TLSSocket (runTLSSocket) where

import Params (makeParams)
import Getter (getter)

import Control.Applicative
import qualified Data.ByteString as B

import Network.Socket (Socket)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (Settings, runSettingsConnection)

runTLSSocket ::
	FilePath -> FilePath -> Settings -> Socket -> Application -> IO ()
runTLSSocket crt key set sock app = do
	params <- makeParams <$> B.readFile crt <*> B.readFile key
	runSettingsConnection set (getter params sock) app
