module Params (getParams) where

import Control.Applicative
import Data.Either
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

import Data.PEM (PEM, pemParseBS, pemName, pemContent)
import Data.Certificate.X509 (X509, decodeCertificate)
import Data.Certificate.KeyRSA (decodePrivate)
import Network.TLS (
	Params(pCiphers, pCertificates, pAllowedVersions), defaultParamsServer,
	ServerParams(serverWantClientCert), updateServerParams,
	Version(SSL3, TLS10, TLS11, TLS12), PrivateKey(PrivRSA), Cipher)
import Network.TLS.Extra (
	cipher_AES128_SHA1, cipher_AES256_SHA1,
	cipher_RC4_128_MD5, cipher_RC4_128_SHA1)

getParams :: FilePath -> FilePath -> IO Params
getParams cf kf = do
	certs <- readCertificates cf
	pk <- readPrivateKey kf
	return $ mkParams certs pk

mkParams :: [X509] -> PrivateKey -> Params
mkParams certs pk =
	updateServerParams (\sp -> sp { serverWantClientCert = False }) $
		defaultParamsServer {
			pAllowedVersions = [SSL3, TLS10, TLS11, TLS12],
			pCiphers = ciphers,
			pCertificates = zip certs $ (Just pk) : repeat Nothing
		 }

readCertificates :: FilePath -> IO [X509]
readCertificates filepath = do
	rights . parseCerts . pemParseBS <$> B.readFile filepath

parseCerts :: Either String [PEM] -> [Either String X509]
parseCerts (Right pems) =
	map (decodeCertificate . L.fromChunks . (: []) . pemContent) $ filter
		(flip elem ["CERTIFICATE", "TRUSTED CERTIFICATE"] . pemName) pems
parseCerts (Left err) = error $ "cannot parse PEM file: " ++ err

readPrivateKey :: FilePath -> IO PrivateKey
readPrivateKey filepath = head <$>
	rights . parseKey . pemParseBS <$> B.readFile filepath

parseKey :: Either String [PEM] -> [Either String PrivateKey]
parseKey (Right pems) =
	map (fmap (PrivRSA . snd) . decodePrivate . L.fromChunks . (: []) . pemContent) $
		filter ((== "RSA PRIVATE KEY") . pemName) pems
parseKey (Left err) = error $ "Cannot parse PEM file: " ++ err

ciphers :: [Cipher]
ciphers = [
	cipher_AES128_SHA1,
	cipher_AES256_SHA1,
	cipher_RC4_128_MD5,
	cipher_RC4_128_SHA1
 ]
