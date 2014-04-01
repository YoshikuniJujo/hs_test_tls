module Params (makeParams) where

import TLS (
	Params(pCiphers, pCertificates, pAllowedVersions), defaultParamsServer,
	ServerParams(serverWantClientCert), updateServerParams,
	Version(SSL3, TLS10, TLS11, TLS12), PrivateKey(PrivRSA))
import TLS (
	cipher_AES128_SHA1, cipher_AES256_SHA1,
	cipher_RC4_128_MD5, cipher_RC4_128_SHA1)

import Data.Either
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

-- import Control.Applicative
import Control.Arrow ((>>>), (|||))

import Data.PEM (PEM, pemParseBS, pemName, pemContent)
import Data.Certificate.X509 (X509, decodeCertificate)
import Data.Certificate.KeyRSA (decodePrivate)

makeParams :: B.ByteString -> B.ByteString -> Params
makeParams crts pk = initParam { pCertificates =
	zip (parseCrts crts) $ Just (parsePK pk) : repeat Nothing }

initParam :: Params
initParam = updateServerParams (\sp -> sp { serverWantClientCert = False }) $
	defaultParamsServer {
		pAllowedVersions = [SSL3, TLS10, TLS11, TLS12],
		pCiphers = [
			cipher_AES128_SHA1, cipher_AES256_SHA1,
			cipher_RC4_128_MD5, cipher_RC4_128_SHA1 ] }

parseCrts :: B.ByteString -> [X509]
parseCrts = pemParseBS >>>
	error . ("Cannot parse PEM file: " ++) |||
	rights . map (decodeCertificate . pemToLazy) .
		filterPem ["CERTIFICATE", "TRUSTED CERTIFICATE"]

parsePK :: B.ByteString -> PrivateKey
parsePK = pemParseBS >>>
	error . ("Cannot parse PEM file: " ++) |||
	head . rights . map (privRSA . pemToLazy) . filterPem ["RSA PRIVATE KEY"]

privRSA :: L.ByteString -> Either String PrivateKey
privRSA = fmap (PrivRSA . snd) . decodePrivate

pemToLazy :: PEM -> L.ByteString
pemToLazy = L.fromChunks . (: []) . pemContent

filterPem :: [String] -> [PEM] -> [PEM]
filterPem ns = filter $ (`elem` ns) . pemName
