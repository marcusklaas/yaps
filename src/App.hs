{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.Directory
import           System.IO
import           Data.ByteString
import           Data.Text.Encoding
import           Data.ByteString.Lazy
import           Data.ByteString.Lazy.Char8
import           Data.Text
import           Data.Text.Lazy
import           Data.Text.Lazy.Encoding
import           Crypto.Hash
import           Web.FormUrlEncoded

-- * data types

data UncheckedLibrary = UncheckedLibrary { inner :: Data.Text.Text
                                         , hmak :: String
                                         } deriving (Eq, Show)

data InnerLibrary = InnerLibrary { innerBlob :: String
                                 , libraryVersion :: Integer
                                 , apiVersion :: Integer
                                 , modified :: Integer
                                 } deriving (Eq, Show)

instance FromJSON UncheckedLibrary where
  parseJSON = withObject "pair" $ \o -> do
    hmak <- o .: "hmac"
    inner <- o .: "library"
    return UncheckedLibrary { .. }

instance FromJSON InnerLibrary where
  parseJSON = withObject "innerlib" $ \o -> do
    innerBlob <- o .: "blob"
    libraryVersion <- o .: "library_version"
    apiVersion <- o .: "api_version"
    modified <- o .: "modified"
    return InnerLibrary { .. }

instance ToJSON InnerLibrary where
  toJSON o = object [
      "blob" .= innerBlob o,
      "library_version" .= libraryVersion o,
      "api_version" .= apiVersion o,
      "modified" .= modified o ]

instance ToJSON UncheckedLibrary where
  toJSON o = object [
      "library" .= inner o,
      "hmac" .= hmak o ]

data UpdateRequest = UpdateRequest { newLib :: UncheckedLibrary
                                   , hash :: Data.Text.Text }

instance FromForm UpdateRequest where
  fromForm inputs = do
    hash <- lookupUnique "pwhash" inputs
    libText <- lookupUnique "newlib" inputs
    newLib <- case ((decode . Data.Text.Lazy.Encoding.encodeUtf8 . Data.Text.Lazy.fromStrict) libText)
      of Just lib -> Right lib
         Nothing -> Left "Couldn't decode library"
    return UpdateRequest{..}


instance FromHttpApiData UncheckedLibrary where
  parseQueryParam p = case (decode . Data.Text.Lazy.Encoding.encodeUtf8 . Data.Text.Lazy.fromStrict) p
    of Just lib -> Right lib
       Nothing -> Left "Couldn't decode library"

-- * api

type ItemApi =
  "passwords" :> Get '[JSON] UncheckedLibrary :<|>
  "passwords" :> ReqBody '[FormUrlEncoded] UpdateRequest :> PostNoContent '[JSON] NoContent

itemApi :: Proxy ItemApi
itemApi = Proxy

-- * app

run :: IO ()
run = do
  let port = 3001
      settings =
        setPort port $
        setBeforeMainLoop (System.IO.hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve itemApi server

server :: Server ItemApi
server =
  getPasswords :<|>
  updatePasswords

sha1 :: Data.ByteString.ByteString -> Digest SHA1
sha1 = Crypto.Hash.hash

testFile :: String
testFile = "test.json"

backupFile :: Integer -> String
backupFile version = "backup-" ++ (show version) ++ ".json"

oldLibversion :: Handler Integer
oldLibversion = do
  lib <- getPasswords
  innerLib <- getInnerLib lib
  return $ libraryVersion innerLib

doubleHash :: IO String
doubleHash = System.IO.readFile "double-hash.txt"

getInnerLib :: UncheckedLibrary -> Handler InnerLibrary
getInnerLib = decodeOrFail . Data.Text.Lazy.Encoding.encodeUtf8 . Data.Text.Lazy.fromStrict . inner

decodeOrFail :: (FromJSON a) => Data.ByteString.Lazy.ByteString -> Handler a
decodeOrFail x = case (decode x) of Just y -> return y
                                    Nothing -> throwError $ err503 { errBody = "failed json decode" }

-- TODO: assert api version is as expected
updatePasswords :: UpdateRequest -> Handler NoContent
updatePasswords UpdateRequest { hash = submittedHash, newLib = lib } = do
  innerLib <- getInnerLib lib
  targetHash <- liftIO doubleHash
  if targetHash == show $ sha1 $ Data.Text.Encoding.encodeUtf8 submittedHash
    then return ()
    else throwError $ err400 { errBody = "invalid password hash yo!" }
  let newVersion = libraryVersion innerLib
  oldVersion <- oldLibversion
  if (oldVersion + 1 == newVersion)
    then do
      liftIO $ renameFile testFile (backupFile oldVersion)
      liftIO $ Data.ByteString.Lazy.writeFile testFile (encode lib)
      return NoContent
    else throwError $ err503 { errBody = "version mismatch" }

getPasswords :: Handler UncheckedLibrary
getPasswords = do
  bs <- liftIO $ Data.ByteString.Lazy.readFile testFile
  lib <- decodeOrFail bs
  getInnerLib lib >> return lib
