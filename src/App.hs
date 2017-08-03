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
import           Data.Text
import           Data.Text.Lazy
import           Data.Text.Lazy.Encoding
import           Crypto.Hash
import           Web.FormUrlEncoded
import           Data.Either.Combinators

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
                                   , passwordHash :: Data.Text.Text
                                   , newHash :: Maybe Data.Text.Text }

parseLibFromText :: Data.Text.Text -> Either Data.Text.Text UncheckedLibrary
parseLibFromText libText =
  case (decode . Data.Text.Lazy.Encoding.encodeUtf8 . Data.Text.Lazy.fromStrict) libText
    of Just lib -> Right lib
       Nothing -> Left "Couldn't decode library"

instance FromForm UpdateRequest where
  fromForm inputs = do
    passwordHash <- lookupUnique "pwhash" inputs
    libText <- lookupUnique "newlib" inputs
    newLib <- parseLibFromText libText
    let newHash = rightToMaybe $ lookupUnique "newhash" inputs
    return UpdateRequest{..}

instance FromHttpApiData UncheckedLibrary where
  parseQueryParam = parseLibFromText

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
  libraryVersion <$> getInnerLib lib

getInnerLib :: UncheckedLibrary -> Handler InnerLibrary
getInnerLib = decoderHandle . Data.Text.Lazy.Encoding.encodeUtf8 . Data.Text.Lazy.fromStrict . inner

decoderHandle :: (FromJSON a) => Data.ByteString.Lazy.ByteString -> Handler a
decoderHandle x = case (decode x) of Just y -> return y
                                     Nothing -> throwError $ err503 { errBody = "failed json decode" }

assertVersion :: Integer -> InnerLibrary -> Handler ()
assertVersion i lib = if libraryVersion lib == i
  then return ()
  else throwError $ err400 { errBody = "invalid API version" }

assertHash :: Data.Text.Text -> Handler ()
assertHash submittedHash = do
  targetHash <- liftIO $ System.IO.readFile "double-hash.txt"
  if targetHash == (show . sha1 . Data.Text.Encoding.encodeUtf8) submittedHash
    then return ()
    else throwError $ err400 { errBody = "invalid password hash yo!" }

writeLib :: UncheckedLibrary -> Integer -> Handler ()
writeLib lib oldVersion = do
  innerLib <- getInnerLib lib
  if oldVersion + 1 == libraryVersion innerLib
    then do
      liftIO $ renameFile testFile (backupFile oldVersion)
      liftIO $ Data.ByteString.Lazy.writeFile testFile (encode lib)
    else throwError $ err503 { errBody = "version mismatch" }

updateMasterPass :: Maybe Data.Text.Text -> Handler ()
-- TODO: implement!
updateMasterPass _ = return ()

updatePasswords :: UpdateRequest -> Handler NoContent
updatePasswords UpdateRequest { passwordHash = submittedHash, newLib = lib, newHash = maybeNewHash } = do
  innerLib <- getInnerLib lib
  assertVersion 3 innerLib
  assertHash submittedHash
  writeLib lib =<< oldLibversion
  updateMasterPass maybeNewHash
  return NoContent

getPasswords :: Handler UncheckedLibrary
getPasswords = do
  bs <- liftIO $ Data.ByteString.Lazy.readFile testFile
  lib <- decoderHandle bs
  getInnerLib lib >> return lib
