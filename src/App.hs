{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}

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

instance ToForm UpdateRequest where
  toForm req = [
    ("pwhash", toQueryParam (passwordHash req)),
    ("newlib", toQueryParam (newLib req)),
    ("newhash", toQueryParam (newHash req)) ]

instance ToHttpApiData UncheckedLibrary where
  toQueryParam = Data.Text.Encoding.decodeUtf8 . Data.ByteString.Lazy.toStrict . encode

-- * api

type PasswordApi =
  "passwords" :> Get '[JSON] UncheckedLibrary :<|>
  "passwords" :> ReqBody '[FormUrlEncoded] UpdateRequest :> PostNoContent '[JSON] NoContent

passwordApi :: Proxy PasswordApi
passwordApi = Proxy

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
mkApp = return $ serve passwordApi server

server :: Server PasswordApi
server =
  getPasswords :<|>
  updatePasswords

sha1 :: Data.ByteString.ByteString -> Digest SHA1
sha1 = Crypto.Hash.hash

testFile :: String
testFile = "test.json"

backupFile :: Integer -> String
backupFile version = "backup-" ++ (show version) ++ ".json"

doubleHashFile :: String
doubleHashFile = "double-hash.txt"

oldLibversion :: Handler Integer
oldLibversion = do
  lib <- getPasswords
  libraryVersion <$> getInnerLib lib

getInnerLib :: UncheckedLibrary -> Handler InnerLibrary
getInnerLib = decoderHandle . Data.Text.Lazy.Encoding.encodeUtf8 . Data.Text.Lazy.fromStrict . inner

-- TODO: there are some nice helper functions that turn Maybe's and Eithers into ErrorMonads
decoderHandle :: (FromJSON a) => Data.ByteString.Lazy.ByteString -> Handler a
decoderHandle x = case (decode x) of Just y -> return y
                                     Nothing -> throwError $ err503 { errBody = "failed json decode" }

assertVersion :: Integer -> InnerLibrary -> Handler ()
assertVersion i lib = if apiVersion lib == i
  then return ()
  else throwError $ err400 { errBody = "invalid API version" }

hashText :: Data.Text.Text -> String
hashText = show . sha1 . Data.Text.Encoding.encodeUtf8

assertHash :: Data.Text.Text -> Handler ()
assertHash submittedHash = do
  targetHash <- liftIO $ System.IO.readFile doubleHashFile
  if targetHash == hashText submittedHash
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
updateMasterPass Nothing = return ()
updateMasterPass (Just singleHash) = liftIO $ System.IO.writeFile doubleHashFile $ hashText singleHash

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
