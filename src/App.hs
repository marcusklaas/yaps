{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}

module App where

import           Data.Monoid
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
import           System.FilePath.Posix

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
  toForm req = case (newHash req) of
      Just x -> [ ("newhash", toQueryParam x) ] <> form
      Nothing -> [] <> form

    where form = [ ("pwhash", toQueryParam (passwordHash req)),
                   ("newlib", toQueryParam (newLib req)) ]

instance ToHttpApiData UncheckedLibrary where
  toQueryParam = Data.Text.Encoding.decodeUtf8 . Data.ByteString.Lazy.toStrict . encode

-- * api

type PasswordApi =
  "passwords" :> Get '[JSON] UncheckedLibrary :<|>
  "passwords" :> ReqBody '[FormUrlEncoded] UpdateRequest :> PostNoContent '[JSON] NoContent

passwordApi :: Proxy PasswordApi
passwordApi = Proxy

-- * app

run :: String -> Int -> IO ()
run dir port = do
  let settings =
        setPort port $
        setBeforeMainLoop (System.IO.hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp dir

mkApp :: String -> IO Application
mkApp = return . (serve passwordApi) . server 

server :: String -> Server PasswordApi
server dir =
  getPasswords dir :<|>
  updatePasswords dir

sha1 :: Data.ByteString.ByteString -> Digest SHA1
sha1 = Crypto.Hash.hash

testFile :: String
testFile = "passwords.txt"

backupFile :: Integer -> String
backupFile version = "backup-" ++ (show version) ++ ".json"

doubleHashFile :: String
doubleHashFile = "passhash.txt"

oldLibversion :: String -> Handler Integer
oldLibversion dir = do
  lib <- getPasswords dir
  libraryVersion <$> getInnerLib lib

getInnerLib :: UncheckedLibrary -> Handler InnerLibrary
getInnerLib = decoderHandle . Data.Text.Lazy.Encoding.encodeUtf8 . Data.Text.Lazy.fromStrict . inner

decoderHandle :: (FromJSON a) => Data.ByteString.Lazy.ByteString -> Handler a
decoderHandle = (maybe (throwError err503 { errBody = "failed json decode" }) return) . decode

assertVersion :: Integer -> InnerLibrary -> Handler ()
assertVersion i lib = if apiVersion lib == i
  then return ()
  else throwError $ err400 { errBody = "invalid API version" }

hashText :: Data.Text.Text -> String
hashText = show . sha1 . Data.Text.Encoding.encodeUtf8

assertHash :: String -> Data.Text.Text -> Handler ()
assertHash dir submittedHash = do
  targetHash <- liftIO $ System.IO.readFile $ dir </> doubleHashFile
  if targetHash == hashText submittedHash
    then return ()
    else throwError $ err403 { errBody = "invalid password hash yo!" }

writeLib :: String -> UncheckedLibrary -> Integer -> Handler ()
writeLib dir lib oldVersion = do
  innerLib <- getInnerLib lib
  if oldVersion + 1 == libraryVersion innerLib
    then do
      liftIO $ renameFile (dir </> testFile) (dir </> backupFile oldVersion)
      liftIO $ Data.ByteString.Lazy.writeFile (dir </> testFile) (encode lib)
    else throwError $ err400 { errBody = "version mismatch" }

updateMasterPass :: String -> Maybe Data.Text.Text -> Handler ()
updateMasterPass _ Nothing = return ()
updateMasterPass dir (Just singleHash) = liftIO $ System.IO.writeFile (dir </> doubleHashFile) $ hashText singleHash

updatePasswords :: String -> UpdateRequest -> Handler NoContent
updatePasswords dir UpdateRequest { passwordHash = submittedHash, newLib = lib, newHash = maybeNewHash } = do
  innerLib <- getInnerLib lib
  assertVersion 3 innerLib
  assertHash dir submittedHash
  writeLib dir lib =<< oldLibversion dir
  updateMasterPass dir maybeNewHash
  return NoContent

getPasswords :: String -> Handler UncheckedLibrary
getPasswords dir = do
  bs <- liftIO $ Data.ByteString.Lazy.readFile $ dir </> testFile
  lib <- decoderHandle bs
  getInnerLib lib >> return lib
