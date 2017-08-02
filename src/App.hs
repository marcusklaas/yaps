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

sha1 :: Data.ByteString.ByteString -> Digest SHA1
sha1 = hash

-- * data types

data UncheckedLibrary = UncheckedLibrary { inner :: InnerLibrary
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
    innerString <- o .: "library"
    case (decode $ Data.Text.Lazy.Encoding.encodeUtf8 innerString)
      of Just inner -> return UncheckedLibrary { .. }
         Nothing -> fail "Couldn't decode library"

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
      "inner" .= (Data.Text.Lazy.Encoding.decodeUtf8 . encode . inner) o,
      "hmac" .= hmak o ]

instance FromHttpApiData UncheckedLibrary where
  parseQueryParam p = case (decode . Data.Text.Lazy.Encoding.encodeUtf8 . Data.Text.Lazy.fromStrict) p
    of Just lib -> Right lib
       Nothing -> Left "Couldn't decode library"

-- * api

type ItemApi =
  "passwords" :> Get '[JSON] UncheckedLibrary :<|>
  "passwords" :> QueryParam "pwhash" Data.Text.Text :> QueryParam "newlib" UncheckedLibrary :> PostNoContent '[JSON] NoContent

itemApi :: Proxy ItemApi
itemApi = Proxy

-- * app

run :: IO ()
run = do
  let port = 3000
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

testFile :: String
testFile = "test.json"

backupFile :: Integer -> String
backupFile version = "backup-" ++ (show version) ++ ".json"

oldLibversion :: IO (Maybe Integer)
oldLibversion = 
  (return . (fmap (libraryVersion . inner)) . decode) =<< Data.ByteString.Lazy.readFile testFile

doubleHash :: IO Data.ByteString.ByteString
doubleHash = Data.ByteString.readFile "double-hash.txt"

updatePasswords :: Maybe Data.Text.Text -> Maybe UncheckedLibrary -> Handler NoContent
updatePasswords (Just submittedHash) (Just lib) = do
  targetHash <- liftIO doubleHash
  if digestFromByteString targetHash == Just (sha1 $ Data.Text.Encoding.encodeUtf8 submittedHash)
    then return ()
    else throwError $ err400 { errBody = "invalid password hash yo!" }
  let newVersion = (libraryVersion . inner) lib
  oldVersion <- liftIO oldLibversion
  case (oldVersion, newVersion) of
    (Just old, new) -> if (old + 1 == new)
      then do
        liftIO $ renameFile testFile (backupFile old)
        liftIO $ Data.ByteString.Lazy.writeFile testFile (encode lib)
        return NoContent
      else throwError $ err503 { errBody = "version mismatch" }
    (_, _) -> throwError $ err503 { errBody = "decoding error yo!!" }
updatePasswords _ _ = throwError $ err503 { errBody = "missing or incorrect params" }

getPasswords :: Handler UncheckedLibrary
getPasswords = do
  innerLib <- liftIO $ decode <$> Data.ByteString.Lazy.readFile testFile
  case innerLib of Just lib -> return lib
                   Nothing -> throwError $ err503 { errBody = "failed json decode" }
