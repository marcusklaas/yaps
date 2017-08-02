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
import           System.IO
import           Data.ByteString.Lazy
import Data.Text.Lazy
import           Data.Text.Lazy.Encoding

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
    case (decode $ encodeUtf8 innerString)
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
      "inner" .= (decodeUtf8 . encode . inner) o,
      "hmac" .= hmak o ]

instance FromHttpApiData UncheckedLibrary where
  parseQueryParam p = case (decode . encodeUtf8 . Data.Text.Lazy.fromStrict) p
    of Just lib -> Right lib
       Nothing -> Left "Couldn't decode library"

-- * api

type ItemApi =
  "passwords" :> Get '[JSON] UncheckedLibrary :<|>
  "passwords" :> QueryParam "pwhash" String :> QueryParam "newlib" UncheckedLibrary :> PostNoContent '[JSON] NoContent

itemApi :: Proxy ItemApi
itemApi = Proxy

-- * app

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
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

oldLibversion :: IO (Maybe Integer)
oldLibversion = 
  (return . (fmap (libraryVersion . inner)) . decode) =<< Data.ByteString.Lazy.readFile testFile

doubleHash :: IO ByteString
doubleHash = Data.ByteString.Lazy.readFile "double-hash.txt"

updatePasswords :: Maybe String -> Maybe UncheckedLibrary -> Handler NoContent
updatePasswords (Just hash) (Just lib) = do
  targetHash <- liftIO doubleHash
  -- let doubleHash = SHA1 hash
  let newVersion = (libraryVersion . inner) lib
  oldVersion <- liftIO oldLibversion
  case (oldVersion, newVersion) of
    (Just x, y) -> if (x + 1 == y)
      then return NoContent
      else throwError $ err503 { errBody = "version mismatch" }
    (_, _) -> throwError $ err503 { errBody = "decoding error yo!!" }
updatePasswords _ _ = throwError $ err503 { errBody = "missing or incorrect params" }


getPasswords :: Handler UncheckedLibrary
getPasswords = do
  innerLib <- liftIO $ decode <$> Data.ByteString.Lazy.readFile testFile
  case innerLib of Just lib -> return lib
                   Nothing -> throwError $ err503 { errBody = "failed json decode" }
