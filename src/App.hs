{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE RecordWildCards #-}

module App where

-- import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Data.Aeson
-- import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import           Data.ByteString.Lazy

-- * data types

-- TODO: the inner thing should be an InnerLibrary instead of a String
data UncheckedLibrary = UncheckedLibrary { inner :: String
                                         , hmak :: String
                                         } deriving (Eq, Show)

data InnerLibrary = InnerLibrary { innerBlob :: String
                                 , libraryVersion :: Integer
                                 , apiVersion :: Integer
                                 , modified :: Integer
                                 } deriving (Eq, Show)

instance FromJSON UncheckedLibrary where
  parseJSON = withObject "pair" $ \o -> do
    inner <- o .: "library"
    hmak <- o .: "hmac"
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
      "inner" .= inner o,
      "hmac" .= hmak o ]


-- * api

type ItemApi =
  "passwords" :> Get '[JSON] UncheckedLibrary

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
  getPasswords

-- ffs :: IO (Maybe UncheckedLibrary)
-- ffs = decode <$> Data.ByteString.Lazy.readFile "test.json"

-- run = System.IO.putStrLn =<< (show <$> ffs)


getPasswords :: Handler UncheckedLibrary
getPasswords = do
  innerLib <- liftIO $ decode <$> Data.ByteString.Lazy.readFile "test.json"
  case innerLib of Just lib -> return lib
                   Nothing -> throwError $ err503 { errBody = "failed json decode" }
