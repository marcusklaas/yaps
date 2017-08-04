{-# LANGUAGE OverloadedStrings #-} 

module AppSpec where

import           Control.Exception (throwIO)
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import           Network.HTTP.Types
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           Test.Hspec
import           Data.Text

import           App hiding (getPasswords, updatePasswords)

getPasswords :: ClientM UncheckedLibrary
updatePasswords :: UpdateRequest -> ClientM NoContent
getPasswords :<|> updatePasswords = client passwordApi

libOnDisk :: UncheckedLibrary
libOnDisk = UncheckedLibrary { hmak = "hmak!", inner = "{\"blob\":\"/blob\",\"library_version\":1,\"api_version\":3,\"modified\":1337}" }

libVersionTwo :: UncheckedLibrary
libVersionTwo = UncheckedLibrary { hmak = "hmak!", inner = "{\"blob\":\"/blob\",\"library_version\":2,\"api_version\":3,\"modified\":1337}" }

expectedHash :: Text
expectedHash = (pack . show . sha1) "changeme"

spec :: Spec
spec = do
  describe "/passwords" $ do
    withClient (mkApp "test") $ do
      it "returns passwords from disk" $ \ env -> do
        try env getPasswords `shouldReturn` libOnDisk

      it "throws a 40x for invalid hash" $ \ env -> do
        let request = UpdateRequest { passwordHash = "wrong-hash!", newLib = libOnDisk, newHash = Nothing }
        try env (updatePasswords request) `shouldThrow` (\ e -> responseStatus e == forbidden403)

      it "throws a version mismatch when it isn't incremented" $ \ env -> do
        let request = UpdateRequest { passwordHash = expectedHash, newLib = libOnDisk, newHash = Nothing }
        try env (updatePasswords request) `shouldThrow` (\ e -> responseStatus e == status400 && responseBody e == "version mismatch")

      it "returns 2xx when all is ok" $ \ env -> do
        let request = UpdateRequest { passwordHash = expectedHash, newLib = libVersionTwo, newHash = Nothing }
        try env (updatePasswords request) `shouldReturn` NoContent
      -- TODO: test update of library
      -- TODO: test new master key

withClient :: IO Application -> SpecWith ClientEnv -> SpecWith ()
withClient x innerSpec =
  beforeAll (newManager defaultManagerSettings) $ do
    flip aroundWith innerSpec $ \ action -> \ manager -> do
      testWithApplication x $ \ port -> do
        let baseUrl = BaseUrl Http "localhost" port ""
        action (ClientEnv manager baseUrl)

type Host = (Manager, BaseUrl)

try :: ClientEnv -> ClientM a -> IO a
try clientEnv action = either throwIO return =<<
  runClientM action clientEnv
