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

import           App hiding (getPasswords)

getPasswords :: ClientM UncheckedLibrary
updatePasswords :: UpdateRequest -> ClientM NoContent
getPasswords :<|> updatePasswords = client passwordApi

spec :: Spec
spec = do
  describe "/item" $ do
    withClient (mkApp ".") $ do
      it "returns passwords from disk" $ \ env -> do
        try env getPasswords `shouldReturn` UncheckedLibrary { hmak = "yo", inner = "lo" }

      -- it "allows to show items by id" $ \ env -> do
      --   try env (getItem 0) `shouldReturn` Item 0 "example item"

      -- it "throws a 404 for missing items" $ \ env -> do
      --   try env (getItem 42) `shouldThrow` (\ e -> responseStatus e == notFound404)

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
