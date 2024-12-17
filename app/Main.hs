{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import System.Environment

import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Servant.Client (
  ClientM,
  mkClientEnv,
  runClientM,
 )

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Lens ((.~), (^.))
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.Function ((&))
import Data.String
import Data.Text as T
import GHC.Generics (Generic)
import Network.Wreq (defaults, getWith, param, responseBody)
import Telegram.Bot.API qualified as Api

makePost :: Text -> IO ()
makePost txt = do
  chId <- Api.SomeChatId . Api.ChatId . read <$> getEnv "TELEGRAM_BOT_CHANNEL_ID"
  token <- fromString <$> getEnv "TELEGRAM_BOT_TOKEN"
  void $
    runTelegramClient token $
      Api.sendMessage $
        Api.defSendMessage chId txt

newtype FishTextResponse = FishTextResponse {text :: Text}
  deriving anyclass (FromJSON)
  deriving stock (Generic, Show)

getPostText :: IO Text
getPostText = do
  resp <-
    getWith
      (defaults & param "number" .~ ["1"])
      "http://fish-text.ru/get"
  obj <-
    Aeson.throwDecode $
      resp ^. responseBody

  pure $ text obj

main :: IO ()
main = do
  loadFile defaultConfig
  forever $ do
    txt <- getPostText
    makePost txt
    threadDelay 300000000

runTelegramClient :: Api.Token -> ClientM a -> IO a
runTelegramClient token action = do
  httpManager <- newTlsManagerWith tlsManagerSettings
  let clientEnv = mkClientEnv httpManager $ Api.botBaseUrl token
  eResult <- runClientM action clientEnv
  either throwIO pure eResult
