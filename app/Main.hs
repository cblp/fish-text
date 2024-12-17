{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Exception (throwIO)
import Control.Lens ((.~), (^.))
import Control.Monad (forever, void)
import Data.Aeson (FromJSON)
import Data.Aeson qualified as Aeson
import Data.Function ((&))
import Data.String (fromString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Network.Wreq (defaults, getWith, param, responseBody)
import Servant.Client (ClientM, mkClientEnv, runClientM)
import System.Environment (getEnv)
import Telegram.Bot.API qualified as Api
import Time (minute, threadDelay)

makePost :: Text -> IO ()
makePost txt = do
  chId <-
    Api.SomeChatId . Api.ChatId . read <$> getEnv "TELEGRAM_BOT_CHANNEL_ID"
  token <- fromString <$> getEnv "TELEGRAM_BOT_TOKEN"
  void $ runTelegramClient token $ Api.sendMessage $ Api.defSendMessage chId txt

newtype FishTextResponse = FishTextResponse {text :: Text}
  deriving anyclass (FromJSON)
  deriving stock (Generic, Show)

getPostText :: IO Text
getPostText = do
  resp <-
    getWith (defaults & param "number" .~ ["1"]) "https://fish-text.ru/get"
  FishTextResponse{text} <- Aeson.throwDecode $ resp ^. responseBody
  pure text

main :: IO ()
main = do
  loadFile defaultConfig
  forever do
    txt <- getPostText
    makePost txt
    threadDelay $ minute 5

runTelegramClient :: Api.Token -> ClientM a -> IO a
runTelegramClient token action = do
  httpManager <- newTlsManagerWith tlsManagerSettings
  let clientEnv = mkClientEnv httpManager $ Api.botBaseUrl token
  eResult <- runClientM action clientEnv
  either throwIO pure eResult
