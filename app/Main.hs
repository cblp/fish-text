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
import Data.Text as T
import GHC.Generics (Generic)
import Network.Wreq (defaults, getWith, param, responseBody)
import Telegram.Bot.API qualified as Api

sendMessage :: Api.SomeChatId -> Text -> IO (Api.Response Api.Message)
sendMessage chatId html =
  runTelegramClient $
    Api.sendMessage (Api.defSendMessage chatId html)

makePost :: Text -> IO ()
makePost txt = do
  chId <- getEnv "TELEGRAM_BOT_CHANNEL_ID"
  void $ sendMessage (Api.SomeChatId (Api.ChatId (read chId))) txt

newtype FishTextResponse = FishTextResponse {text :: Text}
  deriving anyclass (FromJSON)
  deriving stock (Generic, Show)

getPostText :: IO Text
getPostText = do
  resp <-
    getWith
      (defaults & param "number" .~ [T.pack "1"])
      "http://fish-text.ru/get"
  obj <-
    Aeson.throwDecode $
      resp ^. responseBody

  pure $ obj & text

main :: IO ()
main = do
  loadFile defaultConfig
  forever $ do
    txt <- getPostText
    makePost txt
    threadDelay 300000000

runTelegramClient :: ClientM a -> IO a
runTelegramClient action = do
  httpManager <- newTlsManagerWith tlsManagerSettings
  token <- getEnv "TELEGRAM_BOT_TOKEN"
  let clientEnv = mkClientEnv httpManager $ Api.botBaseUrl (Api.Token $ T.pack token)
  eResult <- runClientM action clientEnv
  either throwIO pure eResult
