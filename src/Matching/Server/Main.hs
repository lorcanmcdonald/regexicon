{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Main where
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Data.Monoid
import Data.String.Conv
import Data.Text (Text, stripPrefix, stripSuffix)
import qualified Data.Text.Lazy.Encoding as T
import Matching
import Matching.Server.Views
import Network.HTTP.Types.Status (requestTimeout408)
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import Web.Scotty as S

main :: IO ()
main = scotty 80 $ do
  middleware logStdout
  get "/" $ do
    re <- param "q" `rescue` (\ _ -> return "")
    n <- param "n" `rescue` (\ _ -> return 5)
    let n' = if n <= 20 then n else 20
    result <- liftIO $ race (quitAfter (20 * 1000)) (selectMatches n' . toS . cleanRE $ re)
    case result of
      Left _ -> html . T.decodeUtf8 $ landingPage (RegexResults [])
      Right candidates -> html . T.decodeUtf8 $ landingPage candidates
  get "/js/:file" $ do
    f <- param "file"
    file $ "./js/" <> f
  get "/style/:file" $ do
    f <- param "file"
    file $ "./style/" <> f
  post "/regex/" $ do
    re <- body
    n <- param "n" `rescue` (\ _ -> return 15)
    let n' = if n <= 20 then n else 20
    result <- liftIO $ race (quitAfter (20 * 1000)) (selectMatches n' . toS . cleanRE . toS $ re)
    case result of
      Left _ -> do
        S.status requestTimeout408
        S.text "Response was too large"
      Right reList -> S.json reList

cleanRE :: Text -> Text
cleanRE (stripPrefix "^" -> Just suf) = cleanREEnd suf
cleanRE re = cleanREEnd re

cleanREEnd :: Text -> Text
cleanREEnd (stripSuffix "$" -> Just pref) = pref
cleanREEnd re = re
