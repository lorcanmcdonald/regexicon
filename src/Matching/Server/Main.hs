{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Concurrent.Async
import Control.Monad.IO.Class
import Data.String.Conv
import Data.Text (Text, stripPrefix, stripSuffix)
import qualified Data.Text.Lazy.Encoding as T
import Matching
import Matching.Server.Views
import Network.HTTP.Types.Status (badRequest400, requestTimeout408)
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import UnliftIO.Exception
import Web.Scotty
  ( body,
    file,
    get,
    html,
    middleware,
    post,
    scotty,
    setHeader,
  )
import qualified Web.Scotty as S

main :: IO ()
main = scotty 80 $ do
  middleware logStdout
  middleware . gzip $ defaultGzipSettings {gzipFiles = GzipCacheFolder "/tmp/"}
  get "/" $ do
    re <- S.queryParam "q" `S.catch` (\(SomeException _) -> return ("" :: Text))
    n <- S.queryParam "n" `S.catch` (\(SomeException _) -> return (5 :: Int))
    let n' = min n 20
    result <- liftIO $ race (quitAfter (20 * 1000)) (selectMatches n' . toS . cleanRE $ re)
    case result of
      Left _ -> html . T.decodeUtf8 $ landingPage (RegexResults [])
      Right candidates -> html . T.decodeUtf8 $ landingPage candidates
  get "/js/:file" $ do
    f <- S.captureParam "file"
    setHeader "Content-Type" "text/javascript"
    file $ "./js/" <> f
  get "/style/:file" $ do
    f <- S.captureParam "file"
    setHeader "Content-Type" "text/css"
    file $ "./style/" <> f
  post "/regex/" $ do
    re <- body
    n <- S.queryParam "n" -- `rescue` (\_ -> return (15:: Int))
    let n' = min n 20
    result <- liftIO $ race (quitAfter (20 * 1000)) (selectMatches n' . toS . cleanRE . toS $ re)
    case result of
      Left _ -> do
        S.status requestTimeout408
        S.text "Response was too large"
      Right reList@(RegexResults _) -> S.json reList
      Right timeout@RegexTimeout -> do
        S.status requestTimeout408
        S.json timeout
      Right failure@(RegexParseFailure _) -> do
        S.status badRequest400
        S.json failure

cleanRE :: Text -> Text
cleanRE (stripPrefix "^" -> Just suf) = cleanREEnd suf
cleanRE re = cleanREEnd re

cleanREEnd :: Text -> Text
cleanREEnd (stripSuffix "$" -> Just pref) = pref
cleanREEnd re = re
