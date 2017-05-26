{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Monoid
import Data.String.Conv
import qualified Data.Text as TL
import qualified Data.Text.Lazy.Encoding as T
import Matching
import Matching.Server.Views
import Web.Scotty as S

main :: IO ()
main = scotty 3000 $ do
  get "/" . html . T.decodeUtf8 $ landingPage
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
    reList <- liftIO . matches n' . toS $ re
    S.json reList
