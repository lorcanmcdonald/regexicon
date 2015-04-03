{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Aeson as JSON
import Data.Monoid
import Web.Scotty as S
import Matching.Server.Views
import Matching
import Control.Monad.IO.Class
import qualified Data.Text as TL
import qualified Data.Text.Lazy.Encoding as T

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    html . T.decodeUtf8 $ landingPage
  get "/js/:file" $ do
    f <- param "file"
    file $ "./js/" <> f
  post "/regex/" $ do
    re <- body
    n <- param "n" `rescue` (\ _ -> return 15)
    reList <- liftIO . matches n . show $ re
    S.json . fmap (JSON.String . TL.pack) $ reList
