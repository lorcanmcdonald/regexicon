{-# LANGUAGE OverloadedStrings #-}
module Matching.Server.Views where
import Control.Concurrent
import Data.ByteString.Lazy (ByteString)
import Data.String.Conv
import Matching
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

landingPage :: ByteString
landingPage = renderHtml . H.docTypeHtml $ do
    H.head $ do
        H.title "Regular Expressions"
        H.script ! src "https://code.jquery.com/jquery-2.1.3.min.js" $ ""
        H.script ! src "/js/client.js" $ ""
        (H.link ! rel "stylesheet") ! href "style/style.css"
    H.body $ do
        p "Enter regular expression"
        H.input
        ul ! class_ "results" $ ""

selectMatches :: Int -> ByteString -> IO RegexResults
selectMatches n = matches n . toS

quitAfter :: Int -> IO ()
quitAfter n = do
  threadDelay n
  return ()
