{-# LANGUAGE OverloadedStrings #-}
module Matching.Server.Views where
import Control.Concurrent
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.String.Conv
import Matching
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

landingPage :: RegexResults -> ByteString
landingPage (RegexResults candidates) = renderHtml . H.docTypeHtml $ do
    H.head $ do
        H.title "Regular Expressions"
        H.script ! src "https://code.jquery.com/jquery-2.1.3.min.js" $ ""
        H.script ! src "/js/client.js" $ ""
        (H.link ! rel "stylesheet") ! href "style/style.css"
    H.body $ do
        H.form ! A.method "GET" ! A.action "/" $ do
          H.input ! A.name "q" ! A.autofocus "" ! A.placeholder "Enter regular expression"
          H.button "🔍"
        ul ! class_ "results" $ mapM_ (li . text . toS) candidates

selectMatches :: Int -> ByteString -> IO RegexResults
selectMatches n = matches n . toS

quitAfter :: Int -> IO ()
quitAfter n = do
  threadDelay n
  return ()
