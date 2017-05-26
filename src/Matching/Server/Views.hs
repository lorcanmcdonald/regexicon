{-# LANGUAGE OverloadedStrings #-}
module Matching.Server.Views where
import qualified Data.ByteString.Lazy as BL
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

landingPage :: BL.ByteString
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
