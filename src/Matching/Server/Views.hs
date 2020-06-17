{-# LANGUAGE OverloadedStrings #-}

module Matching.Server.Views where

import Control.Concurrent
import Data.ByteString.Lazy (ByteString)
import Data.String.Conv
import Data.Text (Text)
import Matching
import Text.Blaze.Html.Renderer.Utf8
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

landingPage :: RegexResults -> ByteString
landingPage results = renderHtml . H.docTypeHtml $ do
  H.head $ do
    H.title "Regular Expressions"
    H.meta
      ! A.name "viewport"
      ! A.content "width=device-width, initial-scale=1"
    (H.link ! rel "stylesheet") ! href "style/style.css"
    (H.link ! rel "stylesheet")
      ! href "style/mobile.css"
      ! media "screen and (max-device-width: 480px)"
      ! href "mobile.css"
  H.body $ do
    H.label ! A.for "q" $
      "Generate random strings which match a regular expression"
    H.form ! A.method "GET" ! A.action "/" $ do
      H.input
        ! A.name "q"
        ! A.id "q"
        ! A.autofocus ""
        ! A.placeholder "[0-9a-f]{32}"
      H.button "\128269"
    case results of
      (RegexResults _) -> resultList
      _ -> examples

    H.script ! src "/js/bundle.js" $ ""
  where
    toResults :: RegexResults -> [Text]
    toResults (RegexResults candidates) = fmap toS candidates
    toResults RegexTimeout = ["Regular expression too complex to calculate"]
    toResults (RegexParseFailure _) = ["Could not parse regular expression"]
    examples =
      H.div ! A.class_ "examples" $ do
        H.span "e.g.: "
        H.ul $ do
          H.li $ H.a ! A.href "/?q=[0-9a-f]{32}" $ "[0-9a-f]{32}"
          H.li $ H.a ! A.href "/?q=[\128512-\128522]+" $ "[\128512-\128522]+"
    resultList =
      H.ul ! A.class_ "results" $ mapM_ (H.li . text) $ toResults results

selectMatches :: Int -> ByteString -> IO RegexResults
selectMatches n = matches n . toS

quitAfter :: Int -> IO ()
quitAfter n = do
  threadDelay n
  return ()
