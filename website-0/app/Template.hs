{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Template where

import Control.Monad.IO.Class (liftIO)
import Data.Monoid
import Data.String (fromString)
import Data.Text as T
import Lucid
import Lucid.Html5
import System.IO (readFile)
import Web.Twain

type Informations = (String, String, String, String, HtmlC)

type HtmlC = HtmlT IO ()

loadSVG :: IO String -> HtmlC
loadSVG c = do
  svg <- liftIO c
  toHtmlRaw svg

svg = (loadSVG . readFile)

header :: String -> String -> HtmlC -> HtmlC
header t l c =
  doctypehtml_
    ( do
        ( head_
            ( do
                meta_ [charset_ "utf-8"]
                meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
                title_ (toHtml t)
                link_ [rel_ "stylesheet", href_ (T.append (fromString "/static/") (fromString l))]
            )
          )
        body_ [class_ "flex justify-center m-auto max-w-5xl"] $ do
          div_
            [class_ "w-full sm:w-9/12 sm:flex sm:flex-col sm:justify-center sm:items-start"]
            ( do
                header_
                  [class_ "w-full"]
                  ( do
                      ol_
                        [class_ "ol-inline border-b border-gray-300 py-1 mx-2"]
                        ( do
                            li_ (a_ [href_ "/", class_ "text-neutral-600 font-medium !no-underline mr-3"] "Home")
                            li_ (a_ [href_ "/about", class_ "text-neutral-600 font-medium !no-underline"] "About")
                        )
                  )
                div_ [class_ "w-full"] c
            )
          footer_
            [class_ "absolute bottom-0 mt-4"]
            ( ol_
                [class_ "inline-block ol-inline text-center inset-x-1/2 py-3 mx-2"]
                ( do
                    li_ [class_ "mx-6"] (a_ [href_ "https://twitter.com/lokasku"] $ svg "assets/svg/twitter.svg")
                    li_ [class_ "mx-6"] (a_ [href_ "https://github.com/Lokasku"] $ svg "assets/svg/github.svg")
                    li_ [class_ "mx-6"] (a_ [href_ "mailto:lukasku@proton.me"] $ svg "assets/svg/mail.svg")
                )
            )
    )

article :: Informations -> HtmlC
article (a, d, t, _, c) = do
  div_
    [class_ "mx-2 mt-6 mb-5 pb-3 border-b border-gray-300"]
    ( do
        h1_ [class_ "text-2xl mb-0.5"] (fromString t)
        p_
          [class_ "text-sm text-neutral-600"]
          ( (fromString "Written by ")
              <> span_ [class_ "text-sm text-red-400"] (fromString a)
              <> (fromString " on ")
              <> span_ [class_ "text-sm"] (fromString d)
              <> (fromString ".")
          )
    )
  div_ [class_ "px-2"] c
