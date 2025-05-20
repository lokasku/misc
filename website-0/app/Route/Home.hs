{-# LANGUAGE OverloadedStrings #-}

module Route.Home where

import Article
import Data.Functor.Identity
import Data.Monoid
import Data.String (fromString)
import Lucid
import Lucid.Html5
import Template as T

showArticles :: [(FilePath, T.Informations)] -> T.HtmlC
showArticles [] = mempty
showArticles ((p, (a, d, t, de, _)) : xs) =
  div_
    [class_ "border-b border-gray-300 p-2 mt-1 w-full"]
    ( do
        p_
          [class_ "text-neutral-600"]
          ( span_ [class_ "text-red-400"] (fromString a)
              <> span_ (fromString ", ")
              <> span_ (fromString d)
          )
        p_ [class_ "mt-0.5"] $ a_ [href_ (fromString p), class_ "font-normal text-xl text-black !no-underline"] (fromString t)
        -- p_ [class_ "text-neutral-500 text-[30px]"] (fromString de)
    )
    <> showArticles xs

home :: HtmlC
home = T.header "Home" "output.css" $ showArticles Article.articles
