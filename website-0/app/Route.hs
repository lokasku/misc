{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Route where

import Article (articles)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy as DBL
import Data.ByteString.Lazy.Internal
import Data.List as List
import Lucid
import Route.About as About
import Route.Home as Home
import Template as T
import Web.Twain
import Web.Twain.Types

get' :: PathPattern -> ByteString -> Middleware
get' fp c = get fp $ send $ html c

routes :: [Middleware]
routes =
  [ get "/" $ do
      content <- liftIO $ renderBST Home.home
      send (html content),
    get "/about" $ do
      content <- liftIO $ renderBST About.about
      send (html content),
    get "/static/:file" $ do
      file <- pathParam "file"
      if ".css" `List.isSuffixOf` file
        then do
          content <- liftIO $ DBL.readFile ("static/" ++ file)
          send (css content)
        else send $ status status404 $ text "Pieprzyć swoją mamę.",
    {- get "/assets/:folder/:file" $ do
      folder <- pathParam "folder"
      file <- pathParam "file"
      content <- liftIO $ DBL.readFile ("assets/" ++ folder ++ "/" ++ file)
      send (xml content), -}
    get "/:article" $ do
      name <- pathParam "article"
      case lookup name articles of
        Nothing -> next
        Just (a, d, t, de, c) -> do
          content <- liftIO $ renderBST (T.header t "output.css" (T.article (a, d, t, de, c)))
          send (html content)
  ]
