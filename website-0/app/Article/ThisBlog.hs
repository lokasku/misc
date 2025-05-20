{-# LANGUAGE OverloadedStrings #-}

module Article.ThisBlog
  ( author,
    date,
    title,
    description,
    content,
  )
where

import Data.Monoid
import Lucid
import Lucid.Html5

author = "Luke"

date = "29 Saturday 2023"

title = "This blog"

description = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat."

content :: HtmlT IO ()
content = do
  div_
    ( do
        p_
          ( "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut "
              <> a_ [href_ "#"] "enim ad minim"
              <> " veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute "
              <> code_ "irure"
              <> " dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat "
              <> a_ [href_ "#"] "cupidatat"
              <> " non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
          )
        pre_ $
          "main :: IO ()\n\
          \main = putStrLn \"hello, everyone\""
    )
