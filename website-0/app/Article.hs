module Article where

import Article.ThisBlog as ThisBlog
import Data.ByteString.Lazy.Internal
import Data.Char (toLower)
import Data.List (intercalate)
import Data.String (fromString)
import Lucid
import Template (Informations)

toPath :: String -> FilePath
toPath p = map toLower $ intercalate "-" . words $ p

articles :: [(FilePath, Informations)]
articles =
  [ (toPath ThisBlog.title, (ThisBlog.author, ThisBlog.date, ThisBlog.title, ThisBlog.description, ThisBlog.content)),
    (toPath ThisBlog.title, (ThisBlog.author, ThisBlog.date, ThisBlog.title, ThisBlog.description, ThisBlog.content))
  ]
