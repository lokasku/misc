{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Route as R
import Web.Twain as Twain

main :: IO ()
main = do
  run 1024 myApp

myApp :: Application
myApp = foldr ($) (notFound $ send $ html "<p>not found</p>") R.routes
