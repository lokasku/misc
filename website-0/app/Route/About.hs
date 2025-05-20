{-# LANGUAGE OverloadedStrings #-}

module Route.About where

import Lucid
import Lucid.Html5
import Template as T

about :: T.HtmlC
about = T.header "About" "output.css" $ div_ [class_ "mx-2 mt-6"] $ do
  p_ "Hi, I'm Luke (Lokasku, but also Lukasku) an enthusiastic apprentice developer. I'm 16 years old and my interests include programming languages development, kernels and low-level in general. The languages I use on a daily basis are Haskell and Rust, but I also know Common Lisp, Python and a few web languages. I plan to learn Coq and C."
  p_
    ( "I also love linguistics (I'm learning Quenya, Russian and Polish), mathematics and Minecraft. You can find me on the "
        <> a_ [href_ "https://openredstone.org"] "ORE"
        <> " by entering "
        <> code_ "/p t Lokasku"
        <> " (at least my plot)."
    )
  p_ "Here is some information about my development tools:"
  ul_
    []
    ( do
        li_ "NixOS"
        li_ "The Kakoune modal code editor"
        li_ "XMonad (the haskellien DE)"
        li_ "The Fish shell"
        li_ "Rog Zephyrus 14\""
    )

  hr_ []
  p_
    ( "This website was written by me (special thanks to "
        <> a_ [href_ "https://github.com/Mesabloo"] "Mesabloo"
        <> ") in Haskell using the Twain lib. You can find the sources "
        <> a_ [href_ "https://github.com/Lokasku/website"] "here"
        <> "."
    )
