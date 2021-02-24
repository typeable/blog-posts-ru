{-# LANGUAGE OverloadedStrings #-}
module Main where

import Reflex.Dom

main :: IO ()
main = mainWidget $ el "h1" $ text "Hello, reflex!"
