module Main where

import System.IO

import Response
import Request
import Router
import Views
import Utils

renderTemplate name = do
  template <- readTemplate name
  return $ HtmlResponse 200 template

readTemplate name = do
  handle <- openFile ("templates/" ++ name) ReadMode
  hGetContents handle

table = [ Route indexGet "/" "GET"
        , Route helloGet "/hello" "GET" ]

main = do
  response <- resolve table (Request "query" "/jopa" "GET")
  print $ getContent response