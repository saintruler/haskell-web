module Main where

import System.IO
import Response
import Router

renderTemplate name = do
  template <- readTemplate name
  return $ HtmlResponse 200 template

readTemplate name = do
  handle <- openFile ("templates/" ++ name) ReadMode
  hGetContents handle

route url method
  | url == "/"      = renderTemplate "index.html"
  | url == "/hello" = renderTemplate "hello.html"

table = [
  Route 
]

main = do
  response <- route "/hello" "GET"
  print $ getContent response