module Web.Views where

import System.IO

import Web.Request
import Web.Response

indexGet (Request query url method) =
  return $ HtmlResponse 200 "<strong>index</strong>"

helloGet req =
  return $ HtmlResponse 200 "<i>hello</i>"

renderTemplate name = do
  template <- readTemplate name
  return $ HtmlResponse 200 template

readTemplate name = do
  handle <- openFile ("templates/" ++ name) ReadMode
  hGetContents handle