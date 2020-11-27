module Web.Views where

import System.IO
import qualified Data.Text as T
import Data.Text (Text)

import Web.Request
import Web.Response

indexGet (Request query url method) =
  return $ HtmlResponse 200 (T.pack "<strong>index</strong>")

helloGet req =
  return $ HtmlResponse 200 (T.pack "<i>hello</i>")

renderTemplate :: String -> IO Response
renderTemplate name = do
  template <- readTemplate name
  return $ HtmlResponse 200 template

readTemplate :: String -> IO Text
readTemplate name = do
  handle <- openFile ("templates/" ++ name) ReadMode
  contents <- hGetContents handle
  return $ T.pack contents
