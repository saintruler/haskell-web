module Views where

import System.IO
import qualified Data.Text as T
import Data.Text (Text)

import Web.Request
import Web.Response

indexGet :: Request -> IO Response
indexGet req = renderTemplate "index.html"

helloGet :: Request -> IO Response
helloGet req = renderTemplate "hello.html"

renderTemplate :: String -> IO Response
renderTemplate name = do
  template <- readTemplate name
  return $ HtmlResponse 200 template

readTemplate :: String -> IO Text
readTemplate name = do
  handle <- openFile ("templates/" ++ name) ReadMode
  contents <- hGetContents handle
  return $ T.pack contents
