module Web.Response where

import qualified Data.Text as T
import Data.Text (Text)

import Web.Utils

data Response
  = HtmlResponse Int Text -- Код возврата, содержимое HTML
  | TextResponse Int Text Text -- Код возврата, Content-Type, содержимое HTML
  deriving Show

notFoundResponse = HtmlResponse 404 (T.pack "<strong>404 Not Found</strong>")

getStatusCode (HtmlResponse code _) = code
getStatusCode (TextResponse code _ _) = code

getContentType (HtmlResponse _ _) = (T.pack "text/html")
getContentType (TextResponse _ contentType _) = contentType

getContent (HtmlResponse _ content) = content
getContent (TextResponse _ _ content) = content

formResponse :: Response -> Text
formResponse (HtmlResponse code html) = 
  T.strip $ T.unlines
  $ map T.pack [ "HTTP/1.1 " ++ getStatus code
               , "Content-Type: text/html; charset=utf-8"
               , "Connection: close"
               , "" ] ++ [html]
