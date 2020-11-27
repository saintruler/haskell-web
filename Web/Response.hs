module Web.Response where

import qualified Data.Text as T
import Data.Text (Text)

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
