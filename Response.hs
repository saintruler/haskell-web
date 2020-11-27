module Response where

data Response
  = HtmlResponse Int String -- Код возврата, содержимое HTML
  | TextResponse Int String String -- Код возврата, Content-Type, содержимое HTML
  deriving Show

notFoundResponse = HtmlResponse 404 "<strong>404 Not Found</strong>"

getStatusCode (HtmlResponse code _) = code
getStatusCode (TextResponse code _ _) = code

getContentType (HtmlResponse _ _) = "text/html"
getContentType (TextResponse _ contentType _) = contentType

getContent (HtmlResponse _ content) = content
getContent (TextResponse _ _ content) = content