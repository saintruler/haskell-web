module Request where

data Request = Request String String String

getQuery (Request query _ _) = query

getUrl (Request _ url _) = url

getMethod (Request _ _ method) = method