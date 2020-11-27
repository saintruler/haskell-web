module Web.Request where

import Web.Http

-- Request query url method
data Request = Request [QueryPair] String Method
  deriving Show

getQuery (Request query _ _) = query

getUrl (Request _ url _) = url

getMethod (Request _ _ method) = method