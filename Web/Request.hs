module Web.Request where

import Data.Text (Text)

import Web.Http

-- Request query url method
data Request = Request [QueryPair] Text Method
  deriving Show

getQuery (Request query _ _) = query

getUrl (Request _ url _) = url

getMethod (Request _ _ method) = method