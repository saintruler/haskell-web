module Http where

import Data.Text (Text)

data Header = Header Text Text
  deriving Show

data Method = GET | PUT | POST
  deriving (Show, Eq)

data QueryPair = QueryPair Text Text
  deriving Show
