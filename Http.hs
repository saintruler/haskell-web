module Http where

import Data.Text (Text)

data Header = Header Text Text
data Method = GET | POST
data QueryPair = QueryPair Text Text
