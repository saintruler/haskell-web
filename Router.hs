module Router where

import Control.Exception
import Response
import Request

data Route = Route (Request -> IO Response) String String

data RouterError = RouteNotFound
  deriving Show

instance Exception RouterError

getResponse [] _ = throw RouteNotFound
getResponse (Route callback routeUrl _ : routerTable) req @ (Request _ url _) =
  if url == routeUrl then
    callback req
  else getResponse routerTable req

router table req =
  
