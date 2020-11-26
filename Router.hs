module Router where

import Control.Exception
import Response
import Request

-- Route callback url method
data Route = Route (Request -> IO Response) String String

data RouterError = RouteNotFound
  deriving Show

instance Exception RouterError

resolve :: [Route] -> Request -> IO Response
resolve [] _ = return notFoundResponse
resolve (Route callback routeUrl _ : routerTable) req @ (Request _ url _) =
  if url == routeUrl then
    callback req
  else resolve routerTable req
