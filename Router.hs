module Router where

import Response
import Request
import Http

-- Route callback url method
data Route = Route (Request -> IO Response) String Method

resolve :: [Route] -> Request -> IO Response
resolve [] _ = return notFoundResponse
resolve (Route callback routeUrl _ : routerTable) req @ (Request _ url _) =
  if url == routeUrl then
    callback req
  else resolve routerTable req
