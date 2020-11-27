module Web.Router where

import Web.Response
import Web.Request
import Web.Http

-- Route callback url method
data Route = Route (Request -> IO Response) String Method

resolve :: [Route] -> Request -> IO Response
resolve [] _ = return notFoundResponse
resolve (Route callback routeUrl _ : routerTable) req @ (Request _ url _) =
  if url == routeUrl then
    callback req
  else resolve routerTable req
