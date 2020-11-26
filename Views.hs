module Views where

import Request
import Response

indexGet (Request query url method) =
  return $ HtmlResponse 200 "<strong>index</strong>"

helloGet req =
  return $ HtmlResponse 200 "<i>hello</i>"