module Routes where

import qualified Data.Text as T

import Web.Response
import Web.Request
import Web.Router
import Web.Utils
import Web.Http

import Views

routesTable = [ Route indexGet (T.pack "/") GET
              , Route helloGet (T.pack "/hello") GET ]