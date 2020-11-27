module Main where

import Web.Response
import Web.Request
import Web.Router
import Web.Views
import Web.Utils
import Web.Http

table = [ Route indexGet "/" GET
        , Route helloGet "/hello" GET ]

main = do
  response <- resolve table (Request [] "/hello" GET)
  print $ response