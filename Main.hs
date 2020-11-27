module Main where

import qualified Data.Text as T

import Web.Response
import Web.Request
import Web.Router
import Web.Views
import Web.Utils
import Web.Http

table = [ Route indexGet (T.pack "/") GET
        , Route helloGet (T.pack "/hello") GET ]

main = do
  response <- resolve table (Request [] (T.pack "/hello") GET)
  print $ formResponse response