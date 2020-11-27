module Main where

import Response
import Request
import Router
import Views
import Utils
import Http

table = [ Route indexGet "/" GET
        , Route helloGet "/hello" GET ]

main = do
  response <- resolve table (Request [] "/hello" GET)
  print $ response