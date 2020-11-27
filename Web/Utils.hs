module Web.Utils (parseQs, parseHttp) where

import Network.HTTP.Types.URI as URI
import qualified Data.ByteString as S
import Data.ByteString (ByteString)

import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text (Text)

import Web.Http

-- Query string parser

decodeUrl :: Text -> Text
decodeUrl = decodeUtf8 . URI.urlDecode True . encodeUtf8

parseQs :: Text -> (Text, [QueryPair])
parseQs url =
  let
    decoded = decodeUrl url
    path = T.takeWhile (\c -> c /= '?') decoded
    rest = T.dropWhile (\c -> c /= '?') decoded

    parsePair :: Text -> Maybe QueryPair
    parsePair s
      | T.any ((==) '=') s = Just (QueryPair a b)
      | otherwise = Nothing
      where a : b : _ = T.splitOn (T.pack "=") s
    
    get (Just p : rest) = p : get rest
    get (Nothing : rest) = get rest
    get [] = []

    pairs = 
      if T.null rest
      then []
      else get $ map parsePair $ T.splitOn (T.pack "&") $ T.tail rest

  in (path, pairs)

-- HTTP Parser

rev :: [a] -> [a]
rev = foldl (flip (:)) []

getMethod :: Text -> Method
getMethod s
  | s == (T.pack "POST") = POST
  | s == (T.pack "PUT")  = PUT
  | otherwise            = GET

parseFirstLine :: Text -> (Method, Text)
parseFirstLine l = (getMethod methodT, url)
  where [methodT, url, _] = T.words l

parseHeader :: Text -> Header
parseHeader line = 
  Header (T.takeWhile p line)
         (T.strip $ T.tail $ T.dropWhile p line)
    where p = (\c -> c /= ':')

parseHeaders :: [Text] -> [Header]
parseHeaders = map parseHeader

parseHttp :: Text -> ((Method, Text, [QueryPair]), [Header], [Text])
parseHttp text = 
  let
    lines = T.splitOn (T.pack "\r\n") text

    getFirstLine (l : rest) = (l, rest)

    getHeaders (l : rest) acc
      | T.null l  = (rev acc, rest)
      | otherwise = getHeaders rest (l : acc)
    
    (fl, rest1) = getFirstLine lines
    (headers, rest2) = getHeaders rest1 []

    (method, url) = parseFirstLine fl
    (path, query) = parseQs url
  in 
    ((method, path, query), parseHeaders headers, rest2)
