module Web.Utils (parseQs, parseHttp, getStatus) where

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

-- HTTP Status Codes
statusCodes = 
  [ (100, "Continue"),
    (101, "Switching Protocols"),
    (102, "Processing"),
    (200, "OK"),
    (201, "Created"),
    (202, "Accepted"),
    (203, "Non Authoritative Information"),
    (204, "No Content"),
    (205, "Reset Content"),
    (206, "Partial Content"),
    (207, "Multi Status"),
    (226, "IM Used"),
    (300, "Multiple Choices"),
    (301, "Moved Permanently"),
    (302, "Found"),
    (303, "See Other"),
    (304, "Not Modified"),
    (305, "Use Proxy"),
    (307, "Temporary Redirect"),
    (308, "Permanent Redirect"),
    (400, "Bad Request"),
    (401, "Unauthorized"),
    (402, "Payment Required"),
    (403, "Forbidden"),
    (404, "Not Found"),
    (405, "Method Not Allowed"),
    (406, "Not Acceptable"),
    (407, "Proxy Authentication Required"),
    (408, "Request Timeout"),
    (409, "Conflict"),
    (410, "Gone"),
    (411, "Length Required"),
    (412, "Precondition Failed"),
    (413, "Request Entity Too Large"),
    (414, "Request URI Too Long"),
    (415, "Unsupported Media Type"),
    (416, "Requested Range Not Satisfiable"),
    (417, "Expectation Failed"),
    (418, "I'm a teapot"),
    (421, "Misdirected Request"),
    (422, "Unprocessable Entity"),
    (423, "Locked"),
    (424, "Failed Dependency"),
    (426, "Upgrade Required"),
    (428, "Precondition Required"),
    (429, "Too Many Requests"),
    (431, "Request Header Fields Too Large"),
    (449, "Retry With"),
    (451, "Unavailable For Legal Reasons"),
    (500, "Internal Server Error"),
    (501, "Not Implemented"),
    (502, "Bad Gateway"),
    (503, "Service Unavailable"),
    (504, "Gateway Timeout"),
    (505, "HTTP Version Not Supported"),
    (507, "Insufficient Storage"),
    (510, "Not Extended") ]

getStatus :: Int -> String
getStatus code = findStatus statusCodes
  where
    findStatus ((codeCheck, stat) : rest)
      | codeCheck == code = show code ++ " " ++ stat
      | otherwise         = findStatus rest
    findStatus [] = "500 Internal Server Error"
