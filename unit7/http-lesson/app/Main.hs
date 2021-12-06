module Main where

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import Network.HTTP.Simple


myToken :: B.ByteString
myToken = "<token here>"

noaaHost :: B.ByteString
noaaHost = "www.ncdc.noaa.gov"

apiPath :: B.ByteString
apiPath = "/cdo-web/api/v2/datasets"

buildRequest :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
             -> Request
buildRequest token host method path  =
  setRequestMethod method $
  setRequestHost host $
  setRequestPath path $
  setRequestHeader "token" [token] $
  setRequestSecure True $
  setRequestPort 443
  defaultRequest

request :: Request
request = buildRequest myToken noaaHost "GET" apiPath

main :: IO ()
main = do
  response <- httpLBS request
  let status = getResponseStatusCode response
  if status == 200
    then do
         putStrLn "saving request to file"
         let jsonBody = getResponseBody response
         L.writeFile "data.json" jsonBody
    else L.putStrLn $ "request failed with\n" <> getResponseBody response
