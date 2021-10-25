module Common where

import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

rawToInt :: ByteString -> Int
rawToInt = read . T.unpack . E.decodeUtf8
