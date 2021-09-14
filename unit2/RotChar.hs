module RotChar where

import Cipher
import Rot

rotCharEncode :: Char -> Char
rotCharEncode = rotNencode size
  where
    size = 1 + (fromEnum (maxBound :: Char))

rotCharDecode :: Char -> Char
rotCharDecode = rotNdecode size
  where
    size = 1 + (fromEnum (maxBound :: Char))

rotEncoder :: String -> String
rotEncoder = map rotCharEncode

rotDecoder :: String -> String
rotDecoder = map rotCharDecode

data Rot = Rot

instance Cipher Rot where
   encode Rot text = rotEncoder text
   decode Rot text = rotDecoder text
