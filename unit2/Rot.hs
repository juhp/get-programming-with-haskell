module Rot where

import Cipher

rotNencode :: (Bounded a, Enum a) => Int -> a -> a
rotNencode size c = toEnum rotation
 where
   halfN = size `div` 2
   offset = (fromEnum c) + halfN
   rotation =  offset `mod` size

rotNdecode :: (Bounded a, Enum a) => Int -> a -> a
rotNdecode size c = toEnum rotation
 where
   halfN = size `div` 2
   offset = if even size
            then (fromEnum c) + halfN
            else 1 + (fromEnum c) + halfN
   rotation =  offset `mod` size
