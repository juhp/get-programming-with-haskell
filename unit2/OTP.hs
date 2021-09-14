module OTP where

import Bits
import Cipher

myPad :: String
myPad = "Shhhhhh"

myPlainText :: String
myPlainText = "Haskell"

applyOTP' :: String -> String -> [Bits]
applyOTP' pad plaintext =  map (\pair ->
                                 (fst pair) `xor` (snd pair))
                           (zip padBits plaintextBits)
 where padBits =  map charToBits pad
       plaintextBits =  map charToBits plaintext

applyOTP :: String -> String -> String
applyOTP pad plaintext = map bitsToChar bitList
 where bitList = applyOTP' pad plaintext

encoderDecoder :: String -> String
encoderDecoder = applyOTP myPad

data XOROneTimePad = XOROTP String

instance Cipher XOROneTimePad where
   encode (XOROTP pad) text = applyOTP pad text
   decode (XOROTP pad) text = applyOTP pad text

myOTP :: XOROneTimePad
myOTP = XOROTP (cycle [minBound .. maxBound])

xor :: [Bool] -> [Bool] -> [Bool]
xor list1 list2 = zipWith xor' list1 list2
  where
    xor' :: Bool -> Bool -> Bool
    xor' value1 value2 = (value1 || value2) && (not (value1 && value2))
