module Bits where

import Data.Word

type Bits = [Bool]

wordToBits :: Word -> Bits
wordToBits n = leadingFalses ++ reversedBits
  where
    reversedBits = reverse  (intToBits' n)
    missingBits = maxBits - (length reversedBits)
    leadingFalses = take missingBits (cycle [False])

    maxBits :: Int
    maxBits = length (intToBits' maxBound)

    intToBits' :: Word -> Bits
    intToBits' 0 = [False]
    intToBits' 1 = [True]
    intToBits' n = if (remainder == 0)
                   then False : (intToBits' nextVal)
                   else True : (intToBits' nextVal)
     where
       remainder = n `mod` 2
       nextVal = n `div` 2

charToBits :: Char -> Bits
charToBits char = wordToBits (toEnum $ fromEnum char)

bitsToWord :: Bits -> Word
bitsToWord bits = sum (map (\x -> 2^(snd x)) trueLocations)
  where
    size = (length bits)
    indices = [size-1,size-2 .. 0]
    trueLocations = filter (\x -> (fst x == True)) (zip bits indices)

bitsToChar :: Bits -> Char
bitsToChar = toEnum . fromEnum . bitsToWord
