import Rot

data ThreeLetterAlpha = Alpha | Beta | Kappa
  deriving (Show,Enum,Bounded)

threeLetterMessage :: [ThreeLetterAlpha]
threeLetterMessage = [Alpha,Alpha,Beta,Alpha,Kappa]

rot3lEncode :: ThreeLetterAlpha -> ThreeLetterAlpha
rot3lEncode = rotNencode size
  where
    size = 1 + (fromEnum ( maxBound :: ThreeLetterAlpha))

rot3lDecode :: ThreeLetterAlpha -> ThreeLetterAlpha
rot3lDecode = rotNdecode size
  where
    size = 1 + (fromEnum ( maxBound :: ThreeLetterAlpha))

threeLetterEncoder :: [ThreeLetterAlpha] -> [ThreeLetterAlpha]
threeLetterEncoder =  map rot3lEncode

threeLetterDecoder :: [ThreeLetterAlpha] -> [ThreeLetterAlpha]
threeLetterDecoder =  map rot3lDecode
