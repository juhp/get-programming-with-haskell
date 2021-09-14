import Rot

data FourLetterAlpha = L1 | L2 | L3 | L4
  deriving (Show,Enum,Bounded)

fourLetterMessage :: [FourLetterAlpha]
fourLetterMessage = [L1,L3,L4,L1,L1,L2]

rot4l :: FourLetterAlpha -> FourLetterAlpha
rot4l = rotNencode 4
