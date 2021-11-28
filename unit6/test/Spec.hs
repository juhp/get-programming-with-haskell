import Test.QuickCheck
import Primes
import Data.Maybe

prop_validPrimesOnly :: Int -> Bool
prop_validPrimesOnly val =
  if val < 2 || val >= last primes
  then isNothing result
  else isJust result
  where
    result = isPrime val

prop_isPrime :: Int -> Bool
prop_isPrime val =
  case isPrime val of
    Just isprime ->
      isprime == null divisors
    Nothing -> True
  where
    divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_factorsMakeOriginal :: Int -> Bool
prop_factorsMakeOriginal val =
  maybe True ((== val) . product) $ primeFactors val

prop_allFactorsPrime :: Int -> Bool
prop_allFactorsPrime val =
  maybe True (all (== Just True) . map isPrime) $ primeFactors val

main :: IO ()
main = do
  quickCheck prop_validPrimesOnly
  quickCheckWith stdArgs { maxSuccess = 1000} prop_isPrime
  quickCheck prop_factorsMakeOriginal
  quickCheck prop_allFactorsPrime
