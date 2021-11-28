import Test.QuickCheck
import Primes
import Data.Maybe

prop_validPrimesOnly :: Int -> Bool
prop_validPrimesOnly val =
  if val < 2 || val >= last primes
  then result == Nothing
  else isJust result
  where
    result = isPrime val

prop_isPrime :: Int -> Bool
prop_isPrime val =
  case isPrime val of
    Just isprime ->
      if isprime
      then length divisors == 0
      else length divisors > 0
    Nothing -> True
  where
    divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_factorsMakeOriginal :: Int -> Bool
prop_factorsMakeOriginal val =
  if result == Nothing
  then True
  else product (fromJust result) == val
  where
    result = primeFactors val

prop_allFactorsPrime :: Int -> Bool
prop_allFactorsPrime val =
  if result == Nothing
  then True
  else all (== Just True) resultsPrime
  where
    result = primeFactors val
    resultsPrime = map isPrime (fromJust result)

main :: IO ()
main = do
  quickCheck prop_validPrimesOnly
  quickCheckWith stdArgs { maxSuccess = 1000} prop_isPrime
  quickCheck prop_factorsMakeOriginal
  quickCheck prop_allFactorsPrime
