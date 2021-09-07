data Cup = Cup ((Int -> Int) -> Int)

cup :: Int -> Cup
cup oz = Cup (\f -> f oz)

update :: (Int -> Int) -> Cup -> Cup
update f (Cup self) = Cup (\g -> g (self f))

small, large :: Cup
small = cup 6
large = cup 10

getOz :: Cup -> Int
getOz (Cup self) = self id

isEmpty :: Cup -> Bool
isEmpty c = getOz c <= 0

gulp :: Cup -> Cup
gulp c = if isEmpty c
         then c
         else update pred c
  where oz = getOz c

afterGulps :: Int -> Cup -> Cup
afterGulps n self = head $ drop n $ iterate gulp self

drinkSmallCup =
  getOz $ afterGulps 6 small
