data Cup = Cup ((Int -> Int) -> Int)

cup :: Int -> Cup
cup oz = Cup (\f -> f oz)

small, large :: Cup
small = cup 6
large = cup 10

getOz :: Cup -> Int
getOz (Cup aCup) = aCup id

gulp :: Cup -> Cup
gulp (Cup aCup) = if oz >= 1
                  then cup (oz - 1)
                  else cup 0
  where oz = getOz (Cup aCup)

afterGulps :: Int -> Cup -> Cup
afterGulps n aCup = head $ drop n $ iterate gulp aCup

drinkSmallCup =
  getOz $ afterGulps 6 small
