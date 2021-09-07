data Bot = Bot {name :: String,
                attack :: Int,
                hit :: Int}

data Robot = Robot ((Bot -> Bot) -> Bot)

robot :: Bot -> Robot
robot st = Robot (\f -> f st)

killerRobot :: Robot
killerRobot = robot (Bot "Kill3r" 25 200)

get :: (Bot -> a) -> Robot -> a
get f (Robot self) = f $ self id

update :: (Bot -> Bot) -> Robot -> Robot
update f (Robot self) = Robot (\g -> g (self f))

printRobot :: Robot -> IO ()
printRobot bot =
  let (Bot n a h) = get id bot
  in putStrLn $ n ++ " attack:" ++ (show a) ++ " hp:"++ (show h)

damage :: Robot -> Int -> Robot
damage bot attackDamage =
  update (\(Bot n a h) -> (Bot n a (h-attackDamage))) bot

fight :: Robot -> Robot -> Robot
fight aRobot defender = damage defender attackHit
  where
    attackHit = if (get hit aRobot > 10)
             then get attack aRobot
             else 0

gentleGiant = robot (Bot "Mr. Friendly" 10 300)

battle1, battle2 :: IO ()

battle1 = do
  printRobot gentleGiantRound3
  printRobot killerRobotRound3
  where
    gentleGiantRound1 = fight killerRobot gentleGiant
    killerRobotRound1 = fight gentleGiant killerRobot
    gentleGiantRound2 = fight killerRobotRound1 gentleGiantRound1
    killerRobotRound2 = fight gentleGiantRound1 killerRobotRound1
    gentleGiantRound3 = fight killerRobotRound2 gentleGiantRound2
    killerRobotRound3 = fight gentleGiantRound2 killerRobotRound2

fastRobot = robot (Bot "speedy" 15 40)
slowRobot = robot (Bot "slowpoke" 20 30)

battle2 = do
  printRobot slowRobotRound3
  printRobot fastRobotRound3
  where
    fastRobotRound3 = fight slowRobotRound3 fastRobotRound2
    fastRobotRound2 = fight slowRobotRound2 fastRobotRound1
    fastRobotRound1 = fight slowRobotRound1 fastRobot
    slowRobotRound3 = fight fastRobotRound2 slowRobotRound2
    slowRobotRound2 = fight fastRobotRound1 slowRobotRound1
    slowRobotRound1 = fight fastRobot slowRobot
