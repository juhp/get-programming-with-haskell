module Main where

import Control.Exception (bracket)
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.Time (getCurrentTime, Day, UTCTime(utctDay))
import Database.SQLite.Simple
import System.Exit (exitSuccess)

data Tool = Tool
  { toolId :: Int
  , name :: String
  , description :: String
  , lastReturned :: Day
  , timesLent :: Int
  }

data User = User
  { userId :: Int
  , userName :: String
  }

instance Show User where
  show user = mconcat [ show $ userId user
                      , ". "
                      , userName user]

instance Show Tool where
  show tool = mconcat [ "#"
                      , show $ toolId tool
                      , " "
                      , name tool
                      , "\n description: "
                      , description tool
                      , "\n last returned: "
                      , show $ lastReturned tool
                      , "\n times lent: "
                      , show $ timesLent tool
                      ]

withDB :: (Connection -> IO ()) -> IO ()
withDB = bracket (open "tools.db") close

addUser :: String -> IO ()
addUser user =
  withDB $ \conn -> do
  execute conn
    "INSERT INTO users (username) VALUES (?)"
    (Only user)
  putStrLn "user added"

checkout :: Int -> Int -> IO ()
checkout userid toolid =
  withDB $ \conn -> do
  execute conn
    "INSERT INTO checkedout (user_id,tool_id) VALUES (?,?)"
    (userid,toolid)

instance FromRow User where
  fromRow = User <$> field
                 <*> field

instance FromRow Tool where
  fromRow = Tool <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

printUsers :: IO ()
printUsers =
  withDB $ \conn -> do
  resp <- query_ conn "SELECT * FROM users;" :: IO [User]
  mapM_ print resp

printToolQuery :: Query -> IO ()
printToolQuery q =
  withDB $ \conn -> do
  resp <- query_ conn q :: IO [Tool]
  mapM_ print resp

printTools :: IO ()
printTools =
  printToolQuery "SELECT * FROM tools;"

printAvailable :: IO ()
printAvailable =
  printToolQuery $
  mconcat ["select * from tools "
          ,"where id not in "
          ,"(select tool_id from checkedout);"]

printCheckedout :: IO ()
printCheckedout =
  printToolQuery $
  mconcat ["select * from tools "
          ,"where id in "
          ,"(select tool_id from checkedout);"]

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolid = do
   resp <- query conn
           "SELECT * FROM tools WHERE id = (?)"
           (Only toolid) :: IO [Tool]
   return $ listToMaybe resp

updateTool :: Tool -> Day -> Tool
updateTool tool date = Tool
   { toolId = toolId tool
   , name = name tool
   , description = description tool
   , lastReturned = date
   , timesLent = 1 + timesLent tool
   }

updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = putStrLn "id not found"
updateOrWarn (Just tool) =
  withDB $ \conn -> do
  let q = mconcat ["UPDATE TOOLS SET lastReturned = ?,"
                  ," timesLent = ? WHERE ID = ?;"]
  execute conn q (lastReturned tool
                 , timesLent tool
                 , toolId tool)
  putStrLn "tool updated"

updateToolTable :: Int -> IO ()
updateToolTable toolid =
  withDB $ \conn -> do
  tool <- selectTool conn toolid
  currentDay <- utctDay <$> getCurrentTime
  let updatedTool = updateTool <$> tool
                    <*> pure currentDay
  updateOrWarn updatedTool

checkin :: Int -> IO ()
checkin toolid =
  withDB $ \conn -> do
  execute conn
    "DELETE FROM checkedout WHERE tool_id = (?);"
    (Only toolid)

checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolid = do
   checkin toolid
   updateToolTable toolid

promptAndCheckout :: IO ()
promptAndCheckout = do
   putStrLn "Enter the id of the user"
   userid <- read <$> getLine
   putStrLn "Enter the id of the tool"
   toolid <- read <$> getLine
   checkout userid toolid

promptAndCheckin :: IO ()
promptAndCheckin = do
   putStrLn "enter the id of tool"
   toolid <- read <$> getLine
   checkinAndUpdate toolid

promptAndAddUser :: IO ()
promptAndAddUser = do
   putStrLn "Enter new user name"
   user <- getLine

   addUser user

commands :: [String]
commands =
  ["users", "tools", "adduser", "checkout", "checkin", "in", "out", "quit"]

main :: IO ()
main = do
  putStrLn $ "\nEnter a command (" ++ intercalate ", " commands ++ "):"
  input <- getLine
  case words input of
    [] -> return ()
    [c] -> if c `elem` commands
           then performCommand c
           else putStrLn "unknown command"
    _ -> putStrLn "enter single command"
  main
  where
    performCommand :: String -> IO ()
    performCommand "users" = printUsers
    performCommand "tools" = printTools
    performCommand "adduser" = promptAndAddUser
    performCommand "checkout" = promptAndCheckout
    performCommand "checkin" = promptAndCheckin
    performCommand "in" = printAvailable
    performCommand "out" = printCheckedout
    performCommand "quit" = putStrLn "bye!" >> exitSuccess
    performCommand _ = putStrLn "unknown command"
