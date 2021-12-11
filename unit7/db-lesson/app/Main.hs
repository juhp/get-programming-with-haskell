{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (intercalate)
import Data.Maybe (listToMaybe, isJust)
import Data.Time (Day, UTCTime (utctDay), getCurrentTime)
import Database.SQLite.Simple
import System.Exit (exitSuccess)
import Data.Char (isDigit)

data Tool = Tool
  { toolId :: Int,
    name :: String,
    description :: String,
    lastReturned :: Day,
    timesLent :: Int
  }

data User = User Int String

instance FromRow Tool where
  fromRow =
    Tool <$> field
      <*> field
      <*> field
      <*> field
      <*> field

instance FromRow User where
  fromRow =
    User <$> field
      <*> field

showUser :: User -> String
showUser (User userid username) =
  mconcat
    [ show userid,
      ". ",
      username
    ]

showTool :: Tool -> String
showTool tool =
  mconcat
    [ "#",
      show $ toolId tool,
      " ",
      name tool,
      "\n description: ",
      description tool,
      "\n last returned: ",
      show $ lastReturned tool,
      "\n times lent: ",
      show $ timesLent tool
    ]

database :: FilePath
database = "tools.db"

addUser :: String -> IO ()
addUser user =
  withConnection database $ \conn -> do
    execute
      conn
      "INSERT INTO users (username) VALUES (?)"
      (Only user)
    putStrLn "user added"

checkout :: IO ()
checkout =
  withConnection database $ \conn -> do
    userid <- readInt "Enter user id:"
    userexists <- haveUser conn userid
    if userexists
      then do
      toolid <- readInt "Enter tool id:"
      toolexists <- haveTool conn toolid
      if toolexists
        then do
        available <- idQuery toolId availableQuery
        if toolid `elem` available
          then do
          execute conn
            "INSERT INTO checkedout (user_id,tool_id) VALUES (?,?)"
            (userid, toolid)
          putStrLn $ "checked out " ++ show toolid
          else putStrLn "not available"
        else putStrLn "unknown toolid"
      else putStrLn "unknown userid"

printUserQuery :: Query -> IO ()
printUserQuery q =
  withConnection database $ \conn -> do
    users <- query_ conn q
    mapM_ (putStrLn . showUser) users

printUsers :: IO ()
printUsers =
  printUserQuery "SELECT * FROM users;"

idQuery :: FromRow a => (a -> Int) -> Query -> IO [Int]
idQuery f = fmap (fmap f) . withConnection database . flip query_

availableQuery :: Query
availableQuery =
      mconcat
      [ "select * from tools ",
        "where id not in ",
        "(select tool_id from checkedout);"
      ]

toolQuery :: Query -> IO [Tool]
toolQuery = withConnection database . flip query_

printToolQuery :: Query -> IO ()
printToolQuery q = toolQuery q >>= mapM_ (putStrLn . showTool)

printTools :: IO ()
printTools =
  printToolQuery "SELECT * FROM tools;"

printAvailable :: IO ()
printAvailable = printToolQuery availableQuery

printCheckedout :: IO ()
printCheckedout =
  printToolQuery $
    mconcat
      [ "select * from tools ",
        "where id in ",
        "(select tool_id from checkedout);"
      ]

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolid = do
  listToMaybe <$>
    query
      conn
      "SELECT * FROM tools WHERE id = (?)"
      (Only toolid)

haveTool :: Connection -> Int -> IO Bool
haveTool conn toolid =
  isJust <$> selectTool conn toolid

selectUser :: Connection -> Int -> IO (Maybe User)
selectUser conn userid = do
  listToMaybe <$>
    query
      conn
      "SELECT * FROM users WHERE id = (?)"
      (Only userid)

haveUser :: Connection -> Int -> IO Bool
haveUser conn userid =
  isJust <$> selectUser conn userid

updateTool :: Tool -> Day -> Tool
updateTool tool date =
  Tool
    { toolId = toolId tool,
      name = name tool,
      description = description tool,
      lastReturned = date,
      timesLent = 1 + timesLent tool
    }

updateOrWarn :: Maybe Tool -> IO ()
updateOrWarn Nothing = putStrLn "id not found"
updateOrWarn (Just tool) =
  withConnection database $ \conn -> do
    let q =
          mconcat
            [ "UPDATE TOOLS SET lastReturned = ?,",
              " timesLent = ? WHERE ID = ?;"
            ]
    execute
      conn
      q
      ( lastReturned tool,
        timesLent tool,
        toolId tool
      )
    putStrLn "tool updated"

updateToolTable :: Int -> IO ()
updateToolTable toolid =
  withConnection database $ \conn -> do
    tool <- selectTool conn toolid
    currentDay <- utctDay <$> getCurrentTime
    let updatedTool =
          updateTool <$> tool
            <*> pure currentDay
    updateOrWarn updatedTool

checkin :: IO ()
checkin = do
  toolid <- readInt "Enter tool id:"
  withConnection database $ \conn -> do
    toolexists <- haveTool conn toolid
    if toolexists
      then do
      available <- idQuery toolId availableQuery
      if toolid `notElem` available
        then do
        execute conn
          "DELETE FROM checkedout WHERE tool_id = (?);"
          (Only toolid)
        updateToolTable toolid
        else putStrLn "already checked in"
      else putStrLn "unknown toolid"

readInt :: String -> IO Int
readInt prompt = do
  putStrLn prompt
  input <- getLine
  case words input of
    [ds] | all isDigit ds -> return $ read ds
    _ -> readInt prompt

promptAndAddUser :: IO ()
promptAndAddUser = do
  putStrLn "Enter new user name:"
  user <- getLine
  case words user of
    [] -> promptAndAddUser
    ns -> addUser $ unwords ns

commands :: [String]
commands =
  ["users", "tools", "adduser", "checkout", "checkin", "in", "out", "quit"]

main :: IO ()
main = do
  putStrLn $ mconcat
    ["\n",
     "Enter a command (",
     intercalate ", " commands,
     "):"
    ]
  input <- getLine
  case words input of
    [] -> return ()
    [c] ->
      if c `elem` commands
        then performCommand c
        else putStrLn "unknown command"
    _ -> putStrLn "enter single command"
  main
  where
    performCommand :: String -> IO ()
    performCommand "users" = printUsers
    performCommand "tools" = printTools
    performCommand "adduser" = promptAndAddUser
    performCommand "checkout" = checkout
    performCommand "checkin" = checkin
    performCommand "in" = printAvailable
    performCommand "out" = printCheckedout
    performCommand "quit" = putStrLn "bye!" >> exitSuccess
    performCommand _ = putStrLn "unknown command"
