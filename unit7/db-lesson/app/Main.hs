{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Main where

import Control.Monad (forM_)
import Data.Char (isDigit)
import Data.List (intercalate)
import Data.Maybe (isJust, listToMaybe)
import Data.Time (Day, UTCTime (utctDay), getCurrentTime)
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import System.Exit (exitSuccess)

data Tool = Tool
  { toolId :: Int,
    name :: String,
    description :: String,
    lastReturned :: Day,
    timesReturned :: Int
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
      "\n times returned: ",
      show $ timesReturned tool
    ]

commands :: [String]
commands =
  ["users", "tools",
   "adduser", "addtool",
   "lend", "return",
   "in", "out",
   "quit"]

dbfile :: FilePath
dbfile = "db/tools.db"

main :: IO ()
main = do
  withConnection dbfile $ \ db ->
    mapM_ (execute_ db) dbTables
  loop

loop :: IO ()
loop = do
  putStrLn $
    mconcat
      [ "\nEnter a command (",
        intercalate ", " commands,
        "):"
      ]
  input <- getLine
  case words input of
    [] -> return ()
    [c] ->
      if c `elem` commands
        then withConnection dbfile $ performCommand c
        else putStrLn "unknown command"
    _ -> putStrLn "enter single command"
  loop
  where
    performCommand :: String -> (Connection -> IO ())
    performCommand "users" = printUsers
    performCommand "tools" = printTools
    performCommand "adduser" = addUser
    performCommand "addtool" = addTool
    performCommand "lend" = checkout
    performCommand "return" = checkin
    performCommand "in" = printAvailable
    performCommand "out" = printCheckedout
    performCommand "quit" = const (putStrLn "bye!" >> exitSuccess)
    performCommand _ = const (putStrLn "unknown command")

printUsers :: Connection -> IO ()
printUsers db =
  query_ db
  [sql|SELECT * FROM users|]
    >>= mapM_ (putStrLn . showUser)

printTools :: Connection -> IO ()
printTools db =
  printToolQuery db [sql|SELECT * FROM tools|]

addUser :: Connection -> IO ()
addUser db = do
  putStrLn "Enter new user name:"
  user <- getLine
  case words user of
    [] -> addUser db
    ns -> do
      execute
        db
        [sql|INSERT INTO users (username) VALUES (?)|]
        (Only (unwords ns))
      putStrLn "user added"

addTool :: Connection -> IO ()
addTool db = do
  tool <- getString "Enter new tool name:"
  desc <- getString "Enter new tool description:"
  execute
    db
    [sql|
        INSERT INTO tools
        (name,description,lastReturned,timesReturned)
        VALUES (?,?,?,?)
        |]
    (tool,desc,SQLText "2017-01-01", SQLInteger 0)
  putStrLn "tool added"

checkout :: Connection -> IO ()
checkout db = do
  userid <- readInt "Enter user id:"
  userexists <- isJust <$> getUser db userid
  if userexists
    then do
      toolid <- readInt "Enter tool id:"
      toolexists <- haveTool db toolid
      if toolexists
        then do
          available <- getIDs db availableQuery toolId
          if toolid `elem` available
            then do
              execute
                db
                [sql|INSERT INTO checkedout (user_id,tool_id) VALUES (?,?)|]
                (userid, toolid)
              putStrLn $ "checked out #" ++ show toolid
            else putStrLn "not available"
        else putStrLn "unknown toolid"
    else putStrLn "unknown userid"

checkin :: Connection -> IO ()
checkin db = do
  toolid <- readInt "Enter tool id:"
  toolexists <- haveTool db toolid
  if toolexists
    then do
      available <- getIDs db availableQuery toolId
      if toolid `notElem` available
        then do
          execute
            db
            [sql|DELETE FROM checkedout WHERE tool_id = (?)|]
            (Only toolid)
          updateToolTable db toolid
        else putStrLn "already checked in"
    else putStrLn "unknown toolid"

printAvailable :: Connection -> IO ()
printAvailable db = printToolQuery db availableQuery

printCheckedout :: Connection -> IO ()
printCheckedout db = do
  out <-
    query_ db $
      [sql|
        SELECT * FROM tools
        WHERE id IN
        (SELECT tool_id FROM checkedout)
        |]
  forM_ out $ \tool -> do
    (putStrLn . showTool) tool
    borrowers <-
      query
        db
        [sql|SELECT user_id FROM checkedout WHERE tool_id = (?)|]
        (Only (toolId tool)) ::
        IO [Only Int]
    mapM_ (putStrLn . (" borrowed by: " ++) . show . fromOnly) borrowers

----

getUser :: Connection -> Int -> IO (Maybe User)
getUser db userid = do
  listToMaybe
    <$> query
      db
      [sql|SELECT * FROM users WHERE id = (?)|]
      (Only userid)

getIDs :: FromRow a => Connection -> Query -> (a -> Int) -> IO [Int]
getIDs db q f = map f <$> query_ db q

availableQuery :: Query
availableQuery =
  [sql|
      select * from tools
      where id not in
      (select tool_id from checkedout)
      |]

printToolQuery :: Connection -> Query -> IO ()
printToolQuery db q = query_ db q >>= mapM_ (putStrLn . showTool)

getTool :: Connection -> Int -> IO (Maybe Tool)
getTool db toolid = do
  listToMaybe
    <$> query
      db
      [sql|SELECT * FROM tools WHERE id = (?)|]
      (Only toolid)

haveTool :: Connection -> Int -> IO Bool
haveTool db toolid =
  isJust <$> getTool db toolid

updateToolTable :: Connection -> Int -> IO ()
updateToolTable db toolid = do
  mtool <- getTool db toolid
  currentDay <- utctDay <$> getCurrentTime
  case mtool of
    Nothing -> putStrLn "id not found"
    Just tool -> update $ updatedTool currentDay tool
  where
    updatedTool :: Day -> Tool -> Tool
    updatedTool date tool =
      tool
        { lastReturned = date,
          timesReturned = timesReturned tool + 1
        }

    update :: Tool -> IO ()
    update tool = do
      execute
        db
        [sql|
            UPDATE tools SET
            lastReturned = ?, timesReturned = ?
            WHERE ID = ?
            |]
        ( lastReturned tool,
          timesReturned tool,
          toolId tool
        )
      putStrLn "tool updated"

readInt :: String -> IO Int
readInt prompt = do
  putStrLn prompt
  input <- getLine
  case words input of
    [ds] | all isDigit ds -> return $ read ds
    _ -> readInt prompt

getString :: String -> IO String
getString prompt = do
  putStrLn prompt
  input <- getLine
  case words input of
    [] -> getString prompt
    ws -> return $ unwords ws

dbTables :: [Query]
dbTables =
  [
    [sql|
        CREATE TABLE IF NOT EXISTS users (
        id INTEGER PRIMARY KEY,
        username TEXT
        )|],
    [sql|
        CREATE TABLE tools (
        id INTEGER PRIMARY KEY,
        name TEXT,
        description TEXT,
        lastReturned TEXT,
        timesReturned INTEGER
        )|],
    [sql|
        CREATE TABLE checkedout (
        user_id INTEGER,
        tool_id INTEGER
        )|]
  ]
