{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( addUser
    , addTool
    , checkout
    , printUsers
    , printTools
    , printAvailable
    , printCheckedout
    , checkinAndUpdate
    )
    where
    
import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.FromRow
import Data.Time
import qualified Data.Text as T

base = "tools.db"

data User = User
    { userId :: Int
    , userName :: String
    }

instance Show User where
    show user = mconcat [ show $ userId user
                        , ") "
                        , userName user
                        ]

instance FromRow User where
    fromRow = User <$> field
                   <*> field

instance ToRow User where
    toRow user = [ SQLInteger . fromIntegral $ userId user
                 , SQLText . T.pack $ userName user
                 ]

data Tool = Tool
    { toolId :: Int
    , name :: String
    , description :: String
    , lastReturned :: Day
    , timesBorrowed :: Int
    }

instance Show Tool where
    show tool = mconcat [ show $ toolId tool
                        , ") "
                        , name tool
                        , "\n description: "
                        , description tool
                        , "\n last returned: "
                        , show $ lastReturned tool
                        , "\n times borrowed: "
                        , show $ timesBorrowed tool
                        , "\n"
                        ]

instance FromRow Tool where
    fromRow = Tool <$> field
                   <*> field
                   <*> field
                   <*> (read <$> field)
                   <*> field

withConn :: (Connection -> IO ()) -> IO ()
withConn action = do
    conn <- open base
    action conn
    close conn

addUser :: String -> IO ()
addUser userName = withConn $
    \conn -> do
        execute conn "insert into users (username) values (?)"
            (Only userName)
        putStrLn $ mconcat [ "user \"", userName, "\" added" ]

addTool :: String -> String -> IO ()
addTool toolName description = withConn $
    \conn -> do
        day <- show . utctDay <$> getCurrentTime
        execute conn "insert into tools (name,description,lastreturned,timesborrowed) values (?,?,?,?)"
            (toolName, description, day, 0 :: Int)
        putStrLn $ mconcat [ "tool \"", toolName, "\" added" ]

checkout :: Int -> Int -> IO ()
checkout userId toolId = withConn $
    \conn -> do
        execute conn "insert into checkedout (user_id,tool_id) values (?,?)"
            (userId,toolId)

data QT = QUser | QTool

printQuery :: QT -> Query -> IO ()
printQuery QUser q = withConn $
    \conn -> do
        resp <- query_ conn q :: IO [User]
        mapM_ print resp
printQuery QTool q = withConn $
    \conn -> do
        resp <- query_ conn q :: IO [Tool]
        mapM_ print resp

printUsers :: IO ()
printUsers = printQuery QUser "select * from users;"

printTools :: IO ()
printTools = printQuery QTool "select * from tools;"

printAvailable :: IO ()
printAvailable = printQuery QTool $ mconcat
    [ "select * from tools "
    , "where id not in "
    , "(select tool_id from checkedout);"
    ]

printCheckedout :: IO ()
printCheckedout = printQuery QTool $ mconcat
    [ "select * from tools "
    , "where id in "
    , "(select tool_id from checkedout);"
    ]

firstOrNothing :: [a] -> Maybe a
firstOrNothing [] = Nothing
firstOrNothing (x:_) = Just x

selectTool :: Connection -> Int -> IO (Maybe Tool)
selectTool conn toolId = do
    resp <- query conn "select * from tools where id = (?);"
        (Only toolId)
    return $ firstOrNothing resp

selectCheckedoutTool :: Connection -> Int -> IO (Bool)
selectCheckedoutTool conn toolId = do
        exist <- (query conn "select tool_id from checkedout where tool_id = (?);"
            (Only toolId) :: IO [(Only Int)])
        return $ length exist > 0

updateTool :: Tool -> Day -> Tool
updateTool tool date = tool
    { lastReturned = date
    , timesBorrowed = 1 + timesBorrowed tool
    }

updateOrWarn :: Maybe Tool -> Int -> IO ()
updateOrWarn Nothing toolId = print $ mconcat [ "Tool id = ", show toolId, " not present" ]
updateOrWarn (Just tool) _ = withConn $
    \conn -> do
        let q = "update tools set lastReturned = ?, timesBorrowed = ? \
                \where id = ?;"
        execute conn q ( lastReturned tool
                       , timesBorrowed tool
                       , toolId tool
                       )
        print "tool updated"

updateToolTable :: Int -> IO ()
updateToolTable toolId = withConn $
    \conn -> do
        tool <- selectTool conn toolId
        currentDay <- utctDay <$> getCurrentTime
        let updatedTool = updateTool <$> tool
                                     <*> pure currentDay
        updateOrWarn updatedTool toolId

checkin :: Int -> IO ()
checkin toolId = withConn $
    \conn -> do
        execute conn "delete from checkedout where tool_id = (?);"
            (Only toolId)

checkinAndUpdate :: Int -> IO ()
checkinAndUpdate toolId = withConn $
    \conn -> do
        exist <- selectCheckedoutTool conn toolId
        if exist
        then do
            checkin toolId
            updateToolTable toolId
        else print $ mconcat [ "No tool checked out for toolId = ", show toolId ]
