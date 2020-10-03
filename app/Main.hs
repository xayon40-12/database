module Main where

import Lib

promptAndAddUser :: IO ()
promptAndAddUser = do
    print "Enter new user name"
    userName <- getLine
    addUser userName

promptAndCheckout :: IO ()
promptAndCheckout = do
    print "Enter the id of the user"
    userId <- read <$> getLine
    print "Enter the id of the tool"
    toolId <- read <$> getLine
    checkout userId toolId

promptAndAddTool :: IO ()
promptAndAddTool = do
    print "Enter new tool name"
    toolName <- getLine
    print "Enter tool description (one line)"
    toolDesc <- getLine
    addTool toolName toolDesc

promptAndCheckin :: IO ()
promptAndCheckin = do
    print "Enter the id of the tool"
    toolId <- read <$> getLine
    checkinAndUpdate toolId

performCommand :: String -> IO ()
performCommand c | c == "users" = printUsers >> main
                 | c == "tools" = printTools >> main
                 | c == "adduser" = promptAndAddUser >> main
                 | c == "checkout" = promptAndCheckout >> main
                 | c == "checkin" = promptAndCheckin >> main
                 | c == "in" = printAvailable >> main
                 | c == "out" = printCheckedout >> main
                 | c == "quit" = print "bye!"
                 | otherwise = (putStrLn $ unlines
                        [ "Sorry command not found, available:"
                        , "\t- users"
                        , "\t- tools"
                        , "\t- adduser"
                        , "\t- checkout"
                        , "\t- checkin"
                        , "\t- in"
                        , "\t- out"
                        , "\t- quit"
                        ]) >> main
main :: IO ()
main = do
    print "Enter a command"
    command <- getLine
    performCommand command
