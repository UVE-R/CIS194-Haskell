{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- Convert a log line list to a LogMessage
tryParseMessage :: [String] -> LogMessage
-- With errors, the error number must also be extracted
tryParseMessage ("E":num:stamp:message) = LogMessage (Error (read num)) (read stamp) (unwords message)
tryParseMessage ("I":stamp:message) = LogMessage Info (read stamp) (unwords message)
tryParseMessage ("W":stamp:message) = LogMessage Warning (read stamp) (unwords message)
tryParseMessage text = Unknown (unwords text)

-- Convert a single log line to a LogMessage
parseMessage :: String -> LogMessage
parseMessage logLine = tryParseMessage $ words logLine

-- Convert a list of log lines to a list of LogMessages
parse :: String -> [LogMessage]
parse logLines = map parseMessage (lines logLines)

-- Return the TimeStamp of a LogMessage
getTime :: LogMessage -> Int
getTime (LogMessage _ timeStamp _ ) = timeStamp
getTime _ = 0

-- Return the message of a LogMessage
getMessage :: LogMessage -> String
getMessage (LogMessage _ _ message ) = message
getMessage _ = ""

-- Return the error number of a LogMessage
getError :: LogMessage -> Int
getError (LogMessage (Error n) _ _ ) = n
getError _  = 0

-- Insert a LogMessage into the binary tree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMsg Leaf = Node Leaf logMsg Leaf
insert logMsg (Node left nodeMsg right)
    | getTime logMsg < getTime nodeMsg = Node (insert logMsg left) nodeMsg right
    | getTime logMsg > getTime nodeMsg = Node left nodeMsg (insert logMsg right)
    | otherwise = Node left nodeMsg right

-- Create a MessageTree from the logs
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Inorder traversal of the tree
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left nodeMsg right) = inOrder left ++ nodeMsg : inOrder right

-- Return a list of error messages with severity greater than 50
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong (x:xs)
    | getError x >= 50 = getMessage x : whatWentWrong xs
    | otherwise = whatWentWrong xs


-- It was the mad hatter!
main :: IO ()
main = do
    s <- readFile "error.log"
    let log1 = parse s
    let log2 = inOrder (build log1)
    mapM_ print (whatWentWrong log2)
    