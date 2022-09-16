{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log


-- Convert the error into a MessageType
getMessageType :: String -> Int -> MessageType
getMessageType messageType num
    | messageType == "I" = Info
    | messageType == "W" = Warning
    | otherwise = Error num

-- For errors, the error number must be removed
extractMessage :: String -> [String] -> (String, Int, [String])
extractMessage "E" message = ("E", read (head message) :: Int , tail message)
extractMessage errorType message = (errorType, 0, message)


parseMessage :: String -> LogMessage
parseMessage infoString = LogMessage messageType timeStamp messageString
    where infoStringList = words infoString

          -- Extract the error number when an error occurs
          (messageTypeString, messageNum, messageList) =
            extractMessage (head infoStringList) (tail infoStringList)

          -- Extract the type of message
          messageType = getMessageType messageTypeString messageNum

          -- Extract timestamp
          timeStamp = read (head messageList) :: Int

          -- Extract message
          messageString = unwords $ tail messageList
