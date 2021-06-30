module Main where

-- Network
import Network.Socket
import Network.Run.TCP (runTCPClient)
import Network.Socket.ByteString.Lazy (sendAll)

-- ByteString
import qualified Data.ByteString.Lazy as L
-- For encoding String to [Word8]
import Data.String.UTF8 (encode)


-- FilePath
import System.FilePath.Posix (takeFileName)

hostName :: HostName
hostName = "127.0.0.1"

port :: ServiceName
port = "3000"

headerSize :: Int -- Size of Header in Bytes
headerSize = 3

main :: IO ()
main = runTCPClient hostName port $ \s -> do
   putStrLn "Oh, i'll try to be cute that time. You could even use nasty '\\' for paths, i'll not die with error, promise."
   putStrLn "Btw, good for you not forgeting execute Receiver.exe first."
   putStrLn "So.. Enter some nicie file location of file you want me to send, pls:"
   filePath <- getLine
   
   let fileNameStr = takeFileName $ swapSlashes filePath
   
   let headerStr = forwardZeros (show (length fileNameStr)) headerSize
   
   putStrLn $ "I'll add that <" <> headerStr <> "> header to Reciver.exe"
   let header = stringToLBS headerStr
   sendAll s header
   putStrLn "Done."
   
   putStrLn $ "Target file name is " <> fileNameStr <> ", right?^^"
   let fileName = stringToLBS fileNameStr
   sendAll s fileName
   putStrLn $ "Now, with all misc info sent, i can start transfer all information from " <> fileNameStr <> "."
   
   fileData <- L.readFile filePath
   sendAll s fileData
   putStrLn "Work done! File sent! Bye-bye, cutie!"


swapSlashes :: String -> String
swapSlashes path = makePathFromParts . pathToParts $ path
   where pathToParts path = case dropWhile (== '\\') path of            -- Like words, but for '\\'
                                 ""    -> []
                                 path' -> part : pathToParts path''
                                          where (part, path'') =
                                                 break (== '\\') path'
         makePathFromParts []           = ""                            -- Like words, but with '/' instead of ' '
         makePathFromParts (part:parts) = part <> go parts
            where go []           = ""
                  go (part:parts) = '/' : (part <> go parts)

forwardZeros :: String -> Int -> String
forwardZeros s size = zeros <> s
   where zeros = take (size - length s) $ cycle "0"

stringToLBS :: String -> L.ByteString
stringToLBS = L.pack . encode