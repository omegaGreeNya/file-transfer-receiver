module Main where

-- Network
import Network.Socket
import Network.Run.TCP (runTCPServer)
import Network.Socket.ByteString (recv)

-- ByteString stuff
import qualified Data.ByteString as B
-- For decoding [Word8] to [Char]
import Data.String.UTF8 (encode, decode)

-- Adding dirs
import System.Directory (createDirectoryIfMissing)

import Control.Monad (unless)

port :: ServiceName
port = "3000"

waitingMsg :: String
waitingMsg = "Waiting for signal from Sender.exe"

main :: IO ()
main = do
   putStrLn waitingMsg
   
   runTCPServer Nothing port $ \s -> do
      putStrLn "Enter directory to save file (Enter for default):"
      dir <- getLine
      
      header <- recv s 3
      let headerStr = fst . decode . B.unpack $ header
      
      fileName <- recv s $ read headerStr
      let fileNameStr = fst . decode . B.unpack $ fileName
      
      let properFilePath = makeProperDir dir
      if properFilePath == ""
         then putStrLn $ "Saving file " <> fileNameStr
         else putStrLn $ "Saving file " <> fileNameStr <> " into /" <> properFilePath
      
      createDirectoryIfMissing True properFilePath
      
      let fullFilePath = properFilePath <> fileNameStr
      receiveFileData s fullFilePath
      
      print "File received and saved!"
      putStrLn waitingMsg
         where
            receiveFileData s pathName = do
               msg <- recv s 1024
               unless (B.null msg) $ do
                  putStrLn $ "Received " <> show (B.length msg) <> " Bytes."
                  B.appendFile pathName msg
                  receiveFileData s pathName




makeProperDir :: String -> FilePath
makeProperDir path = properEnd $ swapSlashes path
   where properEnd ""   = ""
         properEnd path = if last path == '/'
                             then path
                             else path <> "/"

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