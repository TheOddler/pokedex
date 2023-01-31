module Scripts.DownloadImages (downloadImages) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.String.Utils (split)
import Helpers (ProgressMessage (Message, NoMessage), forShowProgress_)
import Network.HTTP.Conduit (simpleHttp)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.FilePath (takeExtension, takeFileName)

outputFolder :: String
outputFolder = "images/"

downloadImages :: IO ()
downloadImages = do
  createDirectoryIfMissing True outputFolder
  input <- readFile "imageurls.txt"
  forShowProgress_ (lines input) $ \line -> do
    case split " -> " line of
      [url] -> processLine url Nothing
      [url, nameOverwrite] -> processLine url $ Just nameOverwrite
      _ -> pure $ Message $ "Failed parsing line: " <> line

processLine :: String -> Maybe String -> IO ProgressMessage
processLine url nameOverwrite = do
  let nameFromUrl = takeFileName url
  let finalName = case nameOverwrite of
        Just overwrite -> overwrite ++ takeExtension url
        Nothing -> nameFromUrl
  let finalPath = outputFolder ++ finalName

  -- Check if file with correct name already exists, if so, we are all done already
  allDone <- doesFileExist finalPath
  if allDone
    then pure $ Message "Skipping line, final file already exists."
    else do
      -- If a file with the name from url exists, we just need to rename it
      let unRenamedFilePath = outputFolder ++ nameFromUrl
      onlyNeedsRenaming <- doesFileExist unRenamedFilePath
      if onlyNeedsRenaming
        then do
          renameFile unRenamedFilePath finalPath
          pure $ Message "Skipping download, only renaming."
        else do
          body <- simpleHttp url
          B.writeFile finalPath (LB.toStrict body)
          pure NoMessage
