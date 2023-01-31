module Scripts (main) where

import Data.List (find)
import Scripts.DownloadImages (downloadImages)
import Scripts.OptimizeImages (optimizeImages)
import System.Environment (getArgs)

commands :: [(String, IO ())]
commands =
  [ ("optimizeImages", optimizeImages),
    ("downloadImages", downloadImages)
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [commandName] ->
      let maybeCommand = find (\(n, _) -> n == commandName) commands
       in case maybeCommand of
            Nothing -> printUnknownCommand commandName
            Just (name, command) -> do
              putStrLn $ "Running command: " ++ name
              command
    invalid -> do
      printUnknownCommand $ unwords invalid

printUnknownCommand :: String -> IO ()
printUnknownCommand unknownName = do
  putStrLn $ "Unknown command: " ++ unknownName
  putStrLn $ "Valid commands: " ++ unwords (fst <$> commands)
