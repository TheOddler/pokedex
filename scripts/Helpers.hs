{-# LANGUAGE FlexibleInstances #-}

module Helpers where

import Control.Monad (forM_)
import Data.List (intercalate)

class ProgressInfo a where
  getMessage :: a -> Maybe String

instance ProgressInfo () where
  getMessage () = Nothing

instance ProgressInfo String where
  getMessage str =
    case str of
      "" -> Nothing
      s -> Just s

data ProgressMessage
  = NoMessage
  | Message String

instance ProgressInfo ProgressMessage where
  getMessage NoMessage = Nothing
  getMessage (Message msg) = Just msg

forShowProgress_ :: (Show a, ProgressInfo progressInfo) => [a] -> (a -> IO progressInfo) -> IO ()
forShowProgress_ inputs f = do
  let total = length inputs
  forM_ (zip [0 ..] inputs) $ \(index, input) -> do
    info <- f input
    let percentage = index * 100 `div` total
    let suffix = case getMessage info of
          Nothing -> ""
          Just msg -> ": \t" <> msg
    putStrLn $ show percentage <> "% - " <> show input <> suffix

  putStrLn "Done!"
