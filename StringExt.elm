module StringExt where

import String exposing (..)
import Char

capitalize : String -> String
capitalize string =
  case uncons string of
   Nothing -> ""
   Just (head, tail) ->
      cons (Char.toUpper head) tail
