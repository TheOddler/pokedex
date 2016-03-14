module ListExt where

import List exposing (take, drop, length)

split : Int -> List a -> List (List a)
split n xs =
    if (length xs) > n then (take n xs) :: (split n (drop n xs)) else [xs]
