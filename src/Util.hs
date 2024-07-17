module Util (fillWithSpace) where

fillWithSpace :: Int -> String -> String
fillWithSpace n str | n <= length str = str
fillWithSpace n str = str ++ replicate (n - length str) ' '
