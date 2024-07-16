module Main (main) where

import           AppState (initialState)

main :: IO ()
main = do
    initState <- initialState
    print initState
