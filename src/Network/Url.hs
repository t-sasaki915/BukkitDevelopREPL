module Network.Url
    ( Url
    , FileExtension(..)
    , isUrl
    , takeFileNameFromUrl
    ) where

import           Imports

import           Data.List.Extra (splitOn)

type Url = String

data FileExtension = Jar

instance Show FileExtension where
    show Jar = "jar"

isUrl :: String -> Bool
isUrl = (=~ ("^(https?:\\/\\/|ftp:\\/\\/|file:\\/\\/\\/)?[A-Za-z0-9_\\-]+(\\.[A-Za-z0-9_\\-]+)+((\\?|\\/).*)?$" :: String))

takeFileNameFromUrl :: HasCallStack => FileExtension -> Url -> String
takeFileNameFromUrl ext url
    | url =~ (printf "\\/.+\\.%s$" (show ext) :: String) =
        last (splitOn "/" url)
    | otherwise =
        error (printf "'%s' is not an obvious link to a %s file." url (show ext))
