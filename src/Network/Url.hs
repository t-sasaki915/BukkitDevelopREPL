module Network.Url
    ( Url
    , FileExtension(..)
    , isUrl
    , isDirectLinkOf
    , getFileNameFromUrl
    ) where

import           Imports

import           CrossPlatform   (curlExecName)

import           Data.List.Extra (splitOn, stripInfix)
import           System.Process  (proc, readCreateProcess)

type Url = String

data FileExtension = Jar

instance Show FileExtension where
    show Jar = "jar"

isUrl :: String -> Bool
isUrl = (=~ ("^(https?:\\/\\/|ftp:\\/\\/|file:\\/\\/\\/)?[A-Za-z0-9_\\-]+(\\.[A-Za-z0-9_\\-]+)+((\\?|\\/).*)?$" :: String))

isDirectLinkOf :: FileExtension -> Url -> Bool
isDirectLinkOf ext = (=~ (printf "\\/.+\\.%s$" (show ext) :: String))

getFileNameFromUrl :: HasCallStack => FileExtension -> Url -> IO String
getFileNameFromUrl ext url | isDirectLinkOf ext url =
    return $ last (splitOn "/" url)

getFileNameFromUrl ext url = do
    putStrLn (printf "Checking the filename of '%s': %%s." url)

    curlOutput <- readCreateProcess (proc curlExecName ["-L", "-I", "-s", url]) []

    case lookup "Content-Disposition" (splitHeader (lines curlOutput)) of
        Just headerValue ->
            case lookup "filename" (splitProperty (splitSemicolon headerValue)) of
                Just fileName | fileName =~ (printf ".+\\.%s$" (show ext) :: String) ->
                    return fileName

                Just fileName ->
                    error (printf "'%s' is not a '%s' file." fileName (show ext))

                Nothing ->
                    error (printf "Could not find the filename of '%s'." url)

        Nothing ->
            error (printf "Could not find the Content-Disposition of '%s'." url)
    where
        splitHeader :: [String] -> [(String, String)]
        splitHeader = flip foldl [] $ \headers line ->
            case stripInfix ": " line of
                Just x  -> headers ++ [x]
                Nothing -> headers

        splitSemicolon :: String -> [String]
        splitSemicolon = splitOn "; "

        splitProperty :: [String] -> [(String, String)]
        splitProperty = flip foldl [] $ \properties property ->
            case stripInfix "=" property of
                Just x  -> properties ++ [x]
                Nothing -> properties

