module Network.Url
    ( Url
    , FileExtension(..)
    , isUrl
    , isDirectLinkOf
    , getFileNameFromUrl
    ) where

import           AppState
import           CrossPlatform              (curlExecName)
import           ProcessIO

import           Control.Monad.Trans.Except (throwE)
import           Data.List.Extra            (splitOn, stripInfix)
import           Text.Regex.Posix           ((=~))

type Url = String

data FileExtension = Jar

instance Show FileExtension where
    show Jar = "jar"

isUrl :: String -> Bool
isUrl = (=~ ("^(https?:\\/\\/|ftp:\\/\\/|file:\\/\\/\\/)?[A-Za-z0-9_\\-]+(\\.[A-Za-z0-9_\\-]+)+((\\?|\\/).*)?$" :: String))

isDirectLinkOf :: FileExtension -> Url -> Bool
isDirectLinkOf ext = (=~ ("\\/.+\\." ++ show ext ++ "$" :: String))

getFileNameFromUrl :: FileExtension -> Url -> AppStateIO String
getFileNameFromUrl ext url | isDirectLinkOf ext url =
    return $ last (splitOn "/" url)

getFileNameFromUrl ext url = do
    workingDir <- getWorkingDir

    curlOutput <- execProcessAndGetOutput curlExecName ["-L", "-I", "-s", url] workingDir $
        "Failed to execute curl that was to check the file name of '" ++ url ++ "'"

    case lookup "Content-Disposition" (splitHeader (lines curlOutput)) of
        Just headerValue ->
            case lookup "filename" (splitProperty (splitSemicolon headerValue)) of
                Just fileName | fileName =~ (".+\\." ++ show ext ++ "$" :: String) ->
                    return fileName

                Just fileName ->
                    throwE ("'" ++ fileName ++ "' is not a " ++ show ext ++ " file.")

                Nothing ->
                    throwE ("Could not find the filename of '" ++ url ++ "'.")

        Nothing ->
            throwE ("Could not find the Content-Disposition of '" ++ url ++ "'.")
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

