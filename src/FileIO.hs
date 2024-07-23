module FileIO
    ( makeDirectory
    , copyFile'
    , checkFileExistence
    , checkDirectoryExistence
    , readFileBS
    , readFile'
    , writeFile'
    , removeFile'
    , directoryContents
    ) where

import           Imports

import           AppState

import qualified Data.ByteString  as BS
import           System.Directory

makeDirectory :: FilePath -> String -> AppStateIO ()
makeDirectory = appStateIOTry . createDirectoryIfMissing True

copyFile' :: FilePath -> FilePath -> String -> AppStateIO ()
copyFile' from = appStateIOTry . copyFile from

checkFileExistence :: FilePath -> String -> AppStateIO Bool
checkFileExistence = appStateIOTry . doesFileExist

checkDirectoryExistence :: FilePath -> String -> AppStateIO Bool
checkDirectoryExistence = appStateIOTry . doesDirectoryExist

readFileBS :: FilePath -> String -> AppStateIO BS.ByteString
readFileBS = appStateIOTry . BS.readFile

readFile' :: FilePath -> String -> AppStateIO String
readFile' = appStateIOTry . readFile

writeFile' :: FilePath -> String -> String -> AppStateIO ()
writeFile' filePath = appStateIOTry . writeFile filePath

removeFile' :: FilePath -> String -> AppStateIO ()
removeFile' = appStateIOTry . removeFile

directoryContents :: FilePath -> String -> AppStateIO [FilePath]
directoryContents dirName = appStateIOTry $ map (dirName </>) <$> listDirectory dirName
