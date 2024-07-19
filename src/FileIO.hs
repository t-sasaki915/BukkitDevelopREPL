module FileIO
    ( makeDirectory
    , copyFile'
    , checkFileExistence
    , readFileBS
    , readFile'
    , writeFile'
    , directoryContents
    ) where

import           AppState

import qualified Data.ByteString  as BS
import           System.Directory
import           System.FilePath  ((</>))

makeDirectory :: FilePath -> String -> AppStateIO ()
makeDirectory = appStateIOTry . createDirectoryIfMissing True

copyFile' :: FilePath -> FilePath -> String -> AppStateIO ()
copyFile' from = appStateIOTry . copyFile from

checkFileExistence :: FilePath -> String -> AppStateIO Bool
checkFileExistence = appStateIOTry . doesFileExist

readFileBS :: FilePath -> String -> AppStateIO BS.ByteString
readFileBS = appStateIOTry . BS.readFile

readFile' :: FilePath -> String -> AppStateIO String
readFile' = appStateIOTry . readFile

writeFile' :: FilePath -> String -> String -> AppStateIO ()
writeFile' filePath = appStateIOTry . writeFile filePath

directoryContents :: FilePath -> String -> AppStateIO [FilePath]
directoryContents dirName = appStateIOTry $ map (dirName </>) <$> listDirectory dirName
