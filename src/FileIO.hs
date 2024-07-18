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

import           Control.Exception          (try)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (throwE)
import qualified Data.ByteString            as BS
import           System.Directory
import           System.FilePath            ((</>))
import           System.IO.Error            (ioeGetErrorString)

makeDirectory :: FilePath -> String -> AppStateIO ()
makeDirectory dirName errorMsg =
    lift (lift (try (createDirectoryIfMissing True dirName))) >>= \case
        Right ()   -> return ()
        Left ioErr -> throwE (errorMsg ++ ": " ++ ioeGetErrorString ioErr)

copyFile' :: FilePath -> FilePath -> String -> AppStateIO ()
copyFile' from to errorMsg =
    lift (lift (try (copyFile from to))) >>= \case
        Right ()   -> return ()
        Left ioErr -> throwE (errorMsg ++ ": " ++ ioeGetErrorString ioErr)

checkFileExistence :: FilePath -> String -> AppStateIO Bool
checkFileExistence filePath errorMsg =
    lift (lift (try (doesFileExist filePath))) >>= \case
        Right bool -> return bool
        Left ioErr -> throwE (errorMsg ++ ": " ++ ioeGetErrorString ioErr)

readFileBS :: FilePath -> String -> AppStateIO BS.ByteString
readFileBS filePath errorMsg =
    lift (lift (try (BS.readFile filePath))) >>= \case
        Right byteStr -> return byteStr
        Left ioErr    -> throwE (errorMsg ++ ": " ++ ioeGetErrorString ioErr)

readFile' :: FilePath -> String -> AppStateIO String
readFile' filePath errorMsg =
    lift (lift (try (readFile filePath))) >>= \case
        Right str  -> return str
        Left ioErr -> throwE (errorMsg ++ ": " ++ ioeGetErrorString ioErr)

writeFile' :: FilePath -> String -> String -> AppStateIO ()
writeFile' filePath content errorMsg =
    lift (lift (try (writeFile filePath content))) >>= \case
        Right ()   -> return ()
        Left ioErr -> throwE (errorMsg ++ ": " ++ ioeGetErrorString ioErr)

directoryContents :: FilePath -> String -> AppStateIO [FilePath]
directoryContents dirName errorMsg =
    lift (lift (try (map (dirName </>) <$> listDirectory dirName))) >>= \case
        Right lst  -> return lst
        Left ioErr -> throwE (errorMsg ++ ": " ++ ioeGetErrorString ioErr)
