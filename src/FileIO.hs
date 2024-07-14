module FileIO
    ( makeDirectory
    , deleteDirectory
    , checkDirectoryExistence
    , checkFileExistence
    , copyFileT
    , readFileT
    , writeFileT
    , writeFileBL
    , directoryContents
    , deleteFile
    ) where

import AppOptions (AppOptions(..))

import Control.Exception (try)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT)
import System.Directory
import System.FilePath ((</>))
import System.IO.Error (ioeGetErrorString)

import qualified Data.ByteString.Lazy as BL

makeDirectory :: FilePath -> String -> ExceptT String (StateT AppOptions IO) ()
makeDirectory dirName errorMsg =
    lift (lift (try (createDirectoryIfMissing False dirName))) >>= \case
        Right ()   -> return ()
        Left ioErr -> throwE (errorMsg ++ ": " ++ ioeGetErrorString ioErr)

deleteDirectory :: FilePath -> String -> ExceptT String (StateT AppOptions IO) ()
deleteDirectory dirName errorMsg =
    lift (lift (try (removeDirectoryRecursive dirName))) >>= \case
        Right ()   -> return ()
        Left ioErr -> throwE (errorMsg ++ ": " ++ ioeGetErrorString ioErr)

checkDirectoryExistence :: FilePath -> String -> ExceptT String (StateT AppOptions IO) Bool
checkDirectoryExistence dirName errorMsg =
    lift (lift (try (doesDirectoryExist dirName))) >>= \case
        Right result -> return result
        Left ioErr   -> throwE (errorMsg ++ ": " ++ ioeGetErrorString ioErr)

checkFileExistence :: FilePath -> String -> ExceptT String (StateT AppOptions IO) Bool
checkFileExistence filePath errorMsg =
    lift (lift (try (doesFileExist filePath))) >>= \case
        Right result -> return result
        Left ioErr   -> throwE (errorMsg ++ ": " ++ ioeGetErrorString ioErr)

copyFileT :: FilePath -> FilePath -> String -> ExceptT String (StateT AppOptions IO) ()
copyFileT from to errorMsg =
    lift (lift (try (copyFile from to))) >>= \case
        Right ()   -> return ()
        Left ioErr -> throwE (errorMsg ++ ": " ++ ioeGetErrorString ioErr)

readFileT :: FilePath -> String -> ExceptT String (StateT AppOptions IO) String
readFileT filePath errorMsg =
    lift (lift (try (readFile filePath))) >>= \case
        Right content -> return content
        Left ioErr    -> throwE (errorMsg ++ ": " ++ ioeGetErrorString ioErr)

writeFileT :: FilePath -> String -> String -> ExceptT String (StateT AppOptions IO) ()
writeFileT filePath content errorMsg =
    lift (lift (try (writeFile filePath content))) >>= \case
        Right ()   -> return ()
        Left ioErr -> throwE (errorMsg ++ ": " ++ ioeGetErrorString ioErr)

writeFileBL :: FilePath -> BL.ByteString -> String -> ExceptT String (StateT AppOptions IO) ()
writeFileBL filePath content errorMsg =
    lift (lift (try (BL.writeFile filePath content))) >>= \case
        Right ()   -> return ()
        Left ioErr -> throwE (errorMsg ++ ": " ++ ioeGetErrorString ioErr)

directoryContents :: FilePath -> String -> ExceptT String (StateT AppOptions IO) [FilePath]
directoryContents filePath errorMsg =
    lift (lift (try (map (filePath </>) <$> listDirectory filePath))) >>= \case
        Right lst  -> return lst
        Left ioErr -> throwE (errorMsg ++ ": " ++ ioeGetErrorString ioErr)

deleteFile :: FilePath -> String -> ExceptT String (StateT AppOptions IO) ()
deleteFile filePath errorMsg =
    lift (lift (try (removeFile filePath))) >>= \case
        Right ()   -> return ()
        Left ioErr -> throwE (errorMsg ++ ": " ++ ioeGetErrorString ioErr)
