module FileIO
    ( makeDirectory
    , checkFileExistence
    , verifyFileExistence
    , copyFileT
    , readFileT
    , writeFileT
    ) where

import AppOptions (AppOptions(..))

import Control.Exception (try)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT)
import System.Directory
import System.IO.Error (ioeGetErrorString)

makeDirectory :: FilePath -> ExceptT String (StateT AppOptions IO) ()
makeDirectory dirName =
    lift (lift (try (createDirectoryIfMissing False dirName))) >>= \case
        Right ()   -> return ()
        Left ioErr -> throwE $
            "Failed to create a directory " ++ dirName ++ ": " ++ ioeGetErrorString ioErr

checkFileExistence :: FilePath -> ExceptT String (StateT AppOptions IO) Bool
checkFileExistence filePath =
    lift (lift (try (doesFileExist filePath))) >>= \case
        Right result -> return result
        Left ioErr   -> throwE $
            "Failed to check the existence of " ++ filePath ++ ": " ++ ioeGetErrorString ioErr

verifyFileExistence :: FilePath -> String -> ExceptT String (StateT AppOptions IO) ()
verifyFileExistence filePath errorMsg =
    checkFileExistence filePath >>= \case
        True  -> return ()
        False -> throwE errorMsg

copyFileT :: FilePath -> FilePath -> ExceptT String (StateT AppOptions IO) ()
copyFileT from to =
    lift (lift (try (copyFile from to))) >>= \case
        Right ()   -> return ()
        Left ioErr -> throwE $
            "Failed to copy a file " ++ from ++ " to " ++ to ++ ": " ++ ioeGetErrorString ioErr

readFileT :: FilePath -> ExceptT String (StateT AppOptions IO) String
readFileT filePath =
    lift (lift (try (readFile filePath))) >>= \case
        Right content -> return content
        Left ioErr    -> throwE $
            "Failed to read a file " ++ filePath ++ ": " ++ ioeGetErrorString ioErr

writeFileT :: FilePath -> String -> ExceptT String (StateT AppOptions IO) ()
writeFileT filePath content =
    lift (lift (try (writeFile filePath content))) >>= \case
        Right ()   -> return ()
        Left ioErr -> throwE $
            "Failed to write a file " ++ filePath ++ ": " ++ ioeGetErrorString ioErr
