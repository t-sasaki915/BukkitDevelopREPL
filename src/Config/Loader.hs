module Config.Loader (loadConfig) where

import           Imports

import           Config.Config   (Config)
import           Config.Resource (defaultConfigFile)

import qualified Data.ByteString as BS
import           Data.Yaml       (decodeEither', prettyPrintParseException)

writeDefaultConfigFile :: FilePath -> IO ()
writeDefaultConfigFile filePath =
    BS.writeFile filePath defaultConfigFile

readConfigFile :: FilePath -> IO BS.ByteString
readConfigFile filePath =
    doesFileExist filePath >>= \case
        True -> BS.readFile filePath
        False -> do
            putStrLn $ printf "Could not find '%s'. Using default configs." filePath
            writeDefaultConfigFile filePath
            return defaultConfigFile

loadConfig :: HasCallStack => FilePath -> IO Config
loadConfig filePath = do
    rawConfig <- readConfigFile filePath
    case decodeEither' rawConfig of
        Right conf -> return conf
        Left err   -> error (prettyPrintParseException err)
