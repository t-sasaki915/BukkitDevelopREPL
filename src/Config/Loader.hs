module Config.Loader (loadConfig) where

import           Config.Config    (Config)
import           Config.Resource  (defaultConfigFile)

import qualified Data.ByteString  as BS
import           Data.Yaml        (decodeEither', prettyPrintParseException)
import           System.Directory (doesFileExist)
import           System.Exit      (exitFailure)
import           Text.Printf      (printf)

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

loadConfig :: FilePath -> IO Config
loadConfig filePath = do
    rawConfig <- readConfigFile filePath
    case decodeEither' rawConfig of
        Right conf -> return conf
        Left err -> do
            putStrLn (prettyPrintParseException err)
            exitFailure
