module Config.Loader (loadConfig) where

import           Config.Config    (Config)
import           Config.Resource  (defaultConfigFile)

import qualified Data.ByteString  as BS
import           Data.Yaml        (decodeThrow)
import           System.Directory (doesFileExist)

writeDefaultConfigFile :: FilePath -> IO ()
writeDefaultConfigFile filePath =
    BS.writeFile filePath defaultConfigFile

readConfigFile :: FilePath -> IO BS.ByteString
readConfigFile filePath =
    doesFileExist filePath >>= \case
        True -> BS.readFile filePath
        False -> do
            putStrLn ("Could not find '" ++ filePath ++ "'. Using default configs.")
            writeDefaultConfigFile filePath
            return defaultConfigFile

loadConfig :: FilePath -> IO Config
loadConfig filePath = do
    rawConfig <- readConfigFile filePath
    decodeThrow rawConfig
