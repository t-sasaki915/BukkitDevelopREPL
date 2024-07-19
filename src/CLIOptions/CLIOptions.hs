module CLIOptions.CLIOptions (CLIOptions(..), cliOptionsParser) where

import           CrossPlatform       (defaultMinecraftDir)

import           Options.Applicative
import           System.Directory    (getCurrentDirectory, getHomeDirectory)
import           System.FilePath     ((</>))

data CLIOptions = CLIOptions
    { minecraftDir :: FilePath
    , configFile   :: FilePath
    }
    deriving Show

cliOptionsParser :: IO (Parser CLIOptions)
cliOptionsParser = do
    currentDir <- getCurrentDirectory
    homeDir    <- getHomeDirectory

    let defaultMCDir    = defaultMinecraftDir homeDir
        defaultConfPath = currentDir </> ".BukkitDevelopREPL.yaml"

    return $
        CLIOptions
            <$> strOption
                ( long "minecraft-dir"
               <> metavar "FilePath"
               <> value defaultMCDir
               <> help ("Specifies Minecraft directory expressly. The default is '" ++ defaultMCDir ++ "'.")
                )
            <*> strOption
                ( long "config"
               <> metavar "FilePath"
               <> value defaultConfPath
               <> help ("Specifies config file expressly. The default is '" ++ defaultConfPath ++ "'.")
                )

