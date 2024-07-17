module CLIOptions.CLIOptions (CLIOptions(..), cliOptionsParser) where

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
    return $
        CLIOptions
            <$> strOption
                ( long "minecraft-dir"
               <> metavar "FilePath"
               <> value (homeDir </> "AppData" </> "Roaming" </> ".minecraft")
               <> help "Specifies Minecraft directory expressly. The default value is '~\\AppData\\Roaming\\.minecraft'."
                )
            <*> strOption
                ( long "config"
               <> metavar "FilePath"
               <> value (currentDir </> ".BukkitDevelopREPL.yaml")
               <> help "Specifies config file expressly. The default is '.\\.BukkitDevelopREPL.yaml'."
                )

