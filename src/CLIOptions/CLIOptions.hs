module CLIOptions.CLIOptions (CLIOptions(..), cliOptionsParser) where

import           Imports

import           CrossPlatform       (defaultMinecraftDir)

import           Options.Applicative
import           System.Directory    (getCurrentDirectory, getHomeDirectory)

data CLIOptions = CLIOptions
    { minecraftDir     :: FilePath
    , configFile       :: FilePath
    , enableStacktrace :: Bool
    , dynamicPlugins   :: Maybe [FilePath]
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
               <> short 'm'
               <> metavar "FilePath"
               <> value defaultMCDir
               <> showDefault
               <> help "Specifies Minecraft directory expressly."
                )
            <*> strOption
                ( long "config"
               <> short 'c'
               <> metavar "FilePath"
               <> value defaultConfPath
               <> showDefault
               <> help "Specifies config file expressly."
                )
            <*> switch
                ( long "stacktrace"
               <> short 's'
               <> help "Prints a stacktrace when an error has occurred."
                )
            <*> optional
                ( some
                    ( argument str
                        ( metavar "Plugins..."
                       <> help "Specifies paths to dynamic plugins."
                        )
                    )
                )

