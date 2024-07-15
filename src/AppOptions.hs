module AppOptions
    ( AppOptions(..)
    , tempWorkDir
    , minecraftAssetsDir
    , minecraftLibrariesDir
    , minecraftClientJarFile
    , spigotServerJarFile
    , buildToolsJarFile
    , appOptionsParser
    ) where

import           Constant            (minecraftVersion)

import           Options.Applicative
import           System.FilePath     ((</>))

data AppOptions = AppOptions
    { workingDir              :: FilePath
    , minecraftDir            :: FilePath
    , minecraftClientUsername :: String
    , minecraftClientXms      :: Int
    , minecraftClientXmx      :: Int
    , minecraftServerXms      :: Int
    , minecraftServerXmx      :: Int
    , noClient                :: Bool
    , pluginsToInstall        :: Maybe [FilePath]
    }
    deriving Show

tempWorkDir :: AppOptions -> FilePath
tempWorkDir opts = workingDir opts </> "temp"

minecraftAssetsDir :: AppOptions -> FilePath
minecraftAssetsDir opts = minecraftDir opts </> "assets"

minecraftLibrariesDir :: AppOptions -> FilePath
minecraftLibrariesDir opts = minecraftDir opts </> "libraries"

minecraftClientJarFile :: AppOptions -> FilePath
minecraftClientJarFile opts =
    minecraftDir opts </> "versions" </> minecraftVersion </> (minecraftVersion ++ ".jar")

spigotServerJarFile :: AppOptions -> FilePath
spigotServerJarFile opts = workingDir opts </> ("spigot-" ++ minecraftVersion ++ ".jar")

buildToolsJarFile :: AppOptions -> FilePath
buildToolsJarFile opts = tempWorkDir opts </> "BuildTools.jar"

appOptionsParser :: FilePath -> FilePath -> Parser AppOptions
appOptionsParser currentDir homeDir =
    AppOptions
        <$> strOption
            ( long "work-dir"
           <> metavar "FilePath"
           <> value (currentDir </> "run")
           <> help "Specifies working directory expressly. The default value is '.\\run'."
            )
        <*> strOption
            ( long "minecraft-dir"
           <> metavar "FilePath"
           <> value (homeDir </> "AppData" </> "Roaming" </> ".minecraft")
           <> help "Specifies Minecraft directory expressly. The default value is '~\\AppData\\Roaming\\.minecraft'."
            )
        <*> strOption
            ( long "mc-client-username"
           <> metavar "String"
           <> value "DEV"
           <> help "Customises Minecraft client username. The default value is 'DEV'."
            )
        <*> option auto
            ( long "mc-client-xms"
           <> metavar "Int"
           <> value 2
           <> help "Customises Minecraft client JVM Xms. The default value is '2'."
            )
        <*> option auto
            ( long "mc-client-xmx"
           <> metavar "Int"
           <> value 2
           <> help "Customises Minecraft client JVM Xmx. The default value is '2'."
            )
        <*> option auto
            ( long "mc-server-xms"
           <> metavar "Int"
           <> value 2
           <> help "Customises Minecraft server JVM Xms. The default value is '2'."
            )
        <*> option auto
            ( long "mc-server-xmx"
           <> metavar "Int"
           <> value 2
           <> help "Customises Minecraft server JVM Xmx. The default value is '2'."
            )
        <*> switch
            ( long "no-client"
           <> help "Prevents Minecraft client from startup."
            )
        <*> optional
            ( some
                ( argument str
                    ( metavar "Plugins..."
                   <> help "Specifies Spigot plugin locations to install."
                    )
                )
            )
