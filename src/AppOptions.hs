module AppOptions
    ( AppOptions(..)
    , spigotServerJarFile
    , buildToolsJarFile
    , appOptionsParser
    ) where

import Constant (minecraftVersion)

import Options.Applicative
import System.FilePath ((</>))

data AppOptions = AppOptions
    { workingDir              :: FilePath
    , tempWorkDir             :: FilePath
    , minecraftDir            :: FilePath
    , minecraftAssetsDir      :: FilePath
    , minecraftLibrariesDir   :: FilePath
    , minecraftClientJarFile  :: FilePath
    , minecraftClientUsername :: String
    , minecraftClientXms      :: Int
    , minecraftClientXmx      :: Int
    , minecraftServerXms      :: Int
    , minecraftServerXmx      :: Int
    , noClient                :: Bool
    , pluginsToInstal         :: Maybe [FilePath]
    }
    deriving Show

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
           <> help "Specifies working directory expressly."
            )
        <*> strOption
            ( long "temp-work-dir"
           <> metavar "FilePath"
           <> value (currentDir </> "run" </> "temp")
           <> help "Specifies temporary working directory expressly."
            )
        <*> strOption
            ( long "minecraft-dir"
           <> metavar "FilePath"
           <> value (homeDir </> "AppData" </> "Roaming" </> ".minecraft")
           <> help "Specifies Minecraft directory expressly."
            )
        <*> strOption
            ( long "mc-assets-dir"
           <> metavar "FilePath"
           <> value (homeDir </> "AppData" </> "Roaming" </> ".minecraft" </> "assets")
           <> help "Specifies Minecraft assets directory expressly."
            )
        <*> strOption
            ( long "mc-libraries-dir"
           <> metavar "FilePath"
           <> value (homeDir </> "AppData" </> "Roaming" </> ".minecraft" </> "libraries")
           <> help "Specifies Minecraft libraries directory expressly."
            )
        <*> strOption
            ( long "mc-client-jar"
           <> metavar "FilePath"
           <> value (homeDir </> "AppData" </> "Roaming" </> ".minecraft" </> "versions" </> minecraftVersion </> (minecraftVersion ++ ".jar"))
           <> help "Specifies Minecraft client executable expressly."
            )
        <*> strOption
            ( long "mc-client-username"
           <> metavar "String"
           <> value "DEV"
           <> help "Customises Minecraft client username."
            )
        <*> option auto
            ( long "mc-client-xms"
           <> metavar "Int"
           <> value 2
           <> help "Customises Minecraft client JVM Xms."
            )
        <*> option auto
            ( long "mc-client-xmx"
           <> metavar "Int"
           <> value 2
           <> help "Customises Minecraft client JVM Xmx."
            )
        <*> option auto
            ( long "mc-server-xms"
           <> metavar "Int"
           <> value 2
           <> help "Customises Minecraft server JVM Xms."
            )
        <*> option auto
            ( long "mc-server-xmx"
           <> metavar "Int"
           <> value 2
           <> help "Customises Minecraft server JVM Xmx."
            )
        <*> switch
            ( long "no-client"
           <> help "Prevents Minecraft client from startup."
            )
        <*> optional
            ( some
                ( argument str
                    ( metavar "Plugins..."
                   <> help "Specifies Spigot plugin locations to instal."
                    )
                )
            )
