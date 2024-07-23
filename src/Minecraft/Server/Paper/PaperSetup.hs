module Minecraft.Server.Paper.PaperSetup (setupPaper) where

import           AppState
import           CrossPlatform                (curlExecName)
import           FileIO
import           ProcessIO

import           Control.Monad.Trans.Except   (throwE)
import           Data.Aeson                   (FromJSON (parseJSON),
                                               Value (Object), eitherDecode,
                                               (.:))
import           Data.ByteString.Internal     (c2w)
import           Data.ByteString.Lazy         (pack)
import           Data.Minecraft.MCServerBrand (MCServerBrand (Paper),
                                               getMCServerExecutableName)
import           Data.Minecraft.MCVersion     ()
import           System.FilePath              ((</>))
import           Text.Printf                  (printf)

newtype PaperBuilds = PaperBuilds
    { builds :: [Int]
    }
    deriving Show

instance FromJSON PaperBuilds where
    parseJSON (Object m) =
        PaperBuilds
            <$> (m .: "builds")

    parseJSON x = fail (printf "Unrecognisable Paper builds '%s'." (show x))

makeNecessaryDirectories :: AppStateIO ()
makeNecessaryDirectories = do
    workingDir <- getWorkingDir

    makeDirectory workingDir $
        printf "Failed to make a directory '%s': %%s." workingDir

fetchPaperBuilds :: AppStateIO PaperBuilds
fetchPaperBuilds = do
    serverVersion <- getServerVersion
    workingDir    <- getWorkingDir

    putStrLn' "Fetching Paper Downloads API..."

    let apiUrl = printf "https://api.papermc.io/v2/projects/paper/versions/%s" (show serverVersion)
    execProcessAndGetOutput curlExecName ["-s", apiUrl] workingDir
        "Failed to execute curl that was to fetch Paper Downloads API: %s" >>=
            \rawJson -> case eitherDecode (pack (map c2w rawJson)) of
                Right paperBuilds -> return paperBuilds
                Left err          -> throwE err

downloadLatestPaper :: AppStateIO ()
downloadLatestPaper = do
    ver         <- getServerVersion
    paperBuilds <- fetchPaperBuilds
    workingDir  <- getWorkingDir

    let latest   = last (builds paperBuilds)
        jarName  = printf "paper-%s-%d.jar" (show ver) latest :: String
        jarName' = getMCServerExecutableName Paper ver
        jarUrl   = printf "https://api.papermc.io/v2/projects/paper/versions/%s/builds/%d/downloads/%s" (show ver) latest jarName
        jarPath  = workingDir </> jarName'

    putStrLn' (printf "Downloading Paper %s Build %d ..." (show ver) latest)

    execProcess curlExecName ["-L", "-o", jarPath, jarUrl] workingDir
        (printf "Failed to execute curl that was to download a Paper server '%s': %%s." jarUrl) >>=
            expectExitSuccess
                (printf "Failed to download a Paper server '%s': %%s." jarUrl)

setupPaper :: AppStateIO ()
setupPaper = do
    makeNecessaryDirectories
    downloadLatestPaper
