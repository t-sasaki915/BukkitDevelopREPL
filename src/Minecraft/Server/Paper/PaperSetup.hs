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

newtype PaperBuilds = PaperBuilds
    { builds :: [Int]
    }
    deriving Show

instance FromJSON PaperBuilds where
    parseJSON (Object m) =
        PaperBuilds
            <$> (m .: "builds")

    parseJSON _ = fail "Unrecognisable Paper Downloads API"

makeNecessaryDirectories :: AppStateIO ()
makeNecessaryDirectories = do
    workingDir <- getWorkingDir

    makeDirectory workingDir $
        "Failed to make a directory '" ++ workingDir ++ "'"

fetchPaperBuilds :: AppStateIO PaperBuilds
fetchPaperBuilds = do
    serverVersion <- getServerVersion
    workingDir    <- getWorkingDir

    putStrLn' "Fetching Paper Downloads API..."

    let apiUrl = "https://api.papermc.io/v2/projects/paper/versions/" ++ show serverVersion
    execProcessAndGetOutput curlExecName ["-s", apiUrl] workingDir
        "Failed to execute curl that was to fetch Paper Downloads API" >>=
            \rawJson -> case eitherDecode (pack (map c2w rawJson)) of
                Right paperBuilds -> return paperBuilds
                Left err          -> throwE err

downloadLatestPaper :: AppStateIO ()
downloadLatestPaper = do
    ver         <- getServerVersion
    paperBuilds <- fetchPaperBuilds
    workingDir  <- getWorkingDir

    let latest   = show $ last (builds paperBuilds)
        jarName  = "paper-" ++ show ver ++ "-" ++ latest ++ ".jar"
        jarName' = getMCServerExecutableName Paper ver
        jarUrl   = "https://api.papermc.io/v2/projects/paper/versions/" ++ show ver ++ "/builds/" ++ latest ++ "/downloads/" ++ jarName
        jarPath  = workingDir </> jarName'

    putStrLn' ("Downloading Paper " ++ show ver ++ " Build " ++ latest ++ "...")

    execProcess curlExecName ["-L", "-o", jarPath, jarUrl] workingDir
        "Failed to execute curl that was to download a Paper Server" >>=
            expectExitSuccess
                "Failed to download a Paper server"

setupPaper :: AppStateIO ()
setupPaper = do
    makeNecessaryDirectories
    downloadLatestPaper
