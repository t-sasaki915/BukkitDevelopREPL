module CLIOptions.Parser (parseCLIOptions) where

import           CLIOptions.CLIOptions   (CLIOptions, cliOptionsParser)

import           Data.Version            (showVersion)
import           Options.Applicative
import           Text.Printf             (printf)

import           Paths_BukkitDevelopREPL (version)

parseCLIOptions :: IO CLIOptions
parseCLIOptions = do
    parser <- cliOptionsParser
    customExecParser
        (prefs disambiguate)
            (info (helper <*> parser)
                (fullDesc <> header
                    (printf "BukkitDevelopREPL %s by TSasaki" (showVersion version))))

