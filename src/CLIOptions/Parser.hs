module CLIOptions.Parser (parseCLIOptions) where

import           CLIOptions.CLIOptions          (CLIOptions, cliOptionsParser)

import           Data.Version                   (showVersion)
import           Options.Applicative

import           Paths_spigot_debugger_launcher (version)

parseCLIOptions :: IO CLIOptions
parseCLIOptions = do
    parser <- cliOptionsParser
    customExecParser
        (prefs disambiguate)
            (info (helper <*> parser)
                (fullDesc <> header
                    ("spigot-debugger-launcher " ++ showVersion version ++ " by TSasaki")))

