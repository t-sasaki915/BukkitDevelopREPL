module AppState (AppStateIO, AppState(..), initialState) where

import           CLIOptions.CLIOptions            (CLIOptions (configFile))
import           CLIOptions.Parser                (parseCLIOptions)
import           Config.Config                    (Config)
import           Config.Loader                    (loadConfig)

import           Control.Monad.Trans.Except       (ExceptT)
import           Control.Monad.Trans.State.Strict (StateT)

type AppStateIO = ExceptT String (StateT AppState IO)

data AppState = AppState
    { cliOptions :: CLIOptions
    , config     :: Config
    }
    deriving Show

initialState :: IO AppState
initialState = do
    cliOpts <- parseCLIOptions
    conf    <- loadConfig (configFile cliOpts)

    let constructor = AppState
    return (constructor cliOpts conf)
