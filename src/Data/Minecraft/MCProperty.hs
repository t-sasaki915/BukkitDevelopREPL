module Data.Minecraft.MCProperty
    ( MCProperties
    , MCProperty(..)
    , MCPropertyValue(..)
    , decodeMCProperties
    , encodeMCProperties
    , toTuple
    , fromTuple
    , toList
    , fromList
    , mcPropertiesWork
    , editMCProperties
    , newMCProperties
    , getProperties
    , setProperty
    , addProperty
    , getProperty
    ) where

import           Imports

import           Control.Monad.Trans.Except       (Except, runExcept)
import           Control.Monad.Trans.State.Strict (State, evalState, get, put)
import           Data.Bifunctor                   (second)
import           Data.Maybe                       (maybeToList)
import           Data.Minecraft.MCGameMode        (MCGameMode (..))
import           Text.Read                        (readMaybe)

type MCProperties = [MCProperty]

data MCProperty = MCProperty String MCPropertyValue

instance Show MCProperty where
    show (MCProperty key value) = printf "%s=%s" key (show value)

data MCPropertyValue = MCString String
                     | MCInt Int
                     | MCBool Bool
                     | MCGameMode MCGameMode
                     | MCNull

instance Show MCPropertyValue where
    show (MCString str)  = str
    show (MCInt x)       = show x
    show (MCBool True)   = "true"
    show (MCBool False)  = "false"
    show (MCGameMode gm) = show gm
    show MCNull          = ""

decodeLine :: String -> Except String (Maybe MCProperty)
decodeLine ""          = return Nothing
decodeLine ['#']       = return Nothing
decodeLine ('#'  : _ ) = return Nothing
decodeLine (' '  : xs) = decodeLine xs
decodeLine ('\t' : xs) = decodeLine xs
decodeLine ('\n' : xs) = decodeLine xs
decodeLine str
    | str =~ ("^[^=]+=[ \t]*$" :: String) =
        let (key, _) = span (/= '=') str in
            return $ Just (MCProperty key MCNull)

    | str =~ ("^[^=]+=.+$" :: String) =
        let (key, value) = second tail (span (/= '=') str) in
            return $ Just $ MCProperty key $
                case readMaybe value :: Maybe Int of
                    Just x  -> MCInt x
                    Nothing ->
                        case value of
                            "true"      -> MCBool True
                            "false"     -> MCBool False
                            "survival"  -> MCGameMode Survival
                            "creative"  -> MCGameMode Creative
                            "adventure" -> MCGameMode Adventure
                            "spectator" -> MCGameMode Spectator
                            _           -> MCString value

    | otherwise =
        throwE (printf "Unrecognisable property '%s'." str)

decodeLines :: [String] -> Except String MCProperties
decodeLines = flip foldM [] $ \properties line -> do
    property <- decodeLine line
    return (properties ++ maybeToList property)

decodeMCProperties :: String -> Either String MCProperties
decodeMCProperties = runExcept . decodeLines . lines

encodeMCProperties :: MCProperties -> String
encodeMCProperties = unlines . map show

toTuple :: MCProperty -> (String, MCPropertyValue)
toTuple (MCProperty k v) = (k, v)

fromTuple :: (String, MCPropertyValue) -> MCProperty
fromTuple (k, v) = MCProperty k v

toList :: MCProperties -> [(String, MCPropertyValue)]
toList = map toTuple

fromList :: [(String, MCPropertyValue)] -> MCProperties
fromList = map fromTuple

mcPropertiesWork :: State MCProperties a -> MCProperties -> a
mcPropertiesWork = evalState

editMCProperties :: State MCProperties () -> MCProperties -> MCProperties
editMCProperties f = mcPropertiesWork (f >> getProperties)

newMCProperties :: State MCProperties () -> MCProperties
newMCProperties = flip editMCProperties []

getProperties :: State MCProperties MCProperties
getProperties = get

setProperty :: String -> MCPropertyValue -> State MCProperties ()
setProperty key newValue = do
    getProperty key >>= \case
        Just _ -> do
            current <- getProperties
            put $ flip map current $
                \case
                    (MCProperty k _) | k == key -> MCProperty k newValue
                    x                           -> x
        Nothing ->
            addProperty key newValue

addProperty :: String -> MCPropertyValue -> State MCProperties ()
addProperty key value = do
    current <- getProperties
    put (current ++ [MCProperty key value])

getProperty :: String -> State MCProperties (Maybe MCPropertyValue)
getProperty key = getProperties <&> (lookup key . toList)
