module Minecraft.MinecraftPropertyAnalyser (MCProperty(..), MCPropertyValue(..), parseMinecraftProperty, lookupProperty) where

import           Control.Monad              (foldM)
import           Control.Monad.Trans.Except (Except, runExcept, throwE)
import           Data.List                  (find)
import           Data.Maybe                 (maybeToList)
import           Text.Read                  (readMaybe)
import           Text.Regex.Posix           ((=~))

data MCProperty = MCProperty String MCPropertyValue

data MCPropertyValue = MCString String
                     | MCInt Int
                     | MCBool Bool

analyseLine :: String -> Except String (Maybe MCProperty)
analyseLine ""          = return Nothing
analyseLine [_]         = return Nothing
analyseLine ('#'  : _ ) = return Nothing
analyseLine (' '  : xs) = analyseLine xs
analyseLine ('\t' : xs) = analyseLine xs
analyseLine ('\n' : xs) = analyseLine xs
analyseLine str
    | str =~ "^([^=]+=[^=]+)$" =
        let (key, value) = mapSnd tail (span (/= '=') str) in
            case readMaybe value :: Maybe Int of
                Just x  -> return (Just (MCProperty key (MCInt x)))
                Nothing ->
                    case value of
                        "true"  -> return (Just (MCProperty key (MCBool True)))
                        "false" -> return (Just (MCProperty key (MCBool False)))
                        _       -> return (Just (MCProperty key (MCString value)))
    | otherwise =
        throwE ("Unrecognisable property '" ++ str ++ "'")
    where mapSnd f (a, b) = (a, f b)

analyseLines :: [String] -> Except String [MCProperty]
analyseLines = flip foldM [] $ \properties line -> do
    property <- analyseLine line
    return (properties ++ maybeToList property)

parseMinecraftProperty :: String -> Either String [MCProperty]
parseMinecraftProperty = runExcept . analyseLines . lines

lookupProperty :: String -> [MCProperty] -> Maybe MCPropertyValue
lookupProperty key = fmap (\(MCProperty _ v) -> v) . find (\(MCProperty k _) -> k == key)
