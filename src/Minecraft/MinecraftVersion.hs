module Minecraft.MinecraftVersion
    ( MinecraftVersion(..)
    , parseMinecraftVersion
    , minecraftVersionParser
    ) where

import           Data.Text           (unpack)
import           Data.Yaml           (FromJSON (..), Value (..))
import           Options.Applicative (ReadM, eitherReader)
import           Text.Regex.Posix    ((=~))

data MinecraftVersion = MinecraftVersion Int Int Int deriving Eq

instance Show MinecraftVersion where
    show (MinecraftVersion major minor 0) =
        show major ++ "." ++ show minor
    show (MinecraftVersion major minor patch) =
        show major ++ "." ++ show minor ++ "." ++ show patch

instance Ord MinecraftVersion where
    (<=) (MinecraftVersion major1 minor1 patch1) (MinecraftVersion major2 minor2 patch2) =
        major1 <= major2 && minor1 <= minor2 && patch1 <= patch2

instance FromJSON MinecraftVersion where
    parseJSON (String txt) =
        case parseMinecraftVersion (unpack txt) of
            Just v  -> return v
            Nothing -> fail "Unrecognisable minecraft version"

    parseJSON _ = fail "Unrecognisable minecraft version"

parseMinecraftVersion :: String -> Maybe MinecraftVersion
parseMinecraftVersion str
    | str =~ "[0-9]+\\.[0-9]+\\.[0-9]+$" =
        let (major, str')  = takeWhileAndRemains (/= '.') str
            (minor, patch) = takeWhileAndRemains (/= '.') str' in
            Just (MinecraftVersion (read major) (read minor) (read patch))
    | str =~ "[0-9]+\\.[0-9]+$" =
        let (major, str') = takeWhileAndRemains (/= '.') str
            (minor, _)    = takeWhileAndRemains (/= '.') str' in
            Just (MinecraftVersion (read major) (read minor) 0)
    | otherwise = Nothing
    where
        takeWhileAndRemains f s =
            let taken = takeWhile f s in
                (taken, drop (length taken + 1) s)

minecraftVersionParser :: ReadM MinecraftVersion
minecraftVersionParser = eitherReader $ \str ->
    case parseMinecraftVersion str of
        Just v  -> Right v
        Nothing -> Left "Unrecognisable minecraft version"
