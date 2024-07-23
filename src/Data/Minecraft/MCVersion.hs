module Data.Minecraft.MCVersion
    ( MCVersion(..)
    , parseMCVersion
    , mcVersionParser
    ) where

import           Imports

import           Data.Text           (pack, unpack)
import           Data.Yaml           (FromJSON (..), ToJSON (..), Value (..))
import           Options.Applicative (ReadM, eitherReader)

data MCVersion = MCVersion Int Int Int deriving Eq

instance Show MCVersion where
    show (MCVersion major minor 0) =
        printf "%d.%d" major minor
    show (MCVersion major minor patch) =
        printf "%d.%d.%d" major minor patch

instance Ord MCVersion where
    (<=) v1 v2 = v1 == v2 || v1 < v2

    (<) (MCVersion major1 minor1 patch1) (MCVersion major2 minor2 patch2)
        | major1 < major2                                         = True
        | major1 == major2 && minor1 < minor2                     = True
        | major1 == major2 && minor1 == minor2 && patch1 < patch2 = True
        | otherwise                                               = False

instance FromJSON MCVersion where
    parseJSON (String txt) =
        case parseMCVersion (unpack txt) of
            Just v  -> return v
            Nothing -> fail (printf "Unrecognisable minecraft version '%s'." txt)

    parseJSON x = fail (printf "Unrecognisable minecraft version '%s'." (show x))

instance ToJSON MCVersion where
    toJSON mcVersion = String (pack $ show mcVersion)

parseMCVersion :: String -> Maybe MCVersion
parseMCVersion str
    | str =~ ("[0-9]+\\.[0-9]+\\.[0-9]+$" :: String) =
        let (major, str')  = takeWhileAndRemains (/= '.') str
            (minor, patch) = takeWhileAndRemains (/= '.') str' in
            Just (MCVersion (read major) (read minor) (read patch))
    | str =~ ("[0-9]+\\.[0-9]+$" :: String) =
        let (major, str') = takeWhileAndRemains (/= '.') str
            (minor, _)    = takeWhileAndRemains (/= '.') str' in
            Just (MCVersion (read major) (read minor) 0)
    | otherwise = Nothing
    where
        takeWhileAndRemains f s =
            let taken = takeWhile f s in
                (taken, drop (length taken + 1) s)

mcVersionParser :: ReadM MCVersion
mcVersionParser = eitherReader $ \str ->
    case parseMCVersion str of
        Just v  -> Right v
        Nothing -> Left (printf "Unrecognisable minecraft version '%s'." str)
