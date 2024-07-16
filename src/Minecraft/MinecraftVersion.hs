module Minecraft.MinecraftVersion (MinecraftVersion(..)) where

import           Data.Text        (unpack)
import           Data.Yaml        (FromJSON (..), Value (..))
import           Text.Regex.Posix ((=~))

data MinecraftVersion = MinecraftVersion Int Int Int

instance Show MinecraftVersion where
    show (MinecraftVersion major minor patch) =
        show major ++ "." ++ show minor ++ "." ++ show patch

instance FromJSON MinecraftVersion where
    parseJSON (String txt) = let str = unpack txt in
        if str =~ "[0-9]+\\.[0-9]+\\.[0-9]+$" then
            let (major, str')  = takeWhileAndRemains (/= '.') str
                (minor, patch) = takeWhileAndRemains (/= '.') str'
            in
            return (MinecraftVersion (read major) (read minor) (read patch))
        else
            fail "Unrecognisable minecraft version"
        where
            takeWhileAndRemains f s =
                let taken = takeWhile f s in
                    (taken, drop (length taken + 1) s)

    parseJSON _ = fail "Unrecognisable minecraft version"
