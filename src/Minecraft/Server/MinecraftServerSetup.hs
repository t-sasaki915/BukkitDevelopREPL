module Minecraft.Server.MinecraftServerSetup (setupMinecraftServer) where

import           AppState

import           Minecraft.Server.Paper.PaperSetup   (setupPaper)
import           Minecraft.Server.ServerBrand        (ServerBrand (..))
import           Minecraft.Server.Spigot.SpigotSetup (setupSpigot)

setupMinecraftServer :: AppStateIO ()
setupMinecraftServer = do
    serverBrand <- getServerBrand

    case serverBrand of
        Spigot -> setupSpigot
        Paper  -> setupPaper
