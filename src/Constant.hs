{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Constant where

import System.FilePath ((</>))

type JVMOption = String

buildToolsUrl :: FilePath
buildToolsUrl = "https://hub.spigotmc.org/jenkins/job/BuildTools/lastSuccessfulBuild/artifact/target/BuildTools.jar"

minecraftClientJVMOptions :: [JVMOption]
minecraftClientJVMOptions =
    [ "-XX:+UnlockExperimentalVMOptions"
    , "-XX:+UseG1GC"
    , "-XX:G1NewSizePercent=20"
    , "-XX:G1ReservePercent=20"
    , "-XX:MaxGCPauseMillis=50"
    , "-XX:G1HeapRegionSize=32M"
    ]

minecraftVersion :: String
minecraftVersion = "1.20.1"

minecraftAssetIndex :: Int
minecraftAssetIndex = 5

minecraftClientLibraries :: [FilePath]
minecraftClientLibraries =
    [ "com" </> "github" </> "oshi" </> "oshi-core" </> "6.2.2" </> "oshi-core-6.2.2.jar"
    , "com" </> "google" </> "code" </> "gson" </> "gson" </> "2.10" </> "gson-2.10.jar"
    , "com" </> "google" </> "guava" </> "failureaccess" </> "1.0.1" </> "failureaccess-1.0.1.jar"
    , "com" </> "google" </> "guava" </> "guava" </> "31.1-jre" </> "guava-31.1-jre.jar"
    , "com" </> "ibm" </> "icu" </> "icu4j" </> "71.1" </> "icu4j-71.1.jar"
    , "com" </> "mojang" </> "authlib" </> "4.0.43" </> "authlib-4.0.43.jar"
    , "com" </> "mojang" </> "blocklist" </> "1.0.10" </> "blocklist-1.0.10.jar"
    , "com" </> "mojang" </> "brigadier" </> "1.1.8" </> "brigadier-1.1.8.jar"
    , "com" </> "mojang" </> "datafixerupper" </> "6.0.8" </> "datafixerupper-6.0.8.jar"
    , "com" </> "mojang" </> "logging" </> "1.1.1" </> "logging-1.1.1.jar"
    , "com" </> "mojang" </> "patchy" </> "2.2.10" </> "patchy-2.2.10.jar"
    , "com" </> "mojang" </> "text2speech" </> "1.17.9" </> "text2speech-1.17.9.jar"
    , "commons-codec" </> "commons-codec" </> "1.15" </> "commons-codec-1.15.jar"
    , "commons-io" </> "commons-io" </> "2.11.0" </> "commons-io-2.11.0.jar"
    , "commons-logging" </> "commons-logging" </> "1.2" </> "commons-logging-1.2.jar"
    , "io" </> "netty" </> "netty-buffer" </> "4.1.82.Final" </> "netty-buffer-4.1.82.Final.jar"
    , "io" </> "netty" </> "netty-codec" </> "4.1.82.Final" </> "netty-codec-4.1.82.Final.jar"
    , "io" </> "netty" </> "netty-common" </> "4.1.82.Final" </> "netty-common-4.1.82.Final.jar"
    , "io" </> "netty" </> "netty-handler" </> "4.1.82.Final" </> "netty-handler-4.1.82.Final.jar"
    , "io" </> "netty" </> "netty-resolver" </> "4.1.82.Final" </> "netty-resolver-4.1.82.Final.jar"
    , "io" </> "netty" </> "netty-transport-classes-epoll" </> "4.1.82.Final" </> "netty-transport-classes-epoll-4.1.82.Final.jar"
    , "io" </> "netty" </> "netty-transport-native-unix-common" </> "4.1.82.Final" </> "netty-transport-native-unix-common-4.1.82.Final.jar"
    , "io" </> "netty" </> "netty-transport" </> "4.1.82.Final" </> "netty-transport-4.1.82.Final.jar"
    , "it" </> "unimi" </> "dsi" </> "fastutil" </> "8.5.9" </> "fastutil-8.5.9.jar"
    , "net" </> "java" </> "dev" </> "jna" </> "jna-platform" </> "5.12.1" </> "jna-platform-5.12.1.jar"
    , "net" </> "java" </> "dev" </> "jna" </> "jna" </> "5.12.1" </> "jna-5.12.1.jar"
    , "net" </> "sf" </> "jopt-simple" </> "jopt-simple" </> "5.0.4" </> "jopt-simple-5.0.4.jar"
    , "org" </> "apache" </> "commons" </> "commons-compress" </> "1.21" </> "commons-compress-1.21.jar"
    , "org" </> "apache" </> "commons" </> "commons-lang3" </> "3.12.0" </> "commons-lang3-3.12.0.jar"
    , "org" </> "apache" </> "httpcomponents" </> "httpclient" </> "4.5.13" </> "httpclient-4.5.13.jar"
    , "org" </> "apache" </> "httpcomponents" </> "httpcore" </> "4.4.15" </> "httpcore-4.4.15.jar"
    , "org" </> "apache" </> "logging" </> "log4j" </> "log4j-api" </> "2.19.0" </> "log4j-api-2.19.0.jar"
    , "org" </> "apache" </> "logging" </> "log4j" </> "log4j-core" </> "2.19.0" </> "log4j-core-2.19.0.jar"
    , "org" </> "apache" </> "logging" </> "log4j" </> "log4j-slf4j2-impl" </> "2.19.0" </> "log4j-slf4j2-impl-2.19.0.jar"
    , "org" </> "joml" </> "joml" </> "1.10.5" </> "joml-1.10.5.jar"
    , "org" </> "lwjgl" </> "lwjgl-glfw" </> "3.3.1" </> "lwjgl-glfw-3.3.1.jar"
    , "org" </> "lwjgl" </> "lwjgl-glfw" </> "3.3.1" </> "lwjgl-glfw-3.3.1-natives-windows.jar"
    , "org" </> "lwjgl" </> "lwjgl-glfw" </> "3.3.1" </> "lwjgl-glfw-3.3.1-natives-windows-arm64.jar"
    , "org" </> "lwjgl" </> "lwjgl-glfw" </> "3.3.1" </> "lwjgl-glfw-3.3.1-natives-windows-x86.jar"
    , "org" </> "lwjgl" </> "lwjgl-jemalloc" </> "3.3.1" </> "lwjgl-jemalloc-3.3.1.jar"
    , "org" </> "lwjgl" </> "lwjgl-jemalloc" </> "3.3.1" </> "lwjgl-jemalloc-3.3.1-natives-windows.jar"
    , "org" </> "lwjgl" </> "lwjgl-jemalloc" </> "3.3.1" </> "lwjgl-jemalloc-3.3.1-natives-windows-arm64.jar"
    , "org" </> "lwjgl" </> "lwjgl-jemalloc" </> "3.3.1" </> "lwjgl-jemalloc-3.3.1-natives-windows-x86.jar"
    , "org" </> "lwjgl" </> "lwjgl-openal" </> "3.3.1" </> "lwjgl-openal-3.3.1.jar"
    , "org" </> "lwjgl" </> "lwjgl-openal" </> "3.3.1" </> "lwjgl-openal-3.3.1-natives-windows.jar"
    , "org" </> "lwjgl" </> "lwjgl-openal" </> "3.3.1" </> "lwjgl-openal-3.3.1-natives-windows-arm64.jar"
    , "org" </> "lwjgl" </> "lwjgl-openal" </> "3.3.1" </> "lwjgl-openal-3.3.1-natives-windows-x86.jar"
    , "org" </> "lwjgl" </> "lwjgl-opengl" </> "3.3.1" </> "lwjgl-opengl-3.3.1.jar"
    , "org" </> "lwjgl" </> "lwjgl-opengl" </> "3.3.1" </> "lwjgl-opengl-3.3.1-natives-windows.jar"
    , "org" </> "lwjgl" </> "lwjgl-opengl" </> "3.3.1" </> "lwjgl-opengl-3.3.1-natives-windows-arm64.jar"
    , "org" </> "lwjgl" </> "lwjgl-opengl" </> "3.3.1" </> "lwjgl-opengl-3.3.1-natives-windows-x86.jar"
    , "org" </> "lwjgl" </> "lwjgl-stb" </> "3.3.1" </> "lwjgl-stb-3.3.1.jar"
    , "org" </> "lwjgl" </> "lwjgl-stb" </> "3.3.1" </> "lwjgl-stb-3.3.1-natives-windows.jar"
    , "org" </> "lwjgl" </> "lwjgl-stb" </> "3.3.1" </> "lwjgl-stb-3.3.1-natives-windows-arm64.jar"
    , "org" </> "lwjgl" </> "lwjgl-stb" </> "3.3.1" </> "lwjgl-stb-3.3.1-natives-windows-x86.jar"
    , "org" </> "lwjgl" </> "lwjgl-tinyfd" </> "3.3.1" </> "lwjgl-tinyfd-3.3.1.jar"
    , "org" </> "lwjgl" </> "lwjgl-tinyfd" </> "3.3.1" </> "lwjgl-tinyfd-3.3.1-natives-windows.jar"
    , "org" </> "lwjgl" </> "lwjgl-tinyfd" </> "3.3.1" </> "lwjgl-tinyfd-3.3.1-natives-windows-arm64.jar"
    , "org" </> "lwjgl" </> "lwjgl-tinyfd" </> "3.3.1" </> "lwjgl-tinyfd-3.3.1-natives-windows-x86.jar"
    , "org" </> "lwjgl" </> "lwjgl" </> "3.3.1" </> "lwjgl-3.3.1.jar"
    , "org" </> "lwjgl" </> "lwjgl" </> "3.3.1" </> "lwjgl-3.3.1-natives-windows.jar"
    , "org" </> "lwjgl" </> "lwjgl" </> "3.3.1" </> "lwjgl-3.3.1-natives-windows-arm64.jar"
    , "org" </> "lwjgl" </> "lwjgl" </> "3.3.1" </> "lwjgl-3.3.1-natives-windows-x86.jar"
    , "org" </> "slf4j" </> "slf4j-api" </> "2.0.1" </> "slf4j-api-2.0.1.jar"
    ]
