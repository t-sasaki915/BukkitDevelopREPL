# spigot-debugger-launcher
Minecraft client and server launcher for Spigot plugin debugging

## Requirements
- Windows 10 or 11 (Windows 11 is recommended, because I have been testing this program with Windows 11)
- Java (17 or higher)
- Git for Windows (If you are going to build a Spigot server with this program)

## Usage
__**WARNING**__ This program is NOT for production. Please use this program on purpose of brief debugging.
```
spigot-debugger-launcher-exe.exe [Options] [Plugin Paths to Instal]
```
Both `[Options]` and `[Plugin Paths to Instal]` could be non-specified.
That is, double-clicking the `.EXE` file is the easiest usage.

### Options
| Option | Default Value | Description |
| :----: | :----: | ---- |
| `--work-dir FilePath` | `.\run` | Specifies working directory expressly. |
| `--minecraft-dir FilePath` | `~\AppData\Roaming\.minecraft` | Specifies Minecraft directory expressly. |
| `--mc-client-username String` | `DEV` | Customises Minecraft client username. |
| `--mc-client-xms Int` | `2` | Customises Minecraft client JVM Xms. The unit is gigabytes. |
| `--mc-client-xmx Int` | `2` | Customises Minecraft client JVM Xmx. The unit is gigabytes. |
| `--mc-server-xms Int` | `2` | Customises Minecraft server JVM Xms. The unit is gigabytes. |
| `--mc-server-xmx Int` | `2` | Customises Minecraft server JVM Xmx. The unit is gigabytes. |

### Flags
| Flag | Description |
| :----: | ---- |
| `--help` | Prints the usage of this program. |
| `--no-client` | Tell this program that Minecraft client is unwanted. That is, server-only mode. |

## Examples of utilisation
__**Note**__ You need a sbt plugin `sbt-assembly`. 
1. Download this program and place `spigot-debugger-launcher-exe.exe` to the project root.
2. Add this script to the bottom of `build.sbt`.
   ```scala
   lazy val spigotDebug = taskKey[Unit]("execute spigot-debugger-launcher")

   spigotDebug := {
     import scala.sys.process._

     assembly.value

     /**
      * Path to spigot-debugger-launcher-exe.exe. The current directory is the project root.
      * If your spigot-debugger-launcher-exe.exe is not located at the project root, you have to adjust this.
      * Examples:
      *   val launcher = ".\\debugger.exe"
      *   val launcher = ".\\bin\\spigot-debugger-launcher-exe.exe"
      */
     val launcher = ".\\spigot-debugger-launcher-exe.exe"

     /**
      * Command line options for spigot-debugger-launcher-exe.exe.
      * Examples:
      *   val options = List("--no-client")
      *   val options = List("--mc-server-xms 8", "--mc-server-xmx 8")
      *   val options = List("--work-dir C:\\Temp")
      */
     val options = List()

     /**
      * Plugin paths to instal on the Spigot server.
      * Examples:
      *   val plugins = List(".\\dependencies\\worldedit.jar", ".\\dependencies\\placeholderapi.jar")
      */
     val plugins = List()
  
     s"cmd /c start $launcher ${options.mkString(" ")} ${(assembly / assemblyOutputPath).value} ${plugins.mkString(" ")}" !
   }
   ```
3. Gitignore `spigot-debugger-launcher-exe.exe` and `run/`
4. Execute `sbt spigotDebug`
