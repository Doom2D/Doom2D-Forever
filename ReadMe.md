# About
A recreation of the classic [Doom2D](https://doomwiki.org/wiki/Doom_2D) game from the late DOS era, a simulacrum of a parody - just as it should be.

This open-source, multiplatform project has been developed from the ground up to recreate the original game while significantly enhancing gameplay, graphics, and introducing several new gamemodes. It features support for high-resolution graphics, improved AI, numerous fixes, and most importantly **online multiplayer**, revitalizing one of the biggest cult classics of ex-USSR.
<p align="center">
    <img src="https://doom2d.org/doom2d_forever/about/DFsml.jpg" alt="Doom2D Forever, SuperDM, 640x480.">
</p>

# Download and install
Pre-built binaries of the latest commit (snapshot) are currently available for the following operating systems:
- **Windows**: Download a ZIP-archive, either [Windows x64 (64-bit)](https://github.com/Doom2D/nix_actions/releases/latest/download/doom2df_win64.x86-64.zip) or [**Windows x86 (32-bit)**](https://github.com/Doom2D/nix_actions/releases/latest/download/doom2df_windows.x86.zip). Windows XP and newer is supported.
- **Android**: Download the [**Android APK**](https://github.com/Doom2D/nix_actions/releases/latest/download/Doom2D-Forever.apk). Android 4.0.1 and higher is supported.
- **macOS**: Download the [**macOS App**](https://github.com/Doom2D/nix_actions/releases/latest/download/Doom2D-Forever.dmg). Intel and Apple silicon Mac computers running macOS >= 11.0 are supported.

# Disclaimer
This game is a fair-use parody, licensed _strictly_ under terms of the [**GNU General Public License v3.0**](https://github.com/Doom2D/Doom2D-Forever/blob/master/COPYING).

<a name="contacts"></a>
# Contacts
Join our [**Telegram chatroom**](https://t.me/doom2d) and [**Discord server**](https://discord.gg/sGpJwMy) to discuss the development of the project.

[![Telegram chatroom](https://img.shields.io/badge/Telegram-2CA5E0?style=flat-squeare&logo=telegram&logoColor=white)](https://t.me/doom2d)
[![Discord server](https://img.shields.io/badge/chat-Discord-8c9eff?logo=discord&logoColor=ffffff)](https://discord.gg/sGpJwMy)

# Contribution and Development
This repository is a place for everyone. If you want to contribute or need help, please join our [Telegram and Discord servers](#contacts).

## Requirements
For any possible build of Doom2D Forever you will need:

- FPC >= 3.1.1
- libenet >= 1.3.13

## Build

Please follow the instructions below to be able to build the project from source:

Clone the project, and change your current working directory to project root.

Create the `tmp` and `bin` directories and then run:

```shell
  cd src/game
  fpc -O1 -g -gl -dDISABLE_SOUND -dUSE_SYSTUB -dUSE_GLSTUB -FE../../bin -FU../../tmp Doom2DF.lpr
```

If that command succeeds, you have a working FPC and libenet installation.
## Compile-time switches
### System driver
|Switch          |What it does          |
|----------------|----------------------|
|`-dUSE_SDL2`    |Build with SDL 2.0.x  |
|`-dUSE_SDL`     |Build with SDL 1.2.x  |
|`-dUSE_SYSSTUB` |Disable I/O management|
### Render driver
|Switch         |What it does                  |
|---------------|------------------------------|
|`-dUSE_OPENGL` |Build with desktop OpenGL 2.x |
|`-dUSE_GLES1`  |Build with mobile OpenGLES 1.1|
|`-dUSE_GLSTUB` |Disable rendering             |
### Sound driver:
|Switch            |What it does                                         |
|------------------|-----------------------------------------------------|
|`-dUSE_FMOD`      |Build with FMOD Ex (4.30.22, other versions may fail)|
|`-dUSE_SDLMIXER`  |Build with SDL_mixer                                 |
|`-dUSE_OPENAL`    |Build with OpenAL 1.1                                |
|`-dUSE_SOUNDSTUB` |Build with sound stub                                |
|`-dDISABLE_SOUND` |Disable sound management                             |
### OpenAL only
|Switch                      |What it does                        |
|----------------------------|------------------------------------|
|`-dUSE_SDL2`                |Build with SDL 2.0.x for WAV support|
|`-dUSE_SDL`                 |Build with SDL 1.2.x for WAV support|
|`-dUSE_VORBIS`              |Build with libvorbis                |
|`-dUSE_FLUIDSYNTH`          |Build with libfluidsynth            |
|`-dUSE_MODPLUG`             |Build with libmodplug               |
|`-dUSE_XMP`                 |Build with linxmp                   |
|`-dUSE_MPG123`              |Build with libmpg123                |
|`-dUSE_OPUS`                |Build with libopus                  |
|`-dUSE_GME`                 |Build with libgme                   |
|`-dOPENAL_SINGLETHREADED`   |Don't use threads for OpenAL        |
### Other
|Switch            |What it does                                                         |
|------------------|---------------------------------------------------------------------|
|`-dSDL2_NODPI`    |Build for old libSDL2                                                |
|`-dUSE_MINIUPNPC` |Build with libminiupnpc for automatic server port forwarding via UPnP|
|`-dENABLE_HOLMES` |Build with in-game map debugger                                      |
|`-dHEADLESS`      |Build as headless executable for dedicated servers                   |

## Runtime development flags
- `--gdb`: prevents the game from handling exceptions.

## Platform specific
### OpenGLES
Currently, OpenGLES depends uncoditionally on SDL2.
### Android
Currently, Android support depends unconditionally on SDL2.
### Windows
Windows binaries will require the appropriate DLLs (SDL2.dll, SDL2_mixer.dll or
FMODex.dll, ENet.dll, miniupnpc.dll) unless you choose to link them statically.

#### Static Linking
It's now possible to link Windows' LibJIT and ENet as static libs.

|Switch                          |What it does                               |
|--------------------------------|-------------------------------------------|
|`-dLIBJIT_WINDOZE_STATIC`       |Static LibJIT                              |
|`-dLIBENET_WINDOZE_STATIC`      |Static ENet                                |
|`-dLIBMINIUPNPC_WINDOZE_STATIC` |Static MiniUPnPC                           |
|`-dVORBIS_WINDOZE_STATIC`       |Static libogg/libvorbis (only in AL builds)|
|`-dOPUS_WINDOZE_STATIC`         |Static libogg/libopus (only in AL builds)  |

