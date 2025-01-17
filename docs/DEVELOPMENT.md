# [**Doom2D Forever**](../README.md) build and contribution guide

## Requirements
For any possible build of Doom2D Forever you will need:

- FPC >= 3.1.1
- libenet >= 1.3.13

## Build

Please follow the instructions below to be able to build the project from source:

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
|`-dHEADLESS`      |Build a headless executable for dedicated servers                    |

Run the game with --gdb when using a debugger to prevent it from eating

## Runtime flags
- `--gdb`: prevents the game from eating exceptions.

## Platform specific
### OpenGLES
Currently, using OpenGLES depends uncoditionally on SDL2.
### Android
Currently, Android depends unconditionally on SDL2. 