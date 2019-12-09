//from "sdl_stdinc.h"

  function SDL_malloc (size: size_t): Pointer; cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_malloc' {$ENDIF} {$ENDIF};

  function SDL_calloc (nmemb, size: size_t): Pointer; cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_calloc' {$ENDIF} {$ENDIF};

  function SDL_realloc (mem: Pointer; size: size_t): Pointer; cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_realloc' {$ENDIF} {$ENDIF};

  procedure SDL_free (mem: Pointer); cdecl; external SDL_LibName {$IFDEF DELPHI} {$IFDEF MACOS} name '_SDL_free' {$ENDIF} {$ENDIF};
