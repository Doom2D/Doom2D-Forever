unit e_sound;

{$IFDEF USE_SDLMIXER}
  {$I e_sound_sdl.inc}
{$ELSE}
  {$I e_sound_fmod.inc}
{$ENDIF}
