unit g_options;

interface

type
  TPlayerControl = record
   KeyRight:      Byte;
   KeyLeft:       Byte;
   KeyUp:         Byte;
   KeyDown:       Byte;
   KeyFire:       Byte;
   KeyJump:       Byte;
   KeyNextWeapon: Byte;
   KeyPrevWeapon: Byte;
   KeyOpen:       Byte;
  end;

  TGameControls = record
   TakeScreenshot: Byte;
   Stat: Byte;
  end;

  TControls = record
   GameControls: TGameControls;
   P1Control:    TPlayerControl;
   P2Control:    TPlayerControl;
  end;

procedure g_Options_SetDefault();
procedure g_Options_Read(FileName: string);

var
  gGameControls: TControls;
  gScreenWidth: Word   = 800;
  gScreenHeight: Word  = 600;
  gBPP: Byte           = 16;
  gFreq: Byte          = 0;
  gFullscreen: Boolean = True;
  gVSync: Boolean      = False;
  gTextureFilter: Boolean = True;
  gSoundLevel: Byte    = 255;
  gMusicLevel: Byte    = 255;
  gAdvCorpses: Boolean = True;
  gAdvBlood: Boolean   = True;
  gAdvGibs: Boolean    = True;
  gGibsCount: Integer  = 32;
  gBloodCount: Byte    = 3;
  gFlash: Boolean      = True;
  gDrawBackGround: Boolean = True;
  gShowMessages: Boolean = True;
  gRevertPlayers: Boolean = False;

implementation

uses e_log, g_window, g_sound, g_gfx, g_player, Math, g_map, SysUtils, CONFIG,
     g_game, g_main, e_textures, dglOpenGL;

procedure g_Options_SetDefault();
begin
 gScreenWidth := 800;
 gScreenHeight := 600;
 gFullScreen := True;
 gBPP := 16;
 gVSync := False;
 gTextureFilter := True;

 gSoundLevel := 255;
 gMusicLevel := 255;

 g_GFX_SetMax(2000);
 g_Gibs_SetMax(150);
 g_Corpses_SetMax(20);
 gGibsCount := 32;
 gBloodCount := 3;
 gAdvBlood := True;
 gAdvCorpses := True;
 gAdvGibs := True;
 gFlash := True;
 gDrawBackGround := True;
 gShowMessages := True;
 gRevertPlayers := False;

 with gGameControls.GameControls do
 begin
  TakeScreenshot := 183;
  Stat := 15;
 end;

 with gGameControls.P1Control do
 begin
  KeyRight := 77;
  KeyLeft := 75;
  KeyUp := 72;
  KeyDown := 76;
  KeyFire := 184;
  KeyJump := 157;
  KeyNextWeapon := 73;
  KeyPrevWeapon := 71;
  KeyOpen := 54;
 end;

 with gGameControls.P2Control do
 begin
  KeyRight := 33;
  KeyLeft := 31;
  KeyUp := 18;
  KeyDown := 32;
  KeyFire := 30;
  KeyJump := 16;
  KeyNextWeapon := 19;
  KeyPrevWeapon := 17;
  KeyOpen := 58;
 end;

 with gPlayer1Settings do
 begin
  Name := 'Player1';
  Model := 'Doomer';
  Color.R := PLAYER1_DEF_COLOR.R;
  Color.G := PLAYER1_DEF_COLOR.G;
  Color.B := PLAYER1_DEF_COLOR.B;
  Team := TEAM_RED;
 end;

 with gPlayer2Settings do
 begin
  Name := 'Player2';
  Model := 'Doomer';
  Color.R := PLAYER2_DEF_COLOR.R;
  Color.G := PLAYER2_DEF_COLOR.G;
  Color.B := PLAYER2_DEF_COLOR.B;
  Team := TEAM_BLUE;
 end;
end;

procedure g_Options_Read(FileName: string);
var
  config: TConfig;
begin
 if not FileExists(FileName) then
 begin
  e_WriteLog('config file '+FileName+' not founded', MSG_WARNING);
  g_Options_SetDefault();
  Exit;
 end;

 config := TConfig.CreateFile(FileName);

 gScreenWidth := config.ReadInt('Video', 'ScreenWidth', 800);
 gScreenHeight := config.ReadInt('Video', 'ScreenHeight', 600);
 gFullScreen := config.ReadBool('Video', 'Fullscreen', True);
 gBPP := config.ReadInt('Video', 'BPP', 16);
 gFreq := config.ReadInt('Video', 'Freq', 0);
 gVSync := config.ReadBool('Video', 'VSync', True);
 gTextureFilter := config.ReadBool('Video', 'TextureFilter', True);

 gSoundLevel := Min(config.ReadInt('Sound', 'SoundLevel', 255), 255);
 gMusicLevel := Min(config.ReadInt('Sound', 'MusicLevel', 255), 255);

 with gGameControls.GameControls do
 begin
  TakeScreenshot := config.ReadInt('GameControls', 'TakeScreenshot', 183);
  Stat := config.ReadInt('GameControls', 'Stat', 15)
 end;

 with gGameControls.P1Control, config do
 begin
  KeyRight := ReadInt('Player1', 'KeyRight', 33);
  KeyLeft := ReadInt('Player1', 'KeyLeft', 31);
  KeyUp := ReadInt('Player1', 'KeyUp', 18);
  KeyDown := ReadInt('Player1', 'KeyDown', 32);
  KeyFire := ReadInt('Player1', 'KeyFire', 30);
  KeyJump := ReadInt('Player1', 'KeyJump', 16);
  KeyNextWeapon := ReadInt('Player1', 'KeyNextWeapon', 19);
  KeyPrevWeapon := ReadInt('Player1', 'KeyPrevWeapon', 17);
  KeyOpen := ReadInt('Player1', 'KeyOpen', 58);
 end;

 with gPlayer1Settings, config do
 begin
  Name := ReadStr('Player1', 'name', 'Player1');
  Model := ReadStr('Player1', 'model', 'Doomer');
  Color.R := Min(Abs(ReadInt('Player1', 'red', PLAYER1_DEF_COLOR.R)), 255);
  Color.G := Min(Abs(ReadInt('Player1', 'green', PLAYER1_DEF_COLOR.G)), 255);
  Color.B := Min(Abs(ReadInt('Player1', 'blue', PLAYER1_DEF_COLOR.B)), 255);
  Team := ReadInt('Player1', 'team', TEAM_RED);
  if (Team < TEAM_RED) or (Team > TEAM_BLUE) then Team := TEAM_RED;
 end;

 with gGameControls.P2Control, config do
 begin
  KeyRight := ReadInt('Player2', 'KeyRight', 205);
  KeyLeft := ReadInt('Player2', 'KeyLeft', 203);
  KeyUp := ReadInt('Player2', 'KeyUp', 200);
  KeyDown := ReadInt('Player2', 'KeyDown', 208);
  KeyFire := ReadInt('Player2', 'KeyFire', 184);
  KeyJump := ReadInt('Player2', 'KeyJump', 157);
  KeyNextWeapon := ReadInt('Player2', 'KeyNextWeapon', 73);
  KeyPrevWeapon := ReadInt('Player2', 'KeyPrevWeapon', 71);
  KeyOpen := ReadInt('Player2', 'KeyOpen', 54);
 end;

 with gPlayer2Settings, config do
 begin
  Name := ReadStr('Player2', 'name', 'Player2');
  Model := ReadStr('Player2', 'model', 'Doomer');
  Color.R := Min(Abs(ReadInt('Player2', 'red', PLAYER2_DEF_COLOR.R)), 255);
  Color.G := Min(Abs(ReadInt('Player2', 'green', PLAYER2_DEF_COLOR.G)), 255);
  Color.B := Min(Abs(ReadInt('Player2', 'blue', PLAYER2_DEF_COLOR.B)), 255);
  Team := ReadInt('Player2', 'team', TEAM_BLUE);
  if (Team < TEAM_RED) or (Team > TEAM_BLUE) then Team := TEAM_RED;
 end;

 g_GFX_SetMax(Min(config.ReadInt('Game', 'MaxParticles', 1000), 50000));
 g_Gibs_SetMax(Min(config.ReadInt('Game', 'MaxGibs', 150), 500));
 g_Corpses_SetMax(Min(config.ReadInt('Game', 'MaxCorpses', 20), 100));

 case config.ReadInt('Game', 'GibsCount', 3) of
  0: gGibsCount := 0;
  1: gGibsCount := 8;
  2: gGibsCount := 16;
  3: gGibsCount := 32;
  else gGibsCount := 48;
 end;

 gBloodCount := Min(config.ReadInt('Game', 'BloodCount', 4), 4);
 gAdvBlood := config.ReadBool('Game', 'AdvancesBlood', True);
 gAdvCorpses := config.ReadBool('Game', 'AdvancesCorpses', True);
 gAdvGibs := config.ReadBool('Game', 'AdvancesGibs', True);
 gFlash := config.ReadBool('Game', 'Flash', True);
 gDrawBackGround := config.ReadBool('Game', 'BackGround', True);
 gShowMessages := config.ReadBool('Game', 'Messages', True);
 gRevertPlayers := config.ReadBool('Game', 'RevertPlayers', False);

 config.Destroy;

 if gTextureFilter then TEXTUREFILTER := GL_LINEAR else TEXTUREFILTER := GL_NEAREST;
end;

end.
