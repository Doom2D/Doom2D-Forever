unit f_mapcheck;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, g_items, g_basic, g_monsters, g_areas;

type
  TMapCheckForm = class(TForm)
    Panel1: TPanel;
    lbErrorList: TListBox;
    bClose: TButton;
    bCheckMap: TButton;
    mErrorDescription: TMemo;
    procedure bCloseClick(Sender: TObject);
    procedure bCheckMapClick(Sender: TObject);
    procedure lbErrorListClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MapCheckForm: TMapCheckForm;
  ErrorsNum:    Array of Byte;

implementation

uses f_main;

{$R *.dfm}

procedure TMapCheckForm.bCloseClick(Sender: TObject);
begin
 Close;
end;

procedure TMapCheckForm.bCheckMapClick(Sender: TObject);
var
  a: Integer;
  b: Integer;
begin
 lbErrorList.Clear;
 mErrorDescription.Clear;
 ErrorsNum := nil;

 if MainLevel.AreaSystem.AreasArray <> nil then
 for a := 0 to High(MainLevel.AreaSystem.AreasArray) do
 begin
  if MainLevel.AreaSystem.AreasArray[a].AreaType <> AREA_NONE then
  with MainLevel.AreaSystem.AreasArray[a] do
  begin
   if g_CollideLevel(GameX+Size.Left, GameY+Size.Top, Size.Right-Size.Left, Size.Bottom-Size.Top) then
   begin
    lbErrorList.Items.Add('[Error] ќбласть '+IntToStr(a)+' пересекаетс€ с картой');
    SetLength(ErrorsNum, Length(ErrorsNum)+1);
    ErrorsNum[High(ErrorsNum)] := 1;
   end;
  end;
 end;

 b := 0;
 if MainLevel.AreaSystem.AreasArray <> nil then
 for a := 0 to High(MainLevel.AreaSystem.AreasArray) do
  if MainLevel.AreaSystem.AreasArray[a].AreaType = AREA_PLAYER1 then Inc(b);

 if b = 0 then
 begin
  lbErrorList.Items.Add('[Error] Ќа карте нет точки по€влени€ первого игрока');
  SetLength(ErrorsNum, Length(ErrorsNum)+1);
  ErrorsNum[High(ErrorsNum)] := 2;
 end
  else if b > 1 then
 begin
  lbErrorList.Items.Add('[Hint] Ќафига вам несколько точек по€влени€ первого игрока ?');
  SetLength(ErrorsNum, Length(ErrorsNum)+1);
  ErrorsNum[High(ErrorsNum)] := 3;
 end;

 b := 0;
 if MainLevel.AreaSystem.AreasArray <> nil then
 for a := 0 to High(MainLevel.AreaSystem.AreasArray) do
  if MainLevel.AreaSystem.AreasArray[a].AreaType = AREA_PLAYER2 then Inc(b);

 if b = 0 then
 begin
  lbErrorList.Items.Add('[Error] Ќа карте нет точки по€влени€ второго игрока');
  SetLength(ErrorsNum, Length(ErrorsNum)+1);
  ErrorsNum[High(ErrorsNum)] := 4;
 end
  else if b > 1 then
 begin
  lbErrorList.Items.Add('[Hint] Ќафига вам несколько точек по€влени€ второго игрока ?');
  SetLength(ErrorsNum, Length(ErrorsNum)+1);
  ErrorsNum[High(ErrorsNum)] := 5;
 end;

 b := 0;
 if MainLevel.AreaSystem.AreasArray <> nil then
 for a := 0 to High(MainLevel.AreaSystem.AreasArray) do
  if MainLevel.AreaSystem.AreasArray[a].AreaType = AREA_DMPOINT then Inc(b);

 if b = 0 then
 begin
  lbErrorList.Items.Add('[Error] Ќа карте нет точек DM');
  SetLength(ErrorsNum, Length(ErrorsNum)+1);
  ErrorsNum[High(ErrorsNum)] := 6;
 end;

 if MainLevel.MonsterSystem.MonstersArray <> nil then
 for a := 0 to High(MainLevel.MonsterSystem.MonstersArray) do
 if MainLevel.MonsterSystem.MonstersArray[a].MonsterType <> MONSTER_NONE then
 with MainLevel.MonsterSystem.MonstersArray[a] do
 begin
  if g_CollideLevel(GameX+Size.Left, GameY+Size.Top, Size.Right-Size.Left, Size.Bottom-Size.Top) then
  begin
   lbErrorList.Items.Add('[Error] ћонстр '+IntToStr(a)+' пересекаетс€ с картой');
   SetLength(ErrorsNum, Length(ErrorsNum)+1);
   ErrorsNum[High(ErrorsNum)] := 7;
  end;
 end;
end;

procedure TMapCheckForm.lbErrorListClick(Sender: TObject);
begin
 if lbErrorList.ItemIndex <> -1 then
 case ErrorsNum[lbErrorList.ItemIndex] of
  1: mErrorDescription.Text := 'ќбласть пересекаетс€ с картой, если в этой точке по€витс€ игрок, то он застр€нет в стене и не сможет двигатьс€';
  2: mErrorDescription.Text := 'Ќа карте нет точки по€влени€ первого игрока, не получитс€ поиграть в эту карту в режиме "Single Player"';
  3, 5: mErrorDescription.Text := '«ачем несколько точек по€влени€, игрок все равно будет по€вл€тьс€ только на первой точке';
  4: mErrorDescription.Text := 'Ќа карте нет точки по€влени€ второго игрока, не получитс€ поиграть в эту карту в режиме "Single Player"';
  6: mErrorDescription.Text := 'Ќа карте нет точек DM, поиграть получитс€ только в режиме "Single Player"';
  7: mErrorDescription.Text := 'ћонстр пересекаетс€ с картой, он застр€нет в стене и не сможет двигатьс€';
 end;
end;

end.
