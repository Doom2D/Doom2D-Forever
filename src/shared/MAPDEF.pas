(* Copyright (C)  Doom 2D: Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3 of the License ONLY.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)
{$INCLUDE a_modes.inc}
{$M+}
unit MAPDEF;

interface

uses
  xdynrec;


const
  MAP_SIGNATURE = 'MAP';


const
  TEXTURE_NAME_WATER = '_water_0';
  TEXTURE_NAME_ACID1 = '_water_1';
  TEXTURE_NAME_ACID2 = '_water_2';


type
  TDFPoint = packed record
  public
    X, Y: LongInt;

  public
    constructor Create (ax, ay: LongInt);

    function isZero (): Boolean; inline;
  end;

  TDFSize = packed record
  public
    w, h: LongInt;

  public
    constructor Create (aw, ah: LongInt);

    function isZero (): Boolean; inline;
    function isValid (): Boolean; inline;
  end;

  TDFColor = packed record
  public
    r, g, b, a: Byte; // a: 0 is transparent, 255 is opaque

  public
    constructor Create (ar, ag, ab: LongInt; aa: LongInt=0);

    function isTransparent (): Boolean; inline;
    function isOpaque (): Boolean; inline;
    function isBlack (): Boolean; inline;
    function isWhite (): Boolean; inline;
  end;

{$INCLUDE mapdef.inc}

// various helpers to access map structures
type
  TDynFieldHelper = class helper for TDynField
  public
    function getRGBA (): TDFColor; inline;
    procedure setRGBA (const v: TDFColor); inline;

  public
    property rgba: TDFColor read getRGBA write setRGBA; // for `TColor`
  end;

  TDynRecordHelper = class helper for TDynRecord
  private
    function getFieldWithType (const aname: AnsiString; atype: TDynField.TType): TDynField; inline;

    function getPanelByIdx (idx: Integer): TDynRecord; inline;

    function getTexturePanel (): Integer; inline;
    function getTexturePanelRec (): TDynRecord; inline;

    function getPanelIndex (pan: TDynRecord): Integer;

    function getPointField (const aname: AnsiString): TDFPoint; inline;
    function getSizeField (const aname: AnsiString): TDFSize; inline;

  public
    function panelCount (): Integer; inline;

    // header
    function mapName (): AnsiString; inline;
    function mapAuthor (): AnsiString; inline;
    function mapDesc (): AnsiString; inline;
    function musicName (): AnsiString; inline;
    function skyName (): AnsiString; inline;

    // panel
    function X (): Integer; inline;
    function Y (): Integer; inline;
    function Width (): Word; inline;
    function Height (): Word; inline;
    function TextureNum (): Word; inline;
    function TextureRec (): TDynRecord; inline;
    function PanelType (): Word; inline;
    function Alpha (): Byte; inline;
    function Flags (): Byte; inline;

    function moveSpeed (): TDFPoint; inline;
    function moveStart (): TDFPoint; inline;
    function moveEnd (): TDFPoint; inline;

    function moveOnce (): Boolean; inline;

    function sizeSpeed (): TDFSize; inline;
    function sizeEnd (): TDFSize; inline;

    function endPosTrig (): Integer; inline;
    function endSizeTrig (): Integer; inline;

    // texture
    function Resource (): AnsiString; inline;
    function Anim (): Boolean; inline;

    // item
    function ItemType (): Byte; inline;
    function Options (): Byte; inline;

    // monster
    function MonsterType (): Byte; inline; // type, ubyte
    function Direction (): Byte; inline; // direction, ubyte

    // area
    function AreaType (): Byte; inline; // type, ubyte
    //function Direction (): Byte; inline; // direction, ubyte

    // trigger
    function trigRec (): TDynRecord; {inline;}
    function Enabled (): Boolean; inline; // enabled, bool
    function TriggerType (): Byte; inline; // type, ubyte
    function ActivateType (): Byte; inline; // activatetype, ubyte
    function Keys (): Byte; inline; // keys, ubyte
    //function DATA (): Byte128; inline; // triggerdata, trigdata[128]; // the only special nested structure

    {$INCLUDE mapdef_help.inc}
    function trigMonsterId (): Integer; inline;
    function trigPanelId (): Integer; inline; // panel index in list
    function trigPanelRec (): TDynRecord; inline;

  private
    // user fields
    function getUserPanelId (): Integer; inline;
    procedure setUserPanelId (v: Integer); inline;

    function getUserTrigRef (): Boolean; inline;
    procedure setUserTrigRef (v: Boolean); inline;

  public
    property panel[idx: Integer]: TDynRecord read getPanelByIdx;
    property panelIndex[pan: TDynRecord]: Integer read getPanelIndex;
    // triggers
    property tgPanelId: Integer read trigPanelId;
    property tgPanelRec: TDynRecord read trigPanelRec;
    property TexturePanelId: Integer read getTexturePanel; // texturepanel, int
    property TexturePanelRec: TDynRecord read getTexturePanelRec;
    // user fields
    property userPanelId: Integer read getUserPanelId write setUserPanelId;
    property userPanelTrigRef: Boolean read getUserTrigRef write setUserTrigRef;
  end;

implementation

uses
  SysUtils, {e_log,} utils, xparser, xstreams;


// ////////////////////////////////////////////////////////////////////////// //
constructor TDFPoint.Create (ax, ay: LongInt); begin X := ax; Y := ay; end;
function TDFPoint.isZero (): Boolean; inline; begin result := (X = 0) and (Y = 0); end;


constructor TDFSize.Create (aw, ah: LongInt); begin w := aw; h := ah; end;
function TDFSize.isZero (): Boolean; inline; begin result := (w = 0) and (h = 0); end;
function TDFSize.isValid (): Boolean; inline; begin result := (w > 0) and (h > 0); end;

constructor TDFColor.Create (ar, ag, ab: LongInt; aa: LongInt=0);
begin
  if (ar < 0) then r := 0 else if (ar > 255) then r := 255 else r := Byte(ar);
  if (ag < 0) then g := 0 else if (ag > 255) then g := 255 else g := Byte(ag);
  if (ab < 0) then b := 0 else if (ab > 255) then b := 255 else b := Byte(ab);
  if (aa < 0) then a := 0 else if (aa > 255) then a := 255 else a := Byte(aa);
end;
function TDFColor.isTransparent (): Boolean; inline; begin result := (a = 0); end;
function TDFColor.isOpaque (): Boolean; inline; begin result := (a = 255); end;
function TDFColor.isBlack (): Boolean; inline; begin result := (r = 0) and (g = 0) and (b = 0); end;
function TDFColor.isWhite (): Boolean; inline; begin result := (r = 255) and (g = 255) and (b = 255); end;


// ////////////////////////////////////////////////////////////////////////// //
function TDynFieldHelper.getRGBA (): TDFColor; inline; begin result := TDFColor.Create(red, green, blue, alpha); end;
procedure TDynFieldHelper.setRGBA (const v: TDFColor); inline; begin red := v.r; green := v.g; blue := v.b; alpha := v.a; end;


// ////////////////////////////////////////////////////////////////////////// //
function TDynRecordHelper.getUserPanelId (): Integer; inline;
var
  fld: TDynField;
begin
  fld := field['userPanelId'];
  //if (fld = nil) or (fld.baseType <> TDynField.TType.TInt) then result := -1 else result := fld.ival;
  if (fld = nil) then result := -1 else result := Integer(fld.value);
end;


procedure TDynRecordHelper.setUserPanelId (v: Integer); inline;
begin
  user['userPanelId'] := v;
end;


function TDynRecordHelper.getUserTrigRef (): Boolean; inline;
var
  fld: TDynField;
begin
  fld := field['userPanelTrigRef'];
  if (fld = nil) then result := false else result := Boolean(fld.value);
  //if (fld = nil) or (fld.baseType <> TDynField.TType.TBool) then result := false else result := (fld.ival <> 0);
end;


procedure TDynRecordHelper.setUserTrigRef (v: Boolean); inline;
begin
  user['userPanelTrigRef'] := v;
end;


// ////////////////////////////////////////////////////////////////////////// //
function TDynRecordHelper.moveSpeed (): TDFPoint; inline; begin result := getPointField('move_speed'); end;
function TDynRecordHelper.moveStart (): TDFPoint; inline; begin result := getPointField('move_start'); end;
function TDynRecordHelper.moveEnd (): TDFPoint; inline; begin result := getPointField('move_end'); end;

function TDynRecordHelper.sizeSpeed (): TDFSize; inline; begin result := getSizeField('size_speed'); end;
function TDynRecordHelper.sizeEnd (): TDFSize; inline; begin result := getSizeField('size_end'); end;

function TDynRecordHelper.moveOnce (): Boolean; inline; begin result := (getFieldWithType('move_once', TDynField.TType.TBool).ival <> 0); end;


function TDynRecordHelper.endPosTrig (): Integer; inline;
var
  fld: TDynField;
begin
  fld := getFieldWithType('end_pos_trigger', TDynField.TType.TInt);
  result := fld.recrefIndex;
end;

function TDynRecordHelper.endSizeTrig (): Integer; inline;
var
  fld: TDynField;
begin
  fld := getFieldWithType('end_size_trigger', TDynField.TType.TInt);
  result := fld.recrefIndex;
end;


// ////////////////////////////////////////////////////////////////////////// //
function TDynRecordHelper.getFieldWithType (const aname: AnsiString; atype: TDynField.TType): TDynField; inline;
begin
  result := field[aname];
  if (result = nil) then raise Exception.Create(Format('field ''%s'' not found in record ''%s'' of type ''%s''', [aname, typeName, id]));
  if (result.baseType <> atype) then raise Exception.Create(Format('field ''%s'' in record ''%s'' of type ''%s'' has invalid data type', [aname, typeName, id]));
end;


function TDynRecordHelper.getPointField (const aname: AnsiString): TDFPoint; inline;
var
  fld: TDynField;
begin
  fld := field[aname];
  if (fld = nil) then raise Exception.Create(Format('field ''%s'' not found in record ''%s'' of type ''%s''', [aname, typeName, id]));
  if (fld.baseType <> fld.TType.TPoint) then raise Exception.Create(Format('field ''%s'' in record ''%s'' of type ''%s'' has invalid data type', [aname, typeName, id]));
  result := TDFPoint.Create(fld.ival, fld.ival2);
end;


function TDynRecordHelper.getSizeField (const aname: AnsiString): TDFSize; inline;
var
  fld: TDynField;
begin
  fld := field[aname];
  if (fld = nil) then raise Exception.Create(Format('field ''%s'' not found in record ''%s'' of type ''%s''', [aname, typeName, id]));
  if (fld.baseType <> fld.TType.TSize) and (fld.baseType <> fld.TType.TPoint) then raise Exception.Create(Format('field ''%s'' in record ''%s'' of type ''%s'' has invalid data type', [aname, typeName, id]));
  result := TDFSize.Create(fld.ival, fld.ival2);
end;


function TDynRecordHelper.getPanelByIdx (idx: Integer): TDynRecord; inline;
var
  fld: TDynField;
begin
  fld := headerRec['panel'];
  if (fld <> nil) then result := fld.itemAt[idx] else result := nil;
end;


function TDynRecordHelper.getPanelIndex (pan: TDynRecord): Integer;
var
  fld: TDynField;
  f: Integer;
begin
  result := -1;
  if (pan <> nil) then
  begin
    fld := headerRec['panel'];
    if (fld <> nil) then
    begin
      for f := 0 to fld.count-1 do if (fld.itemAt[f] = pan) then begin result := f; exit; end;
    end;
  end;
end;


function TDynRecordHelper.panelCount (): Integer; inline;
var
  fld: TDynField;
begin
  fld := headerRec['panel'];
  if (fld <> nil) then result := fld.count else result := 0;
end;


function TDynRecordHelper.TextureNum (): Word; inline;
var
  idx: Integer;
  fld: TDynField;
begin
  fld := getFieldWithType('texture', TDynField.TType.TUShort);
  idx := fld.recrefIndex;
  if (idx < 0) then result := Word(TEXTURE_NONE) else result := Word(idx);
end;


// ////////////////////////////////////////////////////////////////////////// //
// trigger
function TDynRecordHelper.trigRec (): TDynRecord; {inline;}
var
  fld: TDynField;
begin
  fld := getFieldWithType('triggerdata', TDynField.TType.TTrigData);
  if (fld <> nil) then result := fld.recref else result := nil;
end;

function TDynRecordHelper.trigMonsterId (): Integer; inline;
var
  fld: TDynField;
begin
  result := -1;
  fld := field['monsterid'];
  if (fld = nil) then exit;
  if (fld.baseType <> TDynField.TType.TInt) then exit;
  if (fld.recref = nil) then exit;
  result := fld.recrefIndex;
end;

function TDynRecordHelper.trigPanelRec (): TDynRecord; inline;
var
  fld: TDynField;
begin
  result := nil;
  fld := field['panelid'];
  if (fld = nil) then exit;
  if (fld.baseType <> TDynField.TType.TInt) then exit;
  result := fld.recref;
  if (result <> nil) and (result.typeName <> 'panel') then result := nil;
end;

// panel index in list
function TDynRecordHelper.trigPanelId (): Integer; inline;
var
  fld: TDynField;
begin
  result := -1;
  fld := field['panelid'];
  if (fld = nil) then exit;
  if (fld.baseType <> TDynField.TType.TInt) then exit;
  if (fld.recref = nil) then exit;
  if (fld.recref.typeName <> 'panel') then exit;
  result := fld.recrefIndex;
end;

function TDynRecordHelper.getTexturePanelRec (): TDynRecord;
var
  fld: TDynField;
begin
  result := nil;
  fld := field['texture_panel'];
  if (fld = nil) then exit;
  if (fld.baseType <> TDynField.TType.TInt) then exit;
  result := fld.recref;
  if (result <> nil) and (result.typeName <> 'panel') then result := nil;
end;

function TDynRecordHelper.getTexturePanel (): Integer;
var
  fld: TDynField;
begin
  result := -1;
  fld := field['texture_panel'];
  if (fld = nil) then exit;
  if (fld.baseType <> TDynField.TType.TInt) then exit;
  if (fld.recref = nil) then exit;
  if (fld.recref.typeName <> 'panel') then exit;
  result := fld.recrefIndex;
end;


// ////////////////////////////////////////////////////////////////////////// //
function TDynRecordHelper.mapName (): AnsiString; inline; begin result := utf2win(getFieldWithType('name', TDynField.TType.TChar).sval); end;
function TDynRecordHelper.mapAuthor (): AnsiString; inline; begin result := utf2win(getFieldWithType('author', TDynField.TType.TChar).sval); end;
function TDynRecordHelper.mapDesc (): AnsiString; inline; begin result := utf2win(getFieldWithType('description', TDynField.TType.TChar).sval); end;
function TDynRecordHelper.musicName (): AnsiString; inline; begin result := utf2win(getFieldWithType('music', TDynField.TType.TChar).sval); end;
function TDynRecordHelper.skyName (): AnsiString; inline; begin result := utf2win(getFieldWithType('sky', TDynField.TType.TChar).sval); end;
function TDynRecordHelper.X (): Integer; inline; begin result := getFieldWithType('position', TDynField.TType.TPoint).ival; end;
function TDynRecordHelper.Y (): Integer; inline; begin result := getFieldWithType('position', TDynField.TType.TPoint).ival2; end;
function TDynRecordHelper.Width (): Word; inline; begin result := Word(getFieldWithType('size', TDynField.TType.TSize).ival); end;
function TDynRecordHelper.Height (): Word; inline; begin result := Word(getFieldWithType('size', TDynField.TType.TSize).ival2); end;
function TDynRecordHelper.PanelType (): Word; inline; begin result := Word(getFieldWithType('type', TDynField.TType.TUShort).ival); end;
function TDynRecordHelper.TextureRec (): TDynRecord; inline; begin result := getFieldWithType('texture', TDynField.TType.TUShort).recref; end;
function TDynRecordHelper.Alpha (): Byte; inline; begin result := Byte(getFieldWithType('alpha', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.Flags (): Byte; inline; begin result := Byte(getFieldWithType('flags', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.Resource (): AnsiString; inline; begin result := utf2win(getFieldWithType('path', TDynField.TType.TChar).sval); end;
function TDynRecordHelper.Anim (): Boolean; inline; begin result := (getFieldWithType('animated', TDynField.TType.TBool).ival <> 0); end;
function TDynRecordHelper.ItemType (): Byte; inline; begin result := Byte(getFieldWithType('type', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.Options (): Byte; inline; begin result := Byte(getFieldWithType('options', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.MonsterType (): Byte; inline; begin result := Byte(getFieldWithType('type', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.Direction (): Byte; inline; begin result := Byte(getFieldWithType('direction', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.AreaType (): Byte; inline; begin result := Byte(getFieldWithType('type', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.Enabled (): Boolean; inline; begin result := (getFieldWithType('enabled', TDynField.TType.TBool).ival <> 0); end;
function TDynRecordHelper.TriggerType (): Byte; inline; begin result := Byte(getFieldWithType('type', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.ActivateType (): Byte; inline; begin result := Byte(getFieldWithType('activate_type', TDynField.TType.TUByte).ival); end;
function TDynRecordHelper.Keys (): Byte; inline; begin result := Byte(getFieldWithType('keys', TDynField.TType.TUByte).ival); end;

{$INCLUDE mapdef_impl.inc}


end.
