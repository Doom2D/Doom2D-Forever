(* Copyright (C)  DooM 2D:Forever Developers
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
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

{
-----------------------------------
MAPDEF.PAS ВЕРСИЯ ОТ 22.03.09

Поддержка карт версии 1
-----------------------------------
}

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
    X, Y: LongInt;
  end;

  Char16     = packed array[0..15] of Char;
  Char32     = packed array[0..31] of Char;
  Char64     = packed array[0..63] of Char;
  Char100    = packed array[0..99] of Char;
  Char256    = packed array[0..255] of Char;
  Byte128    = packed array[0..127] of Byte;

{$INCLUDE mapdef.inc}

type
  TTexturesRec1Array = array of TTextureRec_1;
  TPanelsRec1Array = array of TPanelRec_1;
  TItemsRec1Array = array of TItemRec_1;
  TMonsterRec1Array = array of TMonsterRec_1;
  TAreasRec1Array = array of TAreaRec_1;
  TTriggersRec1Array = array of TTriggerRec_1;


function GetMapHeader (rec: TDynRecord): TMapHeaderRec_1;
function GetTextures (rec: TDynRecord): TTexturesRec1Array;
function GetPanels (rec: TDynRecord): TPanelsRec1Array;
function GetItems (rec: TDynRecord): TItemsRec1Array;
function GetAreas (rec: TDynRecord): TAreasRec1Array;
function GetMonsters (rec: TDynRecord): TMonsterRec1Array;
function GetTriggers (rec: TDynRecord): TTriggersRec1Array;


implementation

uses
  e_log, xparser, xstreams;


function GetMapHeader (rec: TDynRecord): TMapHeaderRec_1;
var
  ws: TSFSMemoryChunkStream = nil;
begin
  FillChar(result, sizeof(result), 0);
  if (rec = nil) then exit;
  try
    ws := TSFSMemoryChunkStream.Create(@result, sizeof(result));
    rec.writeBinTo(ws, -1, true); // only fields
  except // sorry
    FillChar(result, sizeof(result), 0);
  end;
  ws.Free();
end;


function GetTextures (rec: TDynRecord): TTexturesRec1Array;
var
  ws: TSFSMemoryChunkStream = nil;
  fld: TDynField;
  f: Integer;
begin
  result := nil;
  fld := rec.field['texture'];
  if (fld = nil) or (fld.baseType <> fld.TType.TList) or (fld.list.count = 0) then exit;
  ws := TSFSMemoryChunkStream.Create(nil, 0);
  try
    SetLength(result, fld.list.count);
    for f := 0 to fld.list.count-1 do
    begin
      FillChar(result[f], sizeof(result[f]), 0);
      ws.setup(@result[f], sizeof(result[f]));
      fld.list[f].writeBinTo(ws, -1, true); // only fields
    end;
  except
    result := nil;
  end;
  ws.Free();
end;


function GetPanels (rec: TDynRecord): TPanelsRec1Array;
var
  ws: TSFSMemoryChunkStream = nil;
  fld: TDynField;
  f: Integer;
begin
  result := nil;
  fld := rec.field['panel'];
  if (fld = nil) or (fld.baseType <> fld.TType.TList) or (fld.list.count = 0) then exit;
  ws := TSFSMemoryChunkStream.Create(nil, 0);
  try
    SetLength(result, fld.list.count);
    for f := 0 to fld.list.count-1 do
    begin
      FillChar(result[f], sizeof(result[f]), 0);
      ws.setup(@result[f], sizeof(result[f]));
      fld.list[f].writeBinTo(ws, -1, true); // only fields
    end;
  except
    result := nil;
  end;
  ws.Free();
end;


function GetItems (rec: TDynRecord): TItemsRec1Array;
var
  ws: TSFSMemoryChunkStream = nil;
  fld: TDynField;
  f: Integer;
begin
  result := nil;
  fld := rec.field['item'];
  if (fld = nil) or (fld.baseType <> fld.TType.TList) or (fld.list.count = 0) then exit;
  ws := TSFSMemoryChunkStream.Create(nil, 0);
  try
    SetLength(result, fld.list.count);
    for f := 0 to fld.list.count-1 do
    begin
      FillChar(result[f], sizeof(result[f]), 0);
      ws.setup(@result[f], sizeof(result[f]));
      fld.list[f].writeBinTo(ws, -1, true); // only fields
    end;
  except
    result := nil;
  end;
  ws.Free();
end;


function GetAreas (rec: TDynRecord): TAreasRec1Array;
var
  ws: TSFSMemoryChunkStream = nil;
  fld: TDynField;
  f: Integer;
begin
  result := nil;
  fld := rec.field['area'];
  if (fld = nil) or (fld.baseType <> fld.TType.TList) or (fld.list.count = 0) then exit;
  ws := TSFSMemoryChunkStream.Create(nil, 0);
  try
    SetLength(result, fld.list.count);
    for f := 0 to fld.list.count-1 do
    begin
      FillChar(result[f], sizeof(result[f]), 0);
      ws.setup(@result[f], sizeof(result[f]));
      fld.list[f].writeBinTo(ws, -1, true); // only fields
    end;
  except
    result := nil;
  end;
  ws.Free();
end;


function GetMonsters (rec: TDynRecord): TMonsterRec1Array;
var
  ws: TSFSMemoryChunkStream = nil;
  fld: TDynField;
  f: Integer;
begin
  result := nil;
  fld := rec.field['monster'];
  if (fld = nil) or (fld.baseType <> fld.TType.TList) or (fld.list.count = 0) then exit;
  ws := TSFSMemoryChunkStream.Create(nil, 0);
  try
    SetLength(result, fld.list.count);
    for f := 0 to fld.list.count-1 do
    begin
      FillChar(result[f], sizeof(result[f]), 0);
      ws.setup(@result[f], sizeof(result[f]));
      fld.list[f].writeBinTo(ws, -1, true); // only fields
    end;
  except
    result := nil;
  end;
  ws.Free();
end;


function GetTriggers (rec: TDynRecord): TTriggersRec1Array;
var
  ws: TSFSMemoryChunkStream = nil;
  fld: TDynField;
  f: Integer;
  //wr: TTextWriter;
  //fo: File;
begin
  result := nil;
  fld := rec.field['trigger'];
  if (fld = nil) or (fld.baseType <> fld.TType.TList) or (fld.list.count = 0) then exit;
  ws := TSFSMemoryChunkStream.Create(nil, 0);
  try
    //wr := TFileTextWriter.Create('z00.txt');
    SetLength(result, fld.list.count);
    for f := 0 to fld.list.count-1 do
    begin
      FillChar(result[f], sizeof(result[f]), 0);
      //e_LogWritefln(': trigger #%s; TexturePanel=%s', [f, result[f].TexturePanel]);
      ws.setup(@result[f], sizeof(result[f]));
      fld.list[f].writeBinTo(ws, -1, true); // only fields
      {
      e_LogWritefln(': trigger #%s; X=%s; Y=%s; Width=%s; Height=%s; Enabled=%s; TexturePanel=%s; TriggerType=%s; ActivateType=%s; Keys=%s', [f,
       result[f].X,
       result[f].Y,
       result[f].Width,
       result[f].Height,
       result[f].Enabled,
       result[f].TexturePanel,
       result[f].TriggerType,
       result[f].ActivateType,
       result[f].Keys
       ]);
      //e_LogWritefln('***'#10'%s'#10'***', [);
      fld.list[f].writeTo(wr);
      if (f = 0) then
      begin
        AssignFile(fo, 'z00.bin');
        Rewrite(fo, 1);
        BlockWrite(fo, result[f], sizeof(result[f]));
        CloseFile(fo);
      end;
      }
    end;
    //wr.Free();
  except
    result := nil;
  end;
  ws.Free();
end;


end.
