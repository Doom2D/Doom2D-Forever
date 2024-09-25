(* Copyright (C) 2016 - The Doom2D.org team & involved community members <http://www.doom2d.org>.
 * This file is part of Doom2D Forever.
 *
 * This program is free software: you can redistribute it and/or modify it under the terms of
 * the GNU General Public License as published by the Free Software Foundation, version 3 of
 * the License ONLY.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with this program.
 * If not, see <http://www.gnu.org/licenses/>.
 *)

// ////////////////////////////////////////////////////////////////////////// //
function monsTypeToString (mt: Byte): AnsiString;
begin
  case mt of
    MONSTER_NONE: begin result := 'none'; exit; end;
    MONSTER_DEMON: begin result := 'demon'; exit; end;
    MONSTER_IMP: begin result := 'imp'; exit; end;
    MONSTER_ZOMBY: begin result := 'zombie'; exit; end;
    MONSTER_SERG: begin result := 'serg'; exit; end;
    MONSTER_CYBER: begin result := 'cyber'; exit; end;
    MONSTER_CGUN: begin result := 'cgun'; exit; end;
    MONSTER_BARON: begin result := 'baron'; exit; end;
    MONSTER_KNIGHT: begin result := 'knight'; exit; end;
    MONSTER_CACO: begin result := 'caco'; exit; end;
    MONSTER_SOUL: begin result := 'soul'; exit; end;
    MONSTER_PAIN: begin result := 'pain'; exit; end;
    MONSTER_SPIDER: begin result := 'spider'; exit; end;
    MONSTER_BSP: begin result := 'bsp'; exit; end;
    MONSTER_MANCUB: begin result := 'mancubus'; exit; end;
    MONSTER_SKEL: begin result := 'skel'; exit; end;
    MONSTER_VILE: begin result := 'vile'; exit; end;
    MONSTER_FISH: begin result := 'fish'; exit; end;
    MONSTER_BARREL: begin result := 'barrel'; exit; end;
    MONSTER_ROBO: begin result := 'robo'; exit; end;
    MONSTER_MAN: begin result := 'man'; exit; end;
  end;
  result := 'unknown';
end;


function monsBehToString (bt: Byte): AnsiString;
begin
  case bt of
    BH_NORMAL: begin result := 'normal'; exit; end;
    BH_KILLER: begin result := 'killer'; exit; end;
    BH_MANIAC: begin result := 'maniac'; exit; end;
    BH_INSANE: begin result := 'insane'; exit; end;
    BH_CANNIBAL: begin result := 'cannibal'; exit; end;
    BH_GOOD: begin result := 'good'; exit; end;
  end;
  result := 'unknown';
end;


function monsStateToString (st: Byte): AnsiString;
begin
  case st of
    MONSTATE_SLEEP: begin result := 'sleep'; exit; end;
    MONSTATE_GO: begin result := 'go'; exit; end;
    MONSTATE_RUN: begin result := 'run'; exit; end;
    MONSTATE_CLIMB: begin result := 'climb'; exit; end;
    MONSTATE_DIE: begin result := 'die'; exit; end;
    MONSTATE_DEAD: begin result := 'dead'; exit; end;
    MONSTATE_ATTACK: begin result := 'attack'; exit; end;
    MONSTATE_SHOOT: begin result := 'shoot'; exit; end;
    MONSTATE_PAIN: begin result := 'pain'; exit; end;
    MONSTATE_WAIT: begin result := 'wait'; exit; end;
    MONSTATE_REVIVE: begin result := 'revive'; exit; end;
    MONSTATE_RUNOUT: begin result := 'runout'; exit; end;
  end;
  result := 'unknown';
end;


// ////////////////////////////////////////////////////////////////////////// //
function typeKind2Str (t: TTypeKind): AnsiString;
begin
  case t of
    tkUnknown: result := 'Unknown';
    tkInteger: result := 'Integer';
    tkChar: result := 'Char';
    tkEnumeration: result := 'Enumeration';
    tkFloat: result := 'Float';
    tkSet: result := 'Set';
    tkMethod: result := 'Method';
    tkSString: result := 'SString';
    tkLString: result := 'LString';
    tkAString: result := 'AString';
    tkWString: result := 'WString';
    tkVariant: result := 'Variant';
    tkArray: result := 'Array';
    tkRecord: result := 'Record';
    tkInterface: result := 'Interface';
    tkClass: result := 'Class';
    tkObject: result := 'Object';
    tkWChar: result := 'WChar';
    tkBool: result := 'Bool';
    tkInt64: result := 'Int64';
    tkQWord: result := 'QWord';
    tkDynArray: result := 'DynArray';
    tkInterfaceRaw: result := 'InterfaceRaw';
    tkProcVar: result := 'ProcVar';
    tkUString: result := 'UString';
    tkUChar: result := 'UChar';
    tkHelper: result := 'Helper';
    tkFile: result := 'File';
    tkClassRef: result := 'ClassRef';
    tkPointer: result := 'Pointer';
    else result := '<unknown>';
  end;
end;


procedure dumpPublishedProperties (obj: TObject);
var
  pt: PTypeData;
  pi: PTypeInfo;
  i, j: Integer;
  pp: PPropList;
begin
  if (obj = nil) then exit;
  e_LogWritefln('Object of type ''%s'':', [obj.ClassName]);
  pi := obj.ClassInfo;
  pt := GetTypeData(pi);
  e_LogWritefln('property count: %s', [pt.PropCount]);
  GetMem(pp, pt^.PropCount*sizeof(Pointer));
  try
    j := GetPropList(pi, [tkInteger, tkBool, tkSString, tkLString, tkAString, tkSet, tkEnumeration], pp);
    //e_LogWritefln('ordinal property count: %s', [j]);
    for i := 0 to j-1 do
    begin
      if (typinfo.PropType(obj, pp^[i].name) in [tkSString, tkLString, tkAString]) then
      begin
        e_LogWritefln('  #%s: <%s>; type: %s; value: <%s>', [i+1, pp^[i].name, typeKind2Str(typinfo.PropType(obj, pp^[i].name)), GetStrProp(obj, pp^[i])]);
      end
      else if (typinfo.PropType(obj, pp^[i].name) = tkSet) then
      begin
        e_LogWritefln('  #%s: <%s>; type: %s; value: %s', [i+1, pp^[i].name, typeKind2Str(typinfo.PropType(obj, pp^[i].name)), GetSetProp(obj, pp^[i], true)]);
      end
      else if (typinfo.PropType(obj, pp^[i].name) = tkEnumeration) then
      begin
        e_LogWritefln('  #%s: <%s>; type: %s; value: <%s>', [i+1, pp^[i].name, typeKind2Str(typinfo.PropType(obj, pp^[i].name)), GetEnumProp(obj, pp^[i])]);
      end
      else
      begin
        e_LogWritefln('  #%s: <%s>; type: %s; value: %s', [i+1, pp^[i].name, typeKind2Str(typinfo.PropType(obj, pp^[i].name)), GetOrdProp(obj, pp^[i])]);
      end;
    end;
  finally
    FreeMem(pp);
  end;
end;


//FIXME: autogenerate
function trigType2Str (ttype: Integer): AnsiString;
begin
  result := '<unknown>';
  case ttype of
    TRIGGER_NONE: result := 'none';
    TRIGGER_EXIT: result := 'exit';
    TRIGGER_TELEPORT: result := 'teleport';
    TRIGGER_OPENDOOR: result := 'opendoor';
    TRIGGER_CLOSEDOOR: result := 'closedoor';
    TRIGGER_DOOR: result := 'door';
    TRIGGER_DOOR5: result := 'door5';
    TRIGGER_CLOSETRAP: result := 'closetrap';
    TRIGGER_TRAP: result := 'trap';
    TRIGGER_PRESS: result := 'press';
    TRIGGER_SECRET: result := 'secret';
    TRIGGER_LIFTUP: result := 'liftup';
    TRIGGER_LIFTDOWN: result := 'liftdown';
    TRIGGER_LIFT: result := 'lift';
    TRIGGER_TEXTURE: result := 'texture';
    TRIGGER_ON: result := 'on';
    TRIGGER_OFF: result := 'off';
    TRIGGER_ONOFF: result := 'onoff';
    TRIGGER_SOUND: result := 'sound';
    TRIGGER_SPAWNMONSTER: result := 'spawnmonster';
    TRIGGER_SPAWNITEM: result := 'spawnitem';
    TRIGGER_MUSIC: result := 'music';
    TRIGGER_PUSH: result := 'push';
    TRIGGER_SCORE: result := 'score';
    TRIGGER_MESSAGE: result := 'message';
    TRIGGER_DAMAGE: result := 'damage';
    TRIGGER_HEALTH: result := 'health';
    TRIGGER_SHOT: result := 'shot';
    TRIGGER_EFFECT: result := 'effect';
    TRIGGER_SCRIPT: result := 'script';
  end;
end;
