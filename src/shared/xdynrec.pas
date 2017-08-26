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
unit xdynrec;

interface

uses
  Classes,
  xparser, xstreams;


// ////////////////////////////////////////////////////////////////////////// //
type
  TDynMapDef = class;
  TDynRecord = class;

  // this is base type for all scalars (and arrays)
  TDynField = class
  public
    type
      TType = (TBool, TChar, TByte, TUByte, TShort, TUShort, TInt, TUInt, TString, TPoint, TSize, TList, TTrigData);
      // TPoint: pair of Shorts
      // TSize: pair of UShorts
      // TList: actually, array of records
      // TTrigData: array of bytes
      // arrays of chars are pascal shortstrings (with counter in the first byte)

    type
      TDynFieldArray = array of TDynField;
      TDynRecordArray = array of TDynRecord;

  private
    type
      TEBS = (TNone, TRec, TEnum, TBitSet);

  private
    mOwner: TDynRecord;
    mPasName: AnsiString;
    mName: AnsiString;
    mType: TType;
    mIVal: Integer; // for all integer types
    mIVal2: Integer; // for point and size
    mSVal: AnsiString; // string; for byte and char arrays
    mRVal: TDynRecordArray; // for list
    mRecRef: TDynRecord; // for record
    mMaxDim: Integer; // for byte and char arrays; <0: not an array; 0: impossible value
    mBinOfs: Integer; // offset in binary; <0 - none
    mRecOfs: Integer; // offset in record; <0 - none
    mSepPosSize: Boolean; // for points and sizes, use separate fields
    mAsT: Boolean; // for points and sizes, use separate fields, names starts with `t`
    mDefined: Boolean;
    mHasDefault: Boolean;
    mOmitDef: Boolean;
    mInternal: Boolean;
    mNegBool: Boolean;
    mBitSetUnique: Boolean; // bitset can contain only one value
    // default value
    mDefUnparsed: AnsiString;
    mDefSVal: AnsiString; // default string value
    mDefIVal, mDefIVal2: Integer; // default integer values
    mDefRecRef: TDynRecord;
    mEBS: TEBS; // complex type type
    mEBSTypeName: AnsiString; // name of enum, bitset or record

    // temp
    mDefId: AnsiString;

  private
    procedure cleanup ();

    procedure parseDef (pr: TTextParser);

    procedure parseDefaultValue (); // parse `mDefUnparsed` to `mDefSVal`, `mDefIVal`, `mDefIVal2`, `mDefRecRef`
    procedure fixDefaultValue (); // this will NOT clone `mDefRecRef`
    function isDefaultValue (): Boolean;

  public
    constructor Create (const aname: AnsiString; atype: TType);
    constructor Create (pr: TTextParser);
    destructor Destroy (); override;

    class function getTypeName (t: TType): AnsiString;

    function definition (): AnsiString;

    function clone (): TDynField;

    procedure parseValue (pr: TTextParser);
    procedure parseBinValue (st: TStream);

    procedure writeTo (wr: TTextWriter);
    procedure writeBinTo (st: TStream);

    // won't work for lists
    function isSimpleEqu (fld: TDynField): Boolean;

    procedure setValue (const s: AnsiString);

  public
    property pasname: AnsiString read mPasName;
    property name: AnsiString read mName;
    property baseType: TType read mType;
    property defined: Boolean read mDefined write mDefined;
    property internal: Boolean read mInternal write mInternal;
    property ival: Integer read mIVal;
    property sval: AnsiString read mSVal;
    //property list: TDynRecordArray read mRVal write mRVal;
    property maxdim: Integer read mMaxDim; // for fixed-size arrays
    property binOfs: Integer read mBinOfs; // offset in binary; <0 - none
    property recOfs: Integer read mRecOfs; // offset in record; <0 - none
    property hasDefault: Boolean read mHasDefault;
    property defsval: AnsiString read mDefSVal;
    property ebstype: TEBS read mEBS;
    property ebstypename: AnsiString read mEBSTypeName; // enum/bitset name

    property x: Integer read mIVal;
    property w: Integer read mIVal;
    property y: Integer read mIVal2;
    property h: Integer read mIVal2;
  end;


  // "value" header record contains TList fields, with name equal to record type
  TDynRecord = class
  private
    mOwner: TDynMapDef;
    mId: AnsiString;
    mPasName: AnsiString;
    mName: AnsiString;
    mSize: Integer;
    mFields: TDynField.TDynFieldArray;
    mTrigTypes: array of AnsiString; // if this is triggerdata, we'll hold list of triggers here
    mHeader: Boolean; // true for header record
    mBinBlock: Integer; // -1: none
    mHeaderRec: TDynRecord; // for "value" records this is header record with data, for "type" records this is header type record

  private
    procedure parseDef (pr: TTextParser); // parse definition

    function findByName (const aname: AnsiString): Integer; inline;
    function hasByName (const aname: AnsiString): Boolean; inline;
    function getFieldByName (const aname: AnsiString): TDynField; inline;

    function getIsTrigData (): Boolean; inline;
    function getIsForTrig (const aname: AnsiString): Boolean; inline;

  protected
    function findRecordByTypeId (const atypename, aid: AnsiString): TDynRecord;
    function findRecordNumByType (const atypename: AnsiString; rc: TDynRecord): Integer;
    procedure addRecordByType (const atypename: AnsiString; rc: TDynRecord);

  public
    constructor Create ();
    constructor Create (pr: TTextParser); // parse definition
    destructor Destroy (); override;

    function definition (): AnsiString;

    function clone (): TDynRecord;

    procedure parseValue (pr: TTextParser);
    procedure parseBinValue (st: TStream);

    procedure writeTo (wr: TTextWriter; putHeader: Boolean=true);
    procedure writeBinTo (st: TStream; trigbufsz: Integer=-1);

  public
    property id: AnsiString read mId; // for map parser
    property pasname: AnsiString read mPasName;
    property name: AnsiString read mName; // record name
    property size: Integer read mSize; // size in bytes
    property fields: TDynField.TDynFieldArray read mFields write mFields;
    property has[const aname: AnsiString]: Boolean read hasByName;
    property field[const aname: AnsiString]: TDynField read getFieldByName;
    property isTrigData: Boolean read getIsTrigData;
    property isForTrig[const aname: AnsiString]: Boolean read getIsForTrig;
  end;


  TDynEBS = class
  private
    mOwner: TDynMapDef;
    mIsEnum: Boolean;
    mName: AnsiString;
    mIds: array of AnsiString;
    mVals: array of Integer;
    mMaxName: AnsiString; // MAX field
    mMaxVal: Integer; // max value

  private
    procedure cleanup ();

    procedure parseDef (pr: TTextParser); // parse definition

    function findByName (const aname: AnsiString): Integer; inline;
    function hasByName (const aname: AnsiString): Boolean; inline;
    function getFieldByName (const aname: AnsiString): Integer; inline;

  public
    constructor Create (pr: TTextParser); // parse definition
    destructor Destroy (); override;

    function definition (): AnsiString;

  public
    property name: AnsiString read mName; // record name
    property isEnum: Boolean read mIsEnum;
    property has[const aname: AnsiString]: Boolean read hasByName;
    property field[const aname: AnsiString]: Integer read getFieldByName;
  end;


  TDynMapDef = class
  public
    recTypes: array of TDynRecord; // [0] is always header
    trigTypes: array of TDynRecord; // trigdata
    ebsTypes: array of TDynEBS; // enums, bitsets

  private
    procedure parseDef (pr: TTextParser);

    function getHeaderRecType (): TDynRecord; inline;

  public
    constructor Create (pr: TTextParser); // parses data definition
    destructor Destroy (); override;

    function findRecType (const aname: AnsiString): TDynRecord;
    function findTrigFor (const aname: AnsiString): TDynRecord;
    function findEBSType (const aname: AnsiString): TDynEBS;

    // creates new header record
    function parseMap (pr: TTextParser): TDynRecord;

    // creates new header record
    function parseBinMap (st: TStream): TDynRecord;

  public
    property headerType: TDynRecord read getHeaderRecType;
  end;


implementation

uses
  SysUtils,
  utils;


// ////////////////////////////////////////////////////////////////////////// //
constructor TDynField.Create (const aname: AnsiString; atype: TType);
begin
  mRVal := nil;
  mRecRef := nil;
  cleanup();
  mName := aname;
  mType := atype;
end;


constructor TDynField.Create (pr: TTextParser);
begin
  cleanup();
  parseDef(pr);
end;


destructor TDynField.Destroy ();
begin
  cleanup();
  inherited;
end;


procedure TDynField.cleanup ();
begin
  mName := '';
  mType := TType.TInt;
  mIVal := 0;
  mIVal2 := 0;
  mSVal := '';
  mRVal := nil;
  mRecRef := nil;
  mMaxDim := -1;
  mBinOfs := -1;
  mRecOfs := -1;
  mSepPosSize := false;
  mAsT := false;
  mHasDefault := false;
  mDefined := false;
  mOmitDef := false;
  mInternal := true;
  mDefUnparsed := '';
  mDefSVal := '';
  mDefIVal := 0;
  mDefIVal2 := 0;
  mDefRecRef := nil;
  mEBS := TEBS.TNone;
  mEBSTypeName := '';
  mBitSetUnique := false;
  mNegBool := false;
  mDefId := '';
end;


function TDynField.clone (): TDynField;
var
  f: Integer;
begin
  result := TDynField.Create(mName, mType);
  result.mOwner := mOwner;
  result.mPasName := mPasName;
  result.mName := mName;
  result.mType := mType;
  result.mIVal := mIVal;
  result.mIVal2 := mIVal2;
  result.mSVal := mSVal;
  SetLength(result.mRVal, Length(mRVal));
  for f := 0 to High(mRVal) do result.mRVal[f] := mRVal[f].clone();
  result.mRecRef := mRecRef;
  result.mMaxDim := mMaxDim;
  result.mBinOfs := mBinOfs;
  result.mRecOfs := mRecOfs;
  result.mSepPosSize := mSepPosSize;
  result.mAsT := mAsT;
  result.mDefined := mDefined;
  result.mHasDefault := mHasDefault;
  result.mOmitDef := mOmitDef;
  result.mInternal := mInternal;
  result.mDefUnparsed := mDefUnparsed;
  result.mDefSVal := mDefSVal;
  result.mDefIVal := mDefIVal;
  result.mDefIVal2 := mDefIVal2;
  result.mDefRecRef := mDefRecRef;
  result.mEBS := mEBS;
  result.mEBSTypeName := mEBSTypeName;
  result.mBitSetUnique := mBitSetUnique;
  result.mNegBool := mNegBool;
  result.mDefId := mDefId;
end;


// won't work for lists
function TDynField.isSimpleEqu (fld: TDynField): Boolean;
begin
  if (fld = nil) or (mType <> fld.mType) then begin result := false; exit; end;
  case mType of
    TType.TBool: result := ((mIVal <> 0) = (fld.mIVal <> 0));
    TType.TChar: result := (mSVal = fld.mSVal);
    TType.TByte,
    TType.TUByte,
    TType.TShort,
    TType.TUShort,
    TType.TInt,
    TType.TUInt:
      result := (mIVal = fld.mIVal);
    TType.TString: result := (mSVal = fld.mSVal);
    TType.TPoint,
    TType.TSize:
      result := ((mIVal = fld.mIVal) and (mIVal2 = fld.mIVal2));
    TType.TList: result := false;
    TType.TTrigData: result := false;
    else raise Exception.Create('ketmar forgot to handle some field type');
  end;
end;


procedure TDynField.setValue (const s: AnsiString);
var
  stp: TTextParser;
begin
  stp := TStrTextParser.Create(s+';');
  try
    parseValue(stp);
  finally
    stp.Free();
  end;
end;


procedure TDynField.parseDefaultValue ();
var
  stp: TTextParser = nil;
  oSVal: AnsiString;
  oIVal, oIVal2: Integer;
  oRRef: TDynRecord;
  oDef: Boolean;
begin
  if not mHasDefault then
  begin
    mDefSVal := '';
    mDefIVal := 0;
    mDefIVal2 := 0;
    mDefRecRef := nil;
  end
  else
  begin
    oSVal := mSVal;
    oIVal := mIVal;
    oIVal2 := mIVal2;
    oRRef := mRecRef;
    oDef := mDefined;
    try
      stp := TStrTextParser.Create(mDefUnparsed+';');
      parseValue(stp);
      mDefSVal := mSVal;
      mDefIVal := mIVal;
      mDefIVal2 := mIVal2;
      mDefRecRef := mRecRef;
    finally
      mSVal := oSVal;
      mIVal := oIVal;
      mIVal2 := oIVal2;
      mRecRef := oRRef;
      mDefined := oDef;
      stp.Free();
    end;
  end;
end;


// default value should be parsed
procedure TDynField.fixDefaultValue ();
begin
  if mDefined then exit;
  if not mHasDefault then
  begin
    if mInternal then exit;
    raise Exception.Create(Format('field ''%s'' in record ''%s'' of record type ''%s'' is not set', [mName, mOwner.mId, mOwner.mName]));
  end;
  if (mEBS = TEBS.TRec) then
  begin
    mRecRef := mDefRecRef;
    {
    if (mDefRecRef <> nil) then
    begin
      rec := mDefRecRef.clone();
      rec.mHeaderRec := mOwner.mHeaderRec;
      try
        mOwner.addRecordByType(mEBSTypeName, rec);
        mRecRef := rec;
        rec := nil;
      finally
        rec.Free();
      end;
    end;
    }
  end;
  mSVal := mDefSVal;
  mIVal := mDefIVal;
  mIVal2 := mDefIVal2;
  mDefined := true;
end;


// default value should be parsed
function TDynField.isDefaultValue (): Boolean;
begin
  if not mHasDefault then begin result := false; exit; end;
  if (mEBS = TEBS.TRec) then begin result := (mRecRef = mDefRecRef); exit; end;
  case mType of
    TType.TChar, TType.TString: result := (mSVal = mDefSVal);
    TType.TPoint, TType.TSize: result := (mIVal = mDefIVal2) and (mIVal2 = mDefIVal2);
    TType.TList, TType.TTrigData: result := false; // no default values for those types
    else result := (mIVal = mDefIVal);
  end;
end;


class function TDynField.getTypeName (t: TType): AnsiString;
begin
  case t of
    TType.TBool: result := 'bool';
    TType.TChar: result := 'char';
    TType.TByte: result := 'byte';
    TType.TUByte: result := 'ubyte';
    TType.TShort: result := 'short';
    TType.TUShort: result := 'ushort';
    TType.TInt: result := 'int';
    TType.TUInt: result := 'uint';
    TType.TString: result := 'string';
    TType.TPoint: result := 'point';
    TType.TSize: result := 'size';
    TType.TList: result := 'array';
    TType.TTrigData: result := 'trigdata';
    else raise Exception.Create('ketmar forgot to handle some field type');
  end;
end;


function TDynField.definition (): AnsiString;
begin
  result := mPasName+' is '+TTextParser.quote(mName)+' type ';
  result += getTypeName(mType);
  if (mMaxDim >= 0) then result += Format('[%d]', [mMaxDim]);
  if (mRecOfs >= 0) then result += Format(' offset %d', [mRecOfs]);
  case mEBS of
    TEBS.TNone: begin end;
    TEBS.TRec: result += ' '+mEBSTypeName;
    TEBS.TEnum: result += ' enum '+mEBSTypeName;
    TEBS.TBitSet: begin result += ' bitset '; if mBitSetUnique then result += 'unique '; result += mEBSTypeName; end;
  end;
  if mHasDefault and (Length(mDefUnparsed) > 0) then result += ' default '+mDefUnparsed;
  if mSepPosSize then
  begin
         if (mType = TType.TPoint) then begin if (mAsT) then result += ' as txy' else result += ' as xy'; end
    else if (mType = TType.TSize) then begin if (mAsT) then result += ' as twh' else result += ' as wh'; end;
  end;
  if mOmitDef then result += ' omitdefault';
  if mInternal then result += ' internal';
end;


procedure TDynField.parseDef (pr: TTextParser);
var
  fldname: AnsiString;
  fldtype: AnsiString;
  fldofs: Integer;
  fldrecname: AnsiString;
  fldpasname: AnsiString;
  asxy, aswh, ast: Boolean;
  ainternal: Boolean;
  omitdef: Boolean;
  defstr: AnsiString;
  defint: Integer;
  hasdefStr: Boolean;
  hasdefInt: Boolean;
  hasdefId: Boolean;
  lmaxdim: Integer;
  lebs: TDynField.TEBS;
  unique: Boolean;
begin
  fldpasname := '';
  fldname := '';
  fldtype := '';
  fldofs := -1;
  fldrecname := '';
  asxy := false;
  aswh := false;
  ast := false;
  ainternal := false;
  omitdef := false;
  defstr := '';
  defint := 0;
  hasdefStr := false;
  hasdefInt := false;
  hasdefId := false;
  unique := false;
  lmaxdim := -1;
  lebs := TDynField.TEBS.TNone;

  fldpasname := pr.expectId(); // pascal field name
  // field name
  pr.expectId('is');
  fldname := pr.expectStr();
  // field type
  pr.expectId('type');
  fldtype := pr.expectId();

  // fixed-size array?
  if pr.eatDelim('[') then
  begin
    lmaxdim := pr.expectInt();
    if (lmaxdim < 1) then raise Exception.Create(Format('invali field ''%s'' array size', [fldname]));
    pr.expectDelim(']');
  end;

  while (pr.tokType <> pr.TTSemi) do
  begin
    if pr.eatId('offset') then
    begin
      if (fldofs >= 0) then raise Exception.Create(Format('duplicate field ''%s'' offset', [fldname]));
      fldofs := pr.expectInt();
      if (fldofs < 0) then raise Exception.Create(Format('invalid field ''%s'' offset', [fldname]));
      continue;
    end;

    if pr.eatId('as') then
    begin
           if pr.eatId('xy') then asxy := true
      else if pr.eatId('wh') then aswh := true
      else if pr.eatId('txy') then begin asxy := true; ast := true; end
      else if pr.eatId('twh') then begin aswh := true; ast := true; end
      else raise Exception.Create(Format('invalid field ''%s'' as what?', [fldname]));
      continue;
    end;

    if pr.eatId('enum') then
    begin
      lebs := TDynField.TEBS.TEnum;
      if (Length(fldrecname) <> 0) then raise Exception.Create(Format('field ''%s'' already typed as ''%s''', [fldname, fldrecname]));
      fldrecname := pr.expectId();
      continue;
    end;

    if pr.eatId('bitset') then
    begin
      lebs := TDynField.TEBS.TBitSet;
      if (Length(fldrecname) <> 0) then raise Exception.Create(Format('field ''%s'' already typed as ''%s''', [fldname, fldrecname]));
      unique := pr.eatId('unique');
      fldrecname := pr.expectId();
      continue;
    end;

    if pr.eatId('default') then
    begin
      if hasdefStr or hasdefInt or hasdefId then raise Exception.Create(Format('field ''%s'' has duplicate default', [fldname]));
      case pr.tokType of
        pr.TTStr:
          begin
            hasdefStr := true;
            defstr := pr.expectStr(true); // allow empty strings
          end;
        pr.TTId:
          begin
            hasdefId := true;
            defstr := pr.expectId();
          end;
        pr.TTInt:
          begin
            hasdefInt := true;
            defint := pr.expectInt();
          end;
        else
          raise Exception.Create(Format('field ''%s'' has invalid default', [fldname]));
      end;
      continue;
    end;

    if pr.eatId('omitdefault') then
    begin
      omitdef := true;
      continue;
    end;

    if pr.eatId('internal') then
    begin
      ainternal := true;
      continue;
    end;

    if (pr.tokType <> pr.TTId) then raise Exception.Create(Format('field ''%s'' has something unexpected in definition', [fldname]));

    if (Length(fldrecname) <> 0) then raise Exception.Create(Format('field ''%s'' already typed as ''%s''', [fldname, fldrecname]));
    fldrecname := pr.expectId();
    lebs := TDynField.TEBS.TRec;
  end;

  pr.expectTT(pr.TTSemi);

  // create field
  mName := fldname;
       if (fldtype = 'bool') then mType := TType.TBool
  else if (fldtype = 'negbool') then begin mType := TType.TBool; mNegBool := true; end
  else if (fldtype = 'char') then mType := TType.TChar
  else if (fldtype = 'byte') then mType := TType.TByte
  else if (fldtype = 'ubyte') then mType := TType.TUByte
  else if (fldtype = 'short') then mType := TType.TShort
  else if (fldtype = 'ushort') then mType := TType.TUShort
  else if (fldtype = 'int') then mType := TType.TInt
  else if (fldtype = 'uint') then mType := TType.TUInt
  else if (fldtype = 'string') then mType := TType.TString
  else if (fldtype = 'point') then mType := TType.TPoint
  else if (fldtype = 'size') then mType := TType.TSize
  else if (fldtype = 'trigdata') then mType := TType.TTrigData
  else raise Exception.Create(Format('field ''%s'' has invalid type ''%s''', [fldname, fldtype]));

       if hasdefStr then self.mDefUnparsed := TTextParser.quote(defstr)
  else if hasdefInt then self.mDefUnparsed := Format('%d', [defint])
  else if hasdefId then self.mDefUnparsed := defstr;

  self.mHasDefault := (hasdefStr or hasdefId or hasdefInt);
  self.mPasName := fldpasname;
  self.mEBS := lebs;
  self.mEBSTypeName := fldrecname;
  self.mBitSetUnique := unique;
  self.mMaxDim := lmaxdim;
  self.mBinOfs := fldofs;
  self.mRecOfs := fldofs;
  self.mSepPosSize := (asxy or aswh);
  self.mAsT := ast;
  self.mOmitDef := omitdef;
  self.mInternal := ainternal;

  //if mHasDefault then parseDefaultValue();
end;


procedure TDynField.writeBinTo (st: TStream);
var
  s: AnsiString;
  f: Integer;
  maxv: Integer;
  buf: PByte;
  ws: TStream = nil;
begin
  case mEBS of
    TEBS.TNone: begin end;
    TEBS.TRec:
      begin
        if (mMaxDim >= 0) then
        begin
          // this must be triggerdata
          if (CompareText(mEBSTypeName, 'triggerdata') <> 0) then
          begin
            raise Exception.Create(Format('record reference type ''%s'' in field ''%s'' cannot be written', [mEBSTypeName, mName]));
          end;
          // write triggerdata
          case mType of
            TType.TChar, TType.TByte, TType.TUByte: begin end;
            else raise Exception.Create(Format('record reference type ''%s'' in field ''%s'' cannot be written', [mEBSTypeName, mName]));
          end;
          //writeln('trigdata size: ', mMaxDim);
          GetMem(buf, mMaxDim);
          if (buf = nil) then raise Exception.Create(Format('record reference type ''%s'' in field ''%s'' cannot be written', [mEBSTypeName, mName]));
          try
            FillChar(buf^, mMaxDim, 0);
            if (mRecRef <> nil) then
            begin
              ws := TSFSMemoryChunkStream.Create(buf, mMaxDim);
              mRecRef.writeBinTo(ws, mMaxDim); // as trigdata
            end;
            st.WriteBuffer(buf^, mMaxDim);
          finally
            ws.Free();
            if (buf <> nil) then FreeMem(buf);
          end;
          exit;
        end;
        if (mRecRef = nil) then
        begin
          // no ref, write -1
          case mType of
            TType.TByte, TType.TUByte: writeInt(st, Byte(-1));
            TType.TShort, TType.TUShort: writeInt(st, SmallInt(-1));
            TType.TInt, TType.TUInt: writeInt(st, Integer(-1));
            else raise Exception.Create(Format('record reference type ''%s'' in field ''%s'' cannot be written', [mEBSTypeName, mName]));
          end;
          exit;
        end;
        case mType of
          TType.TByte: maxv := 127;
          TType.TUByte: maxv := 254;
          TType.TShort: maxv := 32767;
          TType.TUShort: maxv := 65534;
          TType.TInt: maxv := $7fffffff;
          TType.TUInt: maxv := $7fffffff;
          else raise Exception.Create(Format('record reference type ''%s'' in field ''%s'' cannot be written', [mEBSTypeName, mName]));
        end;
        // find record number
        f := mOwner.findRecordNumByType(mEBSTypeName, mRecRef);
        if (f < 0) then raise Exception.Create(Format('record reference type ''%s'' in field ''%s'' not found in record list', [mEBSTypeName, mName]));
        if (f > maxv) then raise Exception.Create(Format('record reference type ''%s'' in field ''%s'' has too big index', [mEBSTypeName, mName]));
        case mType of
          TType.TByte, TType.TUByte: writeInt(st, Byte(f));
          TType.TShort, TType.TUShort: writeInt(st, SmallInt(f));
          TType.TInt, TType.TUInt: writeInt(st, Integer(f));
          else raise Exception.Create(Format('record reference type ''%s'' in field ''%s'' cannot be written', [mEBSTypeName, mName]));
        end;
        exit;
      end;
    TEBS.TEnum: begin end;
    TEBS.TBitSet: begin end;
    else raise Exception.Create('ketmar forgot to handle some EBS type');
  end;

  case mType of
    TType.TBool:
      begin
        if (mIVal <> 0) then writeInt(st, Byte(1)) else writeInt(st, Byte(0));
        exit;
      end;
    TType.TChar:
      begin
        if (mMaxDim = 0) then raise Exception.Create(Format('invalid string size definition for field ''%s''', [mName]));
        if (mMaxDim < 0) then
        begin
          if (Length(mSVal) <> 1) then raise Exception.Create(Format('invalid string size definition for field ''%s''', [mName]));
          writeInt(st, Byte(mSVal[1]));
        end
        else
        begin
          if (Length(mSVal) > mMaxDim) then raise Exception.Create(Format('invalid string size definition for field ''%s''', [mName]));
          s := utfTo1251(mSVal);
          if (Length(s) > 0) then st.WriteBuffer(PChar(s)^, Length(s));
          for f := Length(s) to mMaxDim do writeInt(st, Byte(0));
        end;
        exit;
      end;
    TType.TByte,
    TType.TUByte:
      begin
        // triggerdata array was processed earlier
        if (mMaxDim >= 0) then Exception.Create(Format('byte array in field ''%s'' cannot be written', [mName]));
        writeInt(st, Byte(mIVal));
        exit;
      end;
    TType.TShort,
    TType.TUShort:
      begin
        if (mMaxDim >= 0) then raise Exception.Create(Format('short array in field ''%s'' cannot be written', [mName]));
        writeInt(st, Word(mIVal));
        exit;
      end;
    TType.TInt,
    TType.TUInt:
      begin
        if (mMaxDim >= 0) then raise Exception.Create(Format('int array in field ''%s'' cannot be written', [mName]));
        writeInt(st, LongWord(mIVal));
        exit;
      end;
    TType.TString:
      begin
        raise Exception.Create(Format('cannot write string field ''%s''', [mName]));
      end;
    TType.TPoint,
    TType.TSize:
      begin
        if (mMaxDim >= 0) then raise Exception.Create(Format('pos/size array in field ''%s'' cannot be written', [mName]));
        writeInt(st, Word(mIVal));
        writeInt(st, Word(mIVal2));
        exit;
      end;
    TType.TList:
      begin
        assert(false);
        exit;
      end;
    TType.TTrigData:
      begin
        assert(false);
        exit;
      end;
    else raise Exception.Create('ketmar forgot to handle some field type');
  end;
end;


procedure TDynField.writeTo (wr: TTextWriter);
var
  def: TDynMapDef;
  es: TDynEBS = nil;
  f, mask: Integer;
  first, found: Boolean;
begin
  wr.put(mName);
  wr.put(' ');
  // if this field should contain struct, convert type and parse struct
  case mEBS of
    TEBS.TNone: begin end;
    TEBS.TRec:
      begin
        if (mRecRef = nil) then
        begin
          wr.put('null;'#10);
        end
        else if (Length(mRecRef.mId) = 0) then
        begin
          mRecRef.writeTo(wr, false); // only data, no header
        end
        else
        begin
          wr.put(mRecRef.mId);
          wr.put(';'#10);
        end;
        exit;
      end;
    TEBS.TEnum:
      begin
        def := mOwner.mOwner;
        es := def.findEBSType(mEBSTypeName);
        if (es = nil) then raise Exception.Create(Format('record enum type ''%s'' for field ''%s'' not found', [mEBSTypeName, mName]));
        for f := 0 to High(es.mVals) do
        begin
          if (es.mVals[f] = mIVal) then
          begin
            wr.put(es.mIds[f]);
            wr.put(';'#10);
            exit;
          end;
        end;
        raise Exception.Create(Format('value %d in record enum type ''%s'' for field ''%s'' not found', [mIVal, mEBSTypeName, mName]));
      end;
    TEBS.TBitSet:
      begin
        def := mOwner.mOwner;
        es := def.findEBSType(mEBSTypeName);
        if (es = nil) then raise Exception.Create(Format('record bitset type ''%s'' for field ''%s'' not found', [mEBSTypeName, mName]));
        // none?
        if (mIVal = 0) then
        begin
          for f := 0 to High(es.mVals) do
          begin
            if (es.mVals[f] = 0) then
            begin
              wr.put(es.mIds[f]);
              wr.put(';'#10);
              exit;
            end;
          end;
          raise Exception.Create(Format('value %d in record bitset type ''%s'' for field ''%s'' not found', [0, mEBSTypeName, mName]));
        end;
        // not none
        mask := 1;
        first := true;
        while (mask <> 0) do
        begin
          if ((mIVal and mask) <> 0) then
          begin
            found := false;
            for f := 0 to High(es.mVals) do
            begin
              if (es.mVals[f] = mask) then
              begin
                if not first then wr.put('+') else first := false;
                wr.put(es.mIds[f]);
                found := true;
                break;
              end;
            end;
            if not found then raise Exception.Create(Format('value %d in record bitset type ''%s'' for field ''%s'' not found', [mask, mEBSTypeName, mName]));
          end;
          mask := mask shl 1;
        end;
        wr.put(';'#10);
        exit;
      end;
    else raise Exception.Create('ketmar forgot to handle some EBS type');
  end;

  case mType of
    TType.TBool:
      begin
        if (mIVal = 0) then wr.put('false;'#10) else wr.put('true;'#10);
        exit;
      end;
    TType.TChar:
      begin
        if (mMaxDim = 0) then raise Exception.Create(Format('invalid string size definition for field ''%s''', [mName]));
        wr.put(TTextParser.quote(mSVal));
        wr.put(';'#10);
        exit;
      end;
    TType.TByte,
    TType.TUByte,
    TType.TShort,
    TType.TUShort,
    TType.TInt,
    TType.TUInt:
      begin
        wr.put('%d;'#10, [mIVal]);
        exit;
      end;
    TType.TString:
      begin
        wr.put(TTextParser.quote(mSVal));
        wr.put(';'#10);
        exit;
      end;
    TType.TPoint,
    TType.TSize:
      begin
        wr.put('(%d %d);'#10, [mIVal, mIVal2]);
        exit;
      end;
    TType.TList:
      begin
        assert(false);
        exit;
      end;
    TType.TTrigData:
      begin
        assert(false);
        exit;
      end;
    else raise Exception.Create('ketmar forgot to handle some field type');
  end;
  raise Exception.Create(Format('cannot parse field ''%s'' yet', [mName]));
end;


procedure TDynField.parseValue (pr: TTextParser);

  procedure parseInt (min, max: Integer);
  begin
    mIVal := pr.expectInt();
    if (mIVal < min) or (mIVal > max) then raise Exception.Create(Format('invalid %s value for field ''%s''', [getTypeName(mType), mName]));
    mDefined := true;
  end;

var
  rec, rc: TDynRecord;
  es: TDynEBS = nil;
  tfld: TDynField;
  tk: AnsiString;
begin
  // if this field should contain struct, convert type and parse struct
  case mEBS of
    TEBS.TNone: begin end;
    TEBS.TRec:
      begin
        // ugly hack. sorry.
        if (CompareText(mEBSTypeName, 'triggerdata') = 0) then
        begin
          rec := mOwner;
          // find trigger definition
          tfld := rec.field['type'];
          if (tfld = nil) then raise Exception.Create(Format('triggerdata value for field ''%s'' in record ''%s'' without ''type'' field', [mName, rec.mName]));
          if (tfld.mEBS <> TEBS.TEnum) then raise Exception.Create(Format('triggerdata value for field ''%s'' in record ''%s'' with bad ''type'' field', [mName, rec.mName]));
          rc := mOwner.mOwner.findTrigFor(tfld.mSVal); // find in mapdef
          if (rc = nil) then raise Exception.Create(Format('triggerdata definition for field ''%s'' in record ''%s'' with type ''%s'' not found', [mName, rec.mName, tfld.mSVal]));
          rc := rc.clone();
          rc.mHeaderRec := mOwner.mHeaderRec;
          rc.parseValue(pr);
          mRecRef := rc;
          mDefined := true;
          exit;
        end;
        // other record types
        if (pr.tokType = pr.TTId) then
        begin
          if pr.eatId('null') then
          begin
            mRecRef := nil;
          end
          else
          begin
            rec := mOwner.findRecordByTypeId(mEBSTypeName, pr.tokStr);
            if (rec = nil) then raise Exception.Create(Format('record ''%s'' (%s) value for field ''%s'' not found', [pr.tokStr, mEBSTypeName, mName]));
            pr.expectId();
            mRecRef := rec;
          end;
          mDefined := true;
          pr.expectTT(pr.TTSemi);
          exit;
        end
        else if (pr.tokType = pr.TTBegin) then
        begin
          rec := mOwner.mOwner.findRecType(mEBSTypeName); // find in mapdef
          if (rec = nil) then raise Exception.Create(Format('record type ''%s'' for field ''%s'' not found', [mEBSTypeName, mName]));
          rc := rec.clone();
          rc.mHeaderRec := mOwner.mHeaderRec;
          rc.parseValue(pr);
          mRecRef := rc;
          mDefined := true;
          mOwner.addRecordByType(mEBSTypeName, rc);
          pr.eatTT(pr.TTSemi); // hack: allow (but don't require) semicolon after inline records
          exit;
        end;
        pr.expectTT(pr.TTBegin);
      end;
    TEBS.TEnum:
      begin
        es := mOwner.mOwner.findEBSType(mEBSTypeName); // find in mapdef
        if (es = nil) then raise Exception.Create(Format('record enum type ''%s'' for field ''%s'' not found', [mEBSTypeName, mName]));
        tk := pr.expectId();
        if not es.has[tk] then raise Exception.Create(Format('record enum value ''%s'' of type ''%s'' for field ''%s'' not found', [tk, mEBSTypeName, mName]));
        mIVal := es.field[tk];
        mSVal := tk;
        //writeln('ENUM ', mEBSName, '; element <', mSVal, '> with value ', mIVal);
        mDefined := true;
        pr.expectTT(pr.TTSemi);
        exit;
      end;
    TEBS.TBitSet:
      begin
        es := mOwner.mOwner.findEBSType(mEBSTypeName); // find in mapdef
        if (es = nil) then raise Exception.Create(Format('record bitset type ''%s'' for field ''%s'' not found', [mEBSTypeName, mName]));
        mIVal := 0;
        while true do
        begin
          tk := pr.expectId();
          if not es.has[tk] then raise Exception.Create(Format('record bitset value ''%s'' of type ''%s'' for field ''%s'' not found', [tk, mEBSTypeName, mName]));
          mIVal := mIVal or es.field[tk];
          mSVal := tk;
          if (pr.tokType <> pr.TTDelim) or ((pr.tokChar <> '|') and (pr.tokChar <> '+')) then break;
          if mBitSetUnique then raise Exception.Create(Format('record bitset of type ''%s'' for field ''%s'' expects only one value', [tk, mEBSTypeName, mName]));
          //pr.expectDelim('|');
          pr.skipToken(); // plus or pipe
        end;
        mDefined := true;
        pr.expectTT(pr.TTSemi);
        exit;
      end;
    else raise Exception.Create('ketmar forgot to handle some EBS type');
  end;

  case mType of
    TType.TBool:
      begin
             if pr.eatId('true') or pr.eatId('tan') or pr.eatId('yes') then mIVal := 1
        else if pr.eatId('false') or pr.eatId('ona') or pr.eatId('no') then mIVal := 0
        else raise Exception.Create(Format('invalid bool value for field ''%s''', [mName]));
        mDefined := true;
        pr.expectTT(pr.TTSemi);
        exit;
      end;
    TType.TChar:
      begin
        if (mMaxDim = 0) then raise Exception.Create(Format('invalid string size definition for field ''%s''', [mName]));
        mSVal := pr.expectStr(true);
        if (mMaxDim < 0) then
        begin
          // single char
          if (Length(mSVal) <> 1) then raise Exception.Create(Format('invalid string size for field ''%s''', [mName]));
          mIVal := Integer(mSVal[1]);
          mSVal := '';
        end
        else
        begin
          // string
          if (Length(mSVal) > mMaxDim) then raise Exception.Create(Format('invalid string size for field ''%s''', [mName]));
        end;
        mDefined := true;
        pr.expectTT(pr.TTSemi);
        exit;
      end;
    TType.TByte:
      begin
        parseInt(-128, 127);
        pr.expectTT(pr.TTSemi);
        exit;
      end;
    TType.TUByte:
      begin
        parseInt(0, 255);
        pr.expectTT(pr.TTSemi);
        exit;
      end;
    TType.TShort:
      begin
        parseInt(-32768, 32768);
        pr.expectTT(pr.TTSemi);
        exit;
      end;
    TType.TUShort:
      begin
        parseInt(0, 65535);
        pr.expectTT(pr.TTSemi);
        exit;
      end;
    TType.TInt:
      begin
        parseInt(Integer($80000000), $7fffffff);
        pr.expectTT(pr.TTSemi);
        exit;
      end;
    TType.TUInt:
      begin
        parseInt(0, $7fffffff); //FIXME
        pr.expectTT(pr.TTSemi);
        exit;
      end;
    TType.TString:
      begin
        mSVal := pr.expectStr(true);
        mDefined := true;
        pr.expectTT(pr.TTSemi);
        exit;
      end;
    TType.TPoint,
    TType.TSize:
      begin
        pr.expectDelim('(');
        mIVal := pr.expectInt();
        if (mType = TType.TPoint) then
        begin
          if (mIVal < -32768) or (mIVal > 32767) then raise Exception.Create(Format('invalid %s value for field ''%s''', [getTypeName(mType), mName]));
        end
        else
        begin
          if (mIVal < 0) or (mIVal > 32767) then raise Exception.Create(Format('invalid %s value for field ''%s''', [getTypeName(mType), mName]));
        end;
        mIVal2 := pr.expectInt();
        if (mType = TType.TPoint) then
        begin
          if (mIVal2 < -32768) or (mIVal2 > 32767) then raise Exception.Create(Format('invalid %s value for field ''%s''', [getTypeName(mType), mName]));
        end
        else
        begin
          if (mIVal2 < 0) or (mIVal2 > 32767) then raise Exception.Create(Format('invalid %s value for field ''%s''', [getTypeName(mType), mName]));
        end;
        mDefined := true;
        pr.expectDelim(')');
        pr.expectTT(pr.TTSemi);
        exit;
      end;
    TType.TList:
      begin
        assert(false);
        exit;
      end;
    TType.TTrigData:
      begin
        assert(false);
        exit;
      end;
    else raise Exception.Create('ketmar forgot to handle some field type');
  end;
  raise Exception.Create(Format('cannot parse field ''%s'' yet', [mName]));
end;


procedure TDynField.parseBinValue (st: TStream);
begin
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TDynRecord.Create (pr: TTextParser);
begin
  if (pr = nil) then raise Exception.Create('cannot create record type without type definition');
  mId := '';
  mName := '';
  mSize := 0;
  mFields := nil;
  mTrigTypes := nil;
  mHeader := false;
  mHeaderRec := nil;
  mBinBlock := -1;
  parseDef(pr);
end;


constructor TDynRecord.Create ();
begin
  mName := '';
  mSize := 0;
  mFields := nil;
  mTrigTypes := nil;
  mHeader := false;
  mHeaderRec := nil;
end;


destructor TDynRecord.Destroy ();
begin
  mName := '';
  mFields := nil;
  mTrigTypes := nil;
  mHeaderRec := nil;
  inherited;
end;


function TDynRecord.findByName (const aname: AnsiString): Integer; inline;
begin
  result := 0;
  while (result < Length(mFields)) do
  begin
    if (CompareText(aname, mFields[result].mName) = 0) then exit;
    Inc(result);
  end;
  result := -1;
end;


function TDynRecord.hasByName (const aname: AnsiString): Boolean; inline;
begin
  result := (findByName(aname) >= 0);
end;


function TDynRecord.getFieldByName (const aname: AnsiString): TDynField; inline;
var
  f: Integer;
begin
  f := findByName(aname);
  if (f >= 0) then result := mFields[f] else result := nil;
end;


function TDynRecord.getIsTrigData (): Boolean; inline;
begin
  result := (Length(mTrigTypes) > 0);
end;


function TDynRecord.getIsForTrig (const aname: AnsiString): Boolean; inline;
var
  f: Integer;
begin
  result := true;
  for f := 0 to High(mTrigTypes) do if (CompareText(mTrigTypes[f], aname) = 0) then exit;
  result := false;
end;


function TDynRecord.clone (): TDynRecord;
var
  f: Integer;
begin
  result := TDynRecord.Create();
  result.mOwner := mOwner;
  result.mId := mId;
  result.mPasName := mPasName;
  result.mName := mName;
  result.mSize := mSize;
  result.mHeader := mHeader;
  result.mHeaderRec := mHeaderRec;
  result.mBinBlock := mBinBlock;
  SetLength(result.mFields, Length(mFields));
  for f := 0 to High(mFields) do
  begin
    result.mFields[f] := mFields[f].clone();
    result.mFields[f].mOwner := result;
  end;
  SetLength(result.mTrigTypes, Length(mTrigTypes));
  for f := 0 to High(mTrigTypes) do result.mTrigTypes[f] := mTrigTypes[f];
end;


function TDynRecord.findRecordByTypeId (const atypename, aid: AnsiString): TDynRecord;
var
  fld: TDynField;
  f: Integer;
begin
  result := nil;
  if (Length(aid) = 0) then exit;
  // find record data
  fld := mHeaderRec.field[atypename];
  if (fld = nil) then exit;
  if (fld.mType <> fld.TType.TList) then raise Exception.Create(Format('cannot get record of type ''%s'' due to name conflict with ordinary field', [atypename]));
  // find by id
  for f := 0 to High(fld.mRVal) do
  begin
    if (CompareText(fld.mRVal[f].mId, aid) = 0) then begin result := fld.mRVal[f]; exit; end;
  end;
  // alas
end;


function TDynRecord.findRecordNumByType (const atypename: AnsiString; rc: TDynRecord): Integer;
var
  fld: TDynField;
  f: Integer;
begin
  result := -1;
  // find record data
  fld := mHeaderRec.field[atypename];
  if (fld = nil) then exit;
  if (fld.mType <> fld.TType.TList) then raise Exception.Create(Format('cannot get record of type ''%s'' due to name conflict with ordinary field', [atypename]));
  // find by ref
  for f := 0 to High(fld.mRVal) do
  begin
    if (fld.mRVal[f] = rc) then begin result := f; exit; end;
  end;
  // alas
end;


procedure TDynRecord.addRecordByType (const atypename: AnsiString; rc: TDynRecord);
var
  fld: TDynField;
begin
  // find record data
  fld := mHeaderRec.field[atypename];
  if (fld = nil) then
  begin
    // first record
    fld := TDynField.Create(atypename, TDynField.TType.TList);
    fld.mOwner := mHeaderRec;
    SetLength(mHeaderRec.mFields, Length(mHeaderRec.mFields)+1);
    mHeaderRec.mFields[High(mHeaderRec.mFields)] := fld;
  end;
  if (fld.mType <> fld.TType.TList) then raise Exception.Create(Format('cannot append record of type ''%s'' due to name conflict with ordinary field', [atypename]));
  // append
  SetLength(fld.mRVal, Length(fld.mRVal)+1);
  fld.mRVal[High(fld.mRVal)] := rc;
end;


procedure TDynRecord.parseDef (pr: TTextParser);
var
  fld: TDynField;
  tdn: AnsiString;
begin
  if pr.eatId('TriggerData') then
  begin
    pr.expectId('for');
    if pr.eatDelim('(') then
    begin
      while true do
      begin
        while pr.eatTT(pr.TTComma) do begin end;
        if pr.eatDelim(')') then break;
        tdn := pr.expectId();
        if isForTrig[tdn] then raise Exception.Create(Format('duplicate trigdata ''%s'' trigtype ''%s''', [mName, tdn]));
        SetLength(mTrigTypes, Length(mTrigTypes)+1);
        mTrigTypes[High(mTrigTypes)] := tdn;
      end;
    end
    else
    begin
      tdn := pr.expectId();
      SetLength(mTrigTypes, 1);
      mTrigTypes[0] := tdn;
    end;
  end
  else
  begin
    mPasName := pr.expectId(); // pascal record name
    pr.expectId('is');
    mName := pr.expectStr();
    while (pr.tokType <> pr.TTBegin) do
    begin
      if pr.eatId('header') then begin mHeader := true; continue; end;
      if pr.eatId('size') then
      begin
        if (mSize > 0) then raise Exception.Create(Format('duplicate `size` in record ''%s''', [mName]));
        mSize := pr.expectInt();
        if (mSize < 1) then raise Exception.Create(Format('invalid record ''%s'' size: %d', [mName, mSize]));
        pr.expectId('bytes');
        continue;
      end;
      if pr.eatId('binblock') then
      begin
        if (mBinBlock >= 0) then raise Exception.Create(Format('duplicate `binblock` in record ''%s''', [mName]));
        mBinBlock := pr.expectInt();
        if (mBinBlock < 1) then raise Exception.Create(Format('invalid record ''%s'' binblock: %d', [mName, mBinBlock]));
        continue;
      end;
    end;
  end;

  pr.expectTT(pr.TTBegin);
  // load fields
  while (pr.tokType <> pr.TTEnd) do
  begin
    fld := TDynField.Create(pr);
    if hasByName(fld.name) then begin fld.Free(); raise Exception.Create(Format('duplicate field ''%s''', [fld.name])); end;
    // append
    fld.mOwner := self;
    SetLength(mFields, Length(mFields)+1);
    mFields[High(mFields)] := fld;
    // done with field
    //writeln('DEF: ', fld.definition);
  end;
  pr.expectTT(pr.TTEnd);
end;


function TDynRecord.definition (): AnsiString;
var
  f: Integer;
begin
  if isTrigData then
  begin
    // trigger data
    result := 'TriggerData for ';
    if (Length(mTrigTypes) > 1) then
    begin
      result += '(';
      for f := 0 to High(mTrigTypes) do
      begin
        if (f <> 0) then result += ', ';
        result += mTrigTypes[f];
      end;
      result += ')';
    end
    else
    begin
      result += mTrigTypes[0];
    end;
  end
  else
  begin
    // record
    result := mPasName+' is '+TTextParser.quote(mName);
    if (mSize >= 0) then result += Format(' size %d bytes', [mSize]);
    if mHeader then result += ' header';
  end;
  result += ' {'#10;
  for f := 0 to High(mFields) do
  begin
    result += '  ';
    result += mFields[f].definition;
    result += ';'#10;
  end;
  result += '}';
end;


procedure TDynRecord.writeBinTo (st: TStream; trigbufsz: Integer=-1);
var
  fld: TDynField;
  rec: TDynRecord;
  buf: PByte = nil;
  ws: TStream = nil;
  blk, blkmax: Integer;
  f, c: Integer;
  bufsz: Integer = 0;
  blksz: Integer;
begin
  if (trigbufsz < 0) then
  begin
    if (mBinBlock < 1) then raise Exception.Create('cannot write binary record without block number');
    if (mSize < 1) then raise Exception.Create('cannot write binary record without size');
    bufsz := mSize;
  end
  else
  begin
    bufsz := trigbufsz;
  end;
  try
    GetMem(buf, bufsz);
    FillChar(buf^, bufsz, 0);
    ws := TSFSMemoryChunkStream.Create(buf, bufsz);

    // write normal fields
    for f := 0 to High(mFields) do
    begin
      fld := mFields[f];
      // record list?
      if (fld.mType = fld.TType.TList) then continue; // later
      if fld.mInternal then continue;
      if (fld.mBinOfs < 0) then continue;
      if (fld.mBinOfs >= bufsz) then raise Exception.Create('binary value offset is outside of the buffer');
      TSFSMemoryChunkStream(ws).setup(buf+fld.mBinOfs, bufsz-fld.mBinOfs);
      //writeln('writing field <', fld.mName, '>');
      fld.writeBinTo(ws);
    end;

    // write block with normal fields
    if mHeader then
    begin
      writeln('writing header...');
      // signature and version
      writeIntBE(st, LongWord($4D415001));
      writeInt(st, Byte(mBinBlock)); // type
      writeInt(st, LongWord(0)); // reserved
      writeInt(st, LongWord(bufsz)); // size
    end;
    st.WriteBuffer(buf^, bufsz);

    ws.Free(); ws := nil;
    FreeMem(buf); buf := nil;

    // write other blocks, if any
    if mHeader then
    begin
      // calculate blkmax
      blkmax := 0;
      for f := 0 to High(mFields) do
      begin
        fld := mFields[f];
        // record list?
        if (fld.mType = fld.TType.TList) then
        begin
          if (Length(fld.mRVal) = 0) then continue;
          rec := mOwner.findRecType(fld.mName);
          if (rec = nil) then continue;
          if (rec.mBinBlock <= 0) then continue;
          if (blkmax < rec.mBinBlock) then blkmax := rec.mBinBlock;
        end;
      end;
      // write blocks
      for blk := 1 to blkmax do
      begin
        if (blk = mBinBlock) then continue;
        ws := nil;
        for f := 0 to High(mFields) do
        begin
          fld := mFields[f];
          // record list?
          if (fld.mType = fld.TType.TList) then
          begin
            if (Length(fld.mRVal) = 0) then continue;
            rec := mOwner.findRecType(fld.mName);
            if (rec = nil) then continue;
            if (rec.mBinBlock <> blk) then continue;
            if (ws = nil) then ws := TMemoryStream.Create();
            //rec.writeBinTo(ws);
            for c := 0 to High(fld.mRVal) do fld.mRVal[c].writeBinTo(ws);
          end;
        end;
        // flush block
        if (ws <> nil) then
        begin
          blksz := Integer(ws.position);
          ws.position := 0;
          writeInt(st, Byte(blk)); // type
          writeInt(st, LongWord(0)); // reserved
          writeInt(st, LongWord(blksz)); // size
          st.CopyFrom(ws, blksz);
          ws.Free();
          ws := nil;
        end;
      end;
    end;
  finally
    ws.Free();
    if (buf <> nil) then FreeMem(buf);
  end;
end;


procedure TDynRecord.writeTo (wr: TTextWriter; putHeader: Boolean=true);
var
  f, c: Integer;
  fld: TDynField;
begin
  if putHeader then
  begin
    wr.put(mName);
    if (Length(mId) > 0) then begin wr.put(' '); wr.put(mId); end;
    wr.put(' ');
  end;
  wr.put('{'#10);
  wr.indent();
  try
    for f := 0 to High(mFields) do
    begin
      fld := mFields[f];
      // record list?
      if (fld.mType = fld.TType.TList) then
      begin
        if not mHeader then raise Exception.Create('record list in non-header record');
        for c := 0 to High(fld.mRVal) do
        begin
          if (Length(fld.mRVal[c].mId) = 0) then continue;
          wr.putIndent();
          fld.mRVal[c].writeTo(wr, true);
        end;
        continue;
      end;
      if fld.mInternal then continue;
      if fld.mOmitDef and fld.isDefaultValue then continue;
      wr.putIndent();
      fld.writeTo(wr);
    end;
  finally
    wr.unindent();
  end;
  wr.putIndent();
  wr.put('}'#10);
end;


procedure TDynRecord.parseValue (pr: TTextParser);
var
  f, c: Integer;
  fld: TDynField;
  rec, trc: TDynRecord;
  //success: Boolean;
begin
  if (mOwner = nil) then raise Exception.Create(Format('can''t parse record ''%s'' value without owner', [mName]));

  // not a header?
  if not mHeader then
  begin
    // id?
    if (pr.tokType = pr.TTId) then mId := pr.expectId();
  end
  else
  begin
    assert(mHeaderRec = self);
  end;

  //writeln('parsing record <', mName, '>');
  pr.expectTT(pr.TTBegin);
  while (pr.tokType <> pr.TTEnd) do
  begin
    if (pr.tokType <> pr.TTId) then raise Exception.Create('identifier expected');
    //writeln('<', pr.tokStr, ':', asheader, '>');

    // records
    if mHeader then
    begin
      // add records with this type (if any)
      trc := mOwner.findRecType(pr.tokStr);
      if (trc <> nil) then
      begin
        rec := trc.clone();
        rec.mHeaderRec := mHeaderRec;
        try
          pr.skipToken();
          rec.parseValue(pr);
          if (Length(rec.mId) > 0) then
          begin
            fld := field[pr.tokStr];
            if (fld <> nil) then
            begin
              for c := 0 to High(fld.mRVal) do
              begin
                if (Length(fld.mRVal[c].mId) > 0) and (CompareText(fld.mRVal[c].mId, rec.mId) = 0) then raise Exception.Create(Format('duplicate thing ''%s'' in record ''%s''', [fld.mName, mName]));
              end;
            end;
          end;
          addRecordByType(rec.mName, rec);
          rec := nil;
        finally
          rec.Free();
        end;
        continue;
      end;
    end;

    // fields
    fld := field[pr.tokStr];
    if (fld <> nil) then
    begin
      if fld.defined then raise Exception.Create(Format('duplicate field ''%s'' in record ''%s''', [fld.mName, mName]));
      if fld.internal then raise Exception.Create(Format('internal field ''%s'' in record ''%s''', [fld.mName, mName]));
      pr.skipToken();
      fld.parseValue(pr);
      continue;
    end;

    // something is wrong
    raise Exception.Create(Format('unknown field ''%s'' in record ''%s''', [pr.tokStr, mName]));
  end;
  pr.expectTT(pr.TTEnd);
  // fix field defaults
  for f := 0 to High(mFields) do mFields[f].fixDefaultValue();
  //writeln('done parsing record <', mName, '>');
end;


procedure TDynRecord.parseBinValue (st: TStream);
begin
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TDynEBS.Create (pr: TTextParser);
begin
  cleanup();
  parseDef(pr);
end;


destructor TDynEBS.Destroy ();
begin
  cleanup();
  inherited;
end;


procedure TDynEBS.cleanup ();
begin
  mIsEnum := false;
  mName := '';
  mIds := nil;
  mVals := nil;
  mMaxName := '';
  mMaxVal := 0;
end;


function TDynEBS.findByName (const aname: AnsiString): Integer;
begin
  result := 0;
  while (result < Length(mIds)) do
  begin
    if (CompareText(aname, mIds[result]) = 0) then exit;
    Inc(result);
  end;
  result := -1;
end;


function TDynEBS.hasByName (const aname: AnsiString): Boolean; inline;
begin
  result := (findByName(aname) >= 0);
end;


function TDynEBS.getFieldByName (const aname: AnsiString): Integer; inline;
var
  f: Integer;
begin
  f := findByName(aname);
  if (f >= 0) then result := mVals[f] else result := 0;
end;


function TDynEBS.definition (): AnsiString;
var
  f, cv: Integer;
begin
  if mIsEnum then result :='enum ' else result := 'bitset ';
  result += mName;
  result += ' {'#10;
  // fields
  if mIsEnum then cv := 0 else cv := 1;
  for f := 0 to High(mIds) do
  begin
    if (mIds[f] = mMaxName) then continue;
    result += '  '+mIds[f];
    if (mVals[f] <> cv) then
    begin
      result += Format(' = %d', [mVals[f]]);
      if mIsEnum then cv := mVals[f];
      result += ','#10;
    end
    else
    begin
      result += Format(', // %d'#10, [mVals[f]]);
    end;
    if mIsEnum then Inc(cv) else if (mVals[f] = cv) then cv := cv shl 1;
  end;
  // max field
  if (Length(mMaxName) > 0) then result += '  '+mMaxName+' = MAX,'#10;
  result += '}';
end;


procedure TDynEBS.parseDef (pr: TTextParser);
var
  idname: AnsiString;
  cv, v: Integer;
  f: Integer;
  skipAdd: Boolean;
  hasV: Boolean;
begin
       if pr.eatId('enum') then mIsEnum := true
  else if pr.eatId('bitset') then mIsEnum := false
  else pr.expectId('enum');
  mName := pr.expectId();
  mMaxVal := Integer($80000000);
  if mIsEnum then cv := 0 else cv := 1;
  pr.expectTT(pr.TTBegin);
  while (pr.tokType <> pr.TTEnd) do
  begin
    idname := pr.expectId();
    for f := 0 to High(mIds) do
    begin
      if (CompareText(mIds[f], idname) = 0) then raise Exception.Create(Format('duplicate field ''%s'' in enum/bitset ''%s''', [idname, mName]));
    end;
    if (CompareText(mMaxName, idname) = 0) then raise Exception.Create(Format('duplicate field ''%s'' in enum/bitset ''%s''', [idname, mName]));
    skipAdd := false;
    hasV := false;
    v := cv;
    // has value?
    if pr.eatDelim('=') then
    begin
      if pr.eatId('MAX') then
      begin
        if (Length(mMaxName) > 0) then raise Exception.Create(Format('duplicate max field ''%s'' in enum/bitset ''%s''', [idname, mName]));
        mMaxName := idname;
        skipAdd := true;
      end
      else
      begin
        v := pr.expectInt();
          if mIsEnum then cv := v;
        hasV := true;
      end;
    end;
    // append it?
    if not skipAdd then
    begin
      // fix maxvalue
      if mIsEnum or (not hasV) then
      begin
        if (mMaxVal < v) then mMaxVal := v;
      end;
      SetLength(mIds, Length(mIds)+1);
      mIds[High(mIds)] := idname;
      SetLength(mVals, Length(mIds));
      mVals[High(mVals)] := v;
      // next cv
      if mIsEnum or (not hasV) then
      begin
        if mIsEnum then Inc(cv) else cv := cv shl 1;
      end;
    end;
    if (pr.tokType = pr.TTEnd) then break;
    pr.expectTT(pr.TTComma);
    while pr.eatTT(pr.TTComma) do begin end;
  end;
  pr.expectTT(pr.TTEnd);
  // add max field
  if (Length(mMaxName) > 0) then
  begin
    SetLength(mIds, Length(mIds)+1);
    mIds[High(mIds)] := mMaxName;
    SetLength(mVals, Length(mIds));
    mVals[High(mVals)] := mMaxVal;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TDynMapDef.Create (pr: TTextParser);
begin
  recTypes := nil;
  trigTypes := nil;
  ebsTypes := nil;
  parseDef(pr);
end;


destructor TDynMapDef.Destroy ();
var
  f: Integer;
begin
  for f := 0 to High(recTypes) do recTypes[f].Free();
  for f := 0 to High(trigTypes) do trigTypes[f].Free();
  for f := 0 to High(ebsTypes) do ebsTypes[f].Free();
  recTypes := nil;
  trigTypes := nil;
  ebsTypes := nil;
  inherited;
end;


function TDynMapDef.getHeaderRecType (): TDynRecord; inline;
begin
  if (Length(recTypes) = 0) then raise Exception.Create('no header in empty mapdef');
  result := recTypes[0];
end;


function TDynMapDef.findRecType (const aname: AnsiString): TDynRecord;
var
  f: Integer;
begin
  for f := 0 to High(recTypes) do
  begin
    if (CompareText(recTypes[f].name, aname) = 0) then begin result := recTypes[f]; exit; end;
  end;
  result := nil;
end;


function TDynMapDef.findTrigFor (const aname: AnsiString): TDynRecord;
var
  f: Integer;
begin
  for f := 0 to High(trigTypes) do
  begin
    if (trigTypes[f].isForTrig[aname]) then begin result := trigTypes[f]; exit; end;
  end;
  result := nil;
end;


function TDynMapDef.findEBSType (const aname: AnsiString): TDynEBS;
var
  f: Integer;
begin
  for f := 0 to High(ebsTypes) do
  begin
    if (CompareText(ebsTypes[f].name, aname) = 0) then begin result := ebsTypes[f]; exit; end;
  end;
  result := nil;
end;


procedure TDynMapDef.parseDef (pr: TTextParser);
var
  dr, hdr: TDynRecord;
  eb: TDynEBS;
  f, c: Integer;
begin
  hdr := nil;
  while true do
  begin
    if not pr.skipBlanks() then break;
    if (pr.tokType <> pr.TTId) then raise Exception.Create('identifier expected');

    if (pr.tokStr = 'enum') or (pr.tokStr = 'bitset') then
    begin
      eb := TDynEBS.Create(pr);
      if (findEBSType(eb.name) <> nil) then
      begin
        eb.Free();
        raise Exception.Create(Format('duplicate enum/bitset ''%s''', [eb.name]));
      end;
      eb.mOwner := self;
      SetLength(ebsTypes, Length(ebsTypes)+1);
      ebsTypes[High(ebsTypes)] := eb;
      //writeln(eb.definition); writeln;
      continue;
    end;

    if (pr.tokStr = 'TriggerData') then
    begin
      dr := TDynRecord.Create(pr);
      for f := 0 to High(dr.mTrigTypes) do
      begin
        if (findTrigFor(dr.mTrigTypes[f]) <> nil) then
        begin
          dr.Free();
          raise Exception.Create(Format('duplicate trigdata ''%s''', [dr.mTrigTypes[f]]));
        end;
      end;
      dr.mOwner := self;
      SetLength(trigTypes, Length(trigTypes)+1);
      trigTypes[High(trigTypes)] := dr;
      //writeln(dr.definition); writeln;
      continue;
    end;

    dr := TDynRecord.Create(pr);
    //writeln(dr.definition); writeln;
    if (findRecType(dr.name) <> nil) then begin dr.Free(); raise Exception.Create(Format('duplicate record ''%s''', [dr.name])); end;
    if (hdr <> nil) and (CompareText(dr.name, hdr.name) = 0) then begin dr.Free(); raise Exception.Create(Format('duplicate record ''%s''', [dr.name])); end;
    dr.mOwner := self;
    if dr.mHeader then
    begin
      if (hdr <> nil) then begin dr.Free(); raise Exception.Create(Format('duplicate header record ''%s'' (previous is ''%s'')', [dr.name, hdr.name])); end;
      hdr := dr;
    end
    else
    begin
      SetLength(recTypes, Length(recTypes)+1);
      recTypes[High(recTypes)] := dr;
    end;
  end;

  // put header record to top
  if (hdr = nil) then raise Exception.Create('header definition not found in mapdef');
  SetLength(recTypes, Length(recTypes)+1);
  for f := High(recTypes) downto 1 do recTypes[f] := recTypes[f-1];
  recTypes[0] := hdr;

  // setup header links
  for f := 0 to High(recTypes) do recTypes[f].mHeaderRec := recTypes[0];

  // setup default values
  for f := 0 to High(recTypes) do
  begin
    for c := 0 to High(recTypes[f].mFields) do
    begin
      if recTypes[f].mFields[c].mHasDefault then recTypes[f].mFields[c].parseDefaultValue();
    end;
  end;
end;


// ////////////////////////////////////////////////////////////////////////// //
function TDynMapDef.parseMap (pr: TTextParser): TDynRecord;
var
  res: TDynRecord = nil;
begin
  result := nil;
  try
    pr.expectId(headerType.name);
    res := headerType.clone();
    res.mHeaderRec := res;
    res.parseValue(pr);
    result := res;
    res := nil;
  except on E: Exception do
    begin
      res.Free();
      raise;
    end;
  end;
end;


function TDynMapDef.parseBinMap (st: TStream): TDynRecord;
begin
  result := nil;
end;


end.
