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
{.$DEFINE XDYNREC_USE_FIELDHASH} // actually, it is SLOWER with this
unit xdynrec;

interface

uses
  Variants, Classes,
  xparser, xstreams, utils, hashtable;


// ////////////////////////////////////////////////////////////////////////// //
type
  TDynMapDef = class;
  TDynRecord = class;
  TDynField = class;
  TDynEBS = class;

  TDynFieldList = specialize TSimpleList<TDynField>;
  TDynRecList = specialize TSimpleList<TDynRecord>;
  TDynEBSList = specialize TSimpleList<TDynEBS>;

  // this is base type for all scalars (and arrays)
  TDynField = class
  public
    type
      TType = (TBool, TChar, TByte, TUByte, TShort, TUShort, TInt, TUInt, TString, TPoint, TSize, TList, TTrigData);
      // TPoint: pair of Integers
      // TSize: pair of UShorts
      // TList: actually, array of records
      // TTrigData: array of mMaxDim bytes, but internally a record (mRecRef)
      // arrays of chars are pascal shortstrings (with counter in the first byte)

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
    mRVal: TDynRecList; // for list
    mRHash: THashStrInt; // id -> index in mRVal
    mRecRef: TDynRecord; // for TEBS.TRec
    mMaxDim: Integer; // for byte and char arrays; <0: not an array; 0: impossible value
    mBinOfs: Integer; // offset in binary; <0 - none
    mSepPosSize: Boolean; // for points and sizes, use separate fields
    mAsT: Boolean; // for points and sizes, use separate fields, names starts with `t`
    mDefined: Boolean;
    mHasDefault: Boolean;
    mOmitDef: Boolean;
    mInternal: Boolean;
    mNegBool: Boolean;
    mBitSetUnique: Boolean; // bitset can contain only one value
    mAsMonsterId: Boolean; // special hack for triggers: monster record number+1 in binary (so 0 means "none")
    // default value
    mDefUnparsed: AnsiString;
    mDefSVal: AnsiString; // default string value
    mDefIVal, mDefIVal2: Integer; // default integer values
    mDefRecRef: TDynRecord;
    mEBS: TEBS; // complex type type
    mEBSTypeName: AnsiString; // name of enum, bitset or record
    mEBSType: TObject; // either TDynRecord or TDynEBS; nil means "simple type"; nil for `TTrigData` too

    // for binary parser
    mRecRefId: AnsiString;

    // for userdata
    mTagInt: Integer;
    mTagPtr: Pointer;

  private
    procedure cleanup ();

    procedure parseDef (pr: TTextParser);

    procedure parseDefaultValue (); // parse `mDefUnparsed` to `mDefSVal`, `mDefIVal`, `mDefIVal2`, `mDefRecRef`
    procedure fixDefaultValue (); // this will NOT clone `mDefRecRef`
    function isDefaultValue (): Boolean;

    function getListCount (): Integer; inline;
    function getListItem (idx: Integer): TDynRecord; inline; overload;
    function getListItem (const aname: AnsiString): TDynRecord; inline; overload;

    function getRecRefIndex (): Integer;

    procedure setIVal (v: Integer); inline;

    function getVar (): Variant;
    procedure setVar (val: Variant);

  protected
    // returns `true` for duplicate record id
    function addListItem (rec: TDynRecord): Boolean; inline;

  public
    constructor Create (const aname: AnsiString; atype: TType);
    constructor Create (pr: TTextParser);
    constructor Create (const aname: AnsiString; val: Variant);
    destructor Destroy (); override;

    class function getTypeName (t: TType): AnsiString;

    function definition (): AnsiString;
    function pasdef (): AnsiString;

    function clone (newOwner: TDynRecord=nil): TDynField;

    procedure parseValue (pr: TTextParser);
    procedure parseBinValue (st: TStream);

    procedure writeTo (wr: TTextWriter);
    procedure writeBinTo (st: TStream);

    // won't work for lists
    function isSimpleEqu (fld: TDynField): Boolean;

    procedure setValue (const s: AnsiString);

    function GetEnumerator (): TDynRecList.TEnumerator; inline;

  public
    property pasname: AnsiString read mPasName;
    property name: AnsiString read mName;
    property baseType: TType read mType;
    property negbool: Boolean read mNegBool;
    property defined: Boolean read mDefined;
    property internal: Boolean read mInternal write mInternal;
    property hasTPrefix: Boolean read mAsT;
    property separatePasFields: Boolean read mSepPosSize;
    property binOfs: Integer read mBinOfs;
    property ival: Integer read mIVal write setIVal;
    property ival2: Integer read mIVal2;
    property sval: AnsiString read mSVal;
    property hasDefault: Boolean read mHasDefault;
    property defsval: AnsiString read mDefSVal;
    property ebs: TEBS read mEBS;
    property ebstype: TObject read mEBSType;
    property ebstypename: AnsiString read mEBSTypeName; // enum/bitset name
    property recref: TDynRecord read mRecRef;
    property recrefIndex: Integer read getRecRefIndex; // search for this record in header; -1: not found
    // for lists
    property count: Integer read getListCount;
    property item[idx: Integer]: TDynRecord read getListItem;
    property items[const aname: AnsiString]: TDynRecord read getListItem; default; // alas, FPC 3+ lost property overloading feature
    // userdata
    property tagInt: Integer read mTagInt write mTagInt;
    property tagPtr: Pointer read mTagPtr write mTagPtr;
    //
    property varvalue: Variant read getVar write setVar;
  end;


  // "value" header record contains TList fields, with name equal to record type
  TDynRecord = class
  private
    mOwner: TDynMapDef;
    mId: AnsiString;
    mPasName: AnsiString;
    mName: AnsiString;
    mSize: Integer;
    mFields: TDynFieldList;
    {$IF DEFINED(XDYNREC_USE_FIELDHASH)}
    mFieldsHash: THashStrInt; // id -> index in mRVal
    {$ENDIF}
    mTrigTypes: array of AnsiString; // if this is triggerdata, we'll hold list of triggers here
    mHeader: Boolean; // true for header record
    mBinBlock: Integer; // -1: none
    mHeaderRec: TDynRecord; // for "value" records this is header record with data, for "type" records this is header type record

    // for userdata
    mTagInt: Integer;
    mTagPtr: Pointer;

  private
    procedure parseDef (pr: TTextParser); // parse definition

    function findByName (const aname: AnsiString): Integer; inline;
    function hasByName (const aname: AnsiString): Boolean; inline;
    function getFieldByName (const aname: AnsiString): TDynField; inline;
    function getFieldAt (idx: Integer): TDynField; inline;
    function getCount (): Integer; inline;

    function getIsTrigData (): Boolean; inline;
    function getIsForTrig (const aname: AnsiString): Boolean; inline;

    function getForTrigCount (): Integer; inline;
    function getForTrigAt (idx: Integer): AnsiString; inline;

  protected
    function findRecordByTypeId (const atypename, aid: AnsiString): TDynRecord;
    function findRecordNumByType (const atypename: AnsiString; rc: TDynRecord): Integer;
    function addRecordByType (const atypename: AnsiString; rc: TDynRecord): Boolean; // `true`: duplicate record id

    procedure addField (fld: TDynField); inline;
    function addFieldChecked (fld: TDynField): Boolean; inline; // `true`: duplicate name

  public
    constructor Create ();
    constructor Create (pr: TTextParser); // parse definition
    destructor Destroy (); override;

    function definition (): AnsiString;
    function pasdef (): AnsiString;

    function clone (): TDynRecord;

    function isSimpleEqu (rec: TDynRecord): Boolean;

    procedure parseValue (pr: TTextParser; beginEaten: Boolean=false);
    procedure parseBinValue (st: TStream; forceData: Boolean=false);

    procedure writeTo (wr: TTextWriter; putHeader: Boolean=true);
    procedure writeBinTo (st: TStream; trigbufsz: Integer=-1; onlyFields: Boolean=false);

    // find field with `TriggerType` type
    function trigTypeField (): TDynField;

    // number of records of the given instance
    function instanceCount (const typename: AnsiString): Integer;

    //procedure setUserField (const fldname: AnsiString; v: LongInt);
    //procedure setUserField (const fldname: AnsiString; v: AnsiString);
    //procedure setUserField (const fldname: AnsiString; v: Boolean);

    function getUserVar (const aname: AnsiString): Variant;
    procedure setUserVar (const aname: AnsiString; val: Variant);

  public
    property id: AnsiString read mId; // for map parser
    property pasname: AnsiString read mPasName;
    property name: AnsiString read mName; // record name
    property size: Integer read mSize; // size in bytes
    //property fields: TDynFieldList read mFields;
    property has[const aname: AnsiString]: Boolean read hasByName;
    property count: Integer read getCount;
    property field[const aname: AnsiString]: TDynField read getFieldByName; default;
    property fieldAt[idx: Integer]: TDynField read getFieldAt;
    property isTrigData: Boolean read getIsTrigData;
    property isForTrig[const aname: AnsiString]: Boolean read getIsForTrig;
    property forTrigCount: Integer read getForTrigCount;
    property forTrigAt[idx: Integer]: AnsiString read getForTrigAt;
    property headerRec: TDynRecord read mHeaderRec;
    property isHeader: Boolean read mHeader;
    // userdata
    property tagInt: Integer read mTagInt write mTagInt;
    property tagPtr: Pointer read mTagPtr write mTagPtr;
    // userfields
    property user[const aname: AnsiString]: Variant read getUserVar write setUserVar;
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
    function pasdef (): AnsiString;

    // return empty string if not found
    function nameByValue (v: Integer): AnsiString;

  public
    property name: AnsiString read mName; // record name
    property isEnum: Boolean read mIsEnum;
    property has[const aname: AnsiString]: Boolean read hasByName;
    property field[const aname: AnsiString]: Integer read getFieldByName;
  end;


  TDynMapDef = class
  public
    recTypes: TDynRecList; // [0] is always header
    trigTypes: TDynRecList; // trigdata
    ebsTypes: TDynEBSList; // enums, bitsets

  private
    procedure parseDef (pr: TTextParser);

    function getHeaderRecType (): TDynRecord; inline;

    function getTrigTypeCount (): Integer; inline;
    function getTrigTypeAt (idx: Integer): TDynRecord; inline;

  public
    constructor Create (pr: TTextParser); // parses data definition
    destructor Destroy (); override;

    function findRecType (const aname: AnsiString): TDynRecord;
    function findTrigFor (const aname: AnsiString): TDynRecord;
    function findEBSType (const aname: AnsiString): TDynEBS;

    function pasdef (): AnsiString;
    function pasdefconst (): AnsiString;

    // creates new header record
    function parseMap (pr: TTextParser): TDynRecord;

    // creates new header record
    function parseBinMap (st: TStream): TDynRecord;

  public
    property headerType: TDynRecord read getHeaderRecType;
    property trigTypeCount: Integer read getTrigTypeCount;
    property trigType[idx: Integer]: TDynRecord read getTrigTypeAt;
  end;


{$IF DEFINED(D2D_DYNREC_PROFILER)}
procedure xdynDumpProfiles ();
{$ENDIF}


implementation

uses
  SysUtils, e_log
  {$IF DEFINED(D2D_DYNREC_PROFILER)},xprofiler{$ENDIF};


// ////////////////////////////////////////////////////////////////////////// //
function StrEqu (const a, b: AnsiString): Boolean; inline; begin result := (a = b); end;


// ////////////////////////////////////////////////////////////////////////// //
function TDynField.GetEnumerator (): TDynRecList.TEnumerator; inline;
begin
  //result := TListEnumerator.Create(mRVal);
  if (mRVal <> nil) then result := mRVal.GetEnumerator else result := TDynRecList.TEnumerator.Create(nil, 0);
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TDynField.Create (const aname: AnsiString; atype: TType);
begin
  mRVal := nil;
  mRecRef := nil;
  mRHash := nil;
  cleanup();
  mName := aname;
  mType := atype;
  if (mType = TType.TList) then
  begin
    mRVal := TDynRecList.Create();
    mRHash := hashNewStrInt();
  end;
end;


constructor TDynField.Create (pr: TTextParser);
begin
  cleanup();
  parseDef(pr);
end;


constructor TDynField.Create (const aname: AnsiString; val: Variant);
  procedure setInt32 (v: LongInt);
  begin
    case mType of
      TType.TBool:
             if (v = 0) then mIVal := 0
        else if (v = 1) then mIVal := 1
        else raise Exception.Create('cannot convert shortint variant to field value');
      TType.TByte:
             if (v >= -128) and (v <= 127) then mIVal := v
        else raise Exception.Create('cannot convert shortint variant to field value');
      TType.TUByte:
             if (v >= 0) and (v <= 255) then mIVal := v
        else raise Exception.Create('cannot convert shortint variant to field value');
      TType.TShort:
             if (v >= -32768) and (v <= 32767) then mIVal := v
        else raise Exception.Create('cannot convert shortint variant to field value');
      TType.TUShort:
             if (v >= 0) and (v <= 65535) then mIVal := v
        else raise Exception.Create('cannot convert shortint variant to field value');
      TType.TInt:
        mIVal := v;
      TType.TUInt:
        mIVal := v;
      TType.TString:
        mSVal := formatstrf('%s', [v]);
      else
        raise Exception.Create('cannot convert integral variant to field value');
    end;
  end;
begin
  mRVal := nil;
  mRecRef := nil;
  mRHash := nil;
  cleanup();
  mName := aname;
  case varType(val) of
    varEmpty: raise Exception.Create('cannot convert empty variant to field value');
    varNull: raise Exception.Create('cannot convert null variant to field value');
    varSingle: raise Exception.Create('cannot convert single variant to field value');
    varDouble: raise Exception.Create('cannot convert double variant to field value');
    varDecimal: raise Exception.Create('cannot convert decimal variant to field value');
    varCurrency: raise Exception.Create('cannot convert currency variant to field value');
    varDate: raise Exception.Create('cannot convert date variant to field value');
    varOleStr: raise Exception.Create('cannot convert olestr variant to field value');
    varStrArg: raise Exception.Create('cannot convert stdarg variant to field value');
    varString: mType := TType.TString;
    varDispatch: raise Exception.Create('cannot convert dispatch variant to field value');
    varBoolean: mType := TType.TBool;
    varVariant: raise Exception.Create('cannot convert variant variant to field value');
    varUnknown: raise Exception.Create('cannot convert unknown variant to field value');
    varByte: mType := TType.TUByte;
    varWord: mType := TType.TUShort;
    varShortInt: mType := TType.TByte;
    varSmallint: mType := TType.TShort;
    varInteger: mType := TType.TInt;
    varInt64: raise Exception.Create('cannot convert int64 variant to field value');
    varLongWord: raise Exception.Create('cannot convert longword variant to field value');
    varQWord: raise Exception.Create('cannot convert uint64 variant to field value');
    varError: raise Exception.Create('cannot convert error variant to field value');
    else raise Exception.Create('cannot convert undetermined variant to field value');
  end;
  varvalue := val;
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
  mRVal.Free();
  mRVal := nil;
  mRHash.Free();
  mRHash := nil;
  mRecRef := nil;
  mMaxDim := -1;
  mBinOfs := -1;
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
  mEBSType := nil;
  mBitSetUnique := false;
  mAsMonsterId := false;
  mNegBool := false;
  mRecRefId := '';
  mTagInt := 0;
  mTagPtr := nil;
end;


function TDynField.clone (newOwner: TDynRecord=nil): TDynField;
var
  rec: TDynRecord;
begin
  result := TDynField.Create(mName, mType);
  result.mOwner := mOwner;
  if (newOwner <> nil) then result.mOwner := newOwner else result.mOwner := mOwner;
  result.mPasName := mPasName;
  result.mName := mName;
  result.mType := mType;
  result.mIVal := mIVal;
  result.mIVal2 := mIVal2;
  result.mSVal := mSVal;
  if (mRVal <> nil) then
  begin
    if (result.mRVal = nil) then result.mRVal := TDynRecList.Create(mRVal.count);
    if (result.mRHash = nil) then result.mRHash := hashNewStrInt();
    for rec in mRVal do result.addListItem(rec.clone());
  end;
  result.mRecRef := mRecRef;
  result.mMaxDim := mMaxDim;
  result.mBinOfs := mBinOfs;
  result.mSepPosSize := mSepPosSize;
  result.mAsT := mAsT;
  result.mDefined := mDefined;
  result.mHasDefault := mHasDefault;
  result.mOmitDef := mOmitDef;
  result.mInternal := mInternal;
  result.mNegBool := mNegBool;
  result.mBitSetUnique := mBitSetUnique;
  result.mAsMonsterId := mAsMonsterId;
  result.mDefUnparsed := mDefUnparsed;
  result.mDefSVal := mDefSVal;
  result.mDefIVal := mDefIVal;
  result.mDefIVal2 := mDefIVal2;
  result.mDefRecRef := mDefRecRef;
  result.mEBS := mEBS;
  result.mEBSTypeName := mEBSTypeName;
  result.mEBSType := mEBSType;
  result.mRecRefId := mRecRefId;
  result.mTagInt := mTagInt;
  result.mTagPtr := mTagPtr;
end;


procedure TDynField.setIVal (v: Integer); inline;
begin
  //FIXME: check type
  mIVal := v;
  mDefined := true;
end;


function TDynField.getVar (): Variant;
begin
  if (mEBS = TEBS.TRec) then begin result := LongInt(getRecRefIndex); exit; end;
  case mType of
    TType.TBool: result := (mIVal <> 0);
    TType.TChar: result := mSVal;
    TType.TByte: result := ShortInt(mIVal);
    TType.TUByte: result := Byte(mIVal);
    TType.TShort: result := SmallInt(mIVal);
    TType.TUShort: result := Word(mIVal);
    TType.TInt: result := LongInt(mIVal);
    TType.TUInt: result := LongWord(mIVal);
    TType.TString: result := mSVal;
    TType.TPoint: raise Exception.Create('cannot convert point field to variant');
    TType.TSize: raise Exception.Create('cannot convert size field to variant');
    TType.TList: raise Exception.Create('cannot convert list field to variant');
    TType.TTrigData: raise Exception.Create('cannot convert trigdata field to variant');
    else result := Unassigned; raise Exception.Create('ketmar forgot to handle some field type');
  end;
end;


procedure TDynField.setVar (val: Variant);
  procedure setInt32 (v: LongInt);
  begin
    case mType of
      TType.TBool:
             if (v = 0) then mIVal := 0
        else if (v = 1) then mIVal := 1
        else raise Exception.Create('cannot convert shortint variant to field value');
      TType.TByte:
             if (v >= -128) and (v <= 127) then mIVal := v
        else raise Exception.Create('cannot convert shortint variant to field value');
      TType.TUByte:
             if (v >= 0) and (v <= 255) then mIVal := v
        else raise Exception.Create('cannot convert shortint variant to field value');
      TType.TShort:
             if (v >= -32768) and (v <= 32767) then mIVal := v
        else raise Exception.Create('cannot convert shortint variant to field value');
      TType.TUShort:
             if (v >= 0) and (v <= 65535) then mIVal := v
        else raise Exception.Create('cannot convert shortint variant to field value');
      TType.TInt:
        mIVal := v;
      TType.TUInt:
        mIVal := v;
      TType.TString:
        mSVal := formatstrf('%s', [v]);
      else
        raise Exception.Create('cannot convert integral variant to field value');
    end;
  end;
begin
  case varType(val) of
    varEmpty: raise Exception.Create('cannot convert empty variant to field value');
    varNull: raise Exception.Create('cannot convert null variant to field value');
    varSingle: raise Exception.Create('cannot convert single variant to field value');
    varDouble: raise Exception.Create('cannot convert double variant to field value');
    varDecimal: raise Exception.Create('cannot convert decimal variant to field value');
    varCurrency: raise Exception.Create('cannot convert currency variant to field value');
    varDate: raise Exception.Create('cannot convert date variant to field value');
    varOleStr: raise Exception.Create('cannot convert olestr variant to field value');
    varStrArg: raise Exception.Create('cannot convert stdarg variant to field value');
    varString:
      if (mType = TType.TChar) or (mType = TType.TString) then
      begin
        mSVal := val;
      end
      else
      begin
        raise Exception.Create('cannot convert string variant to field value');
      end;
    varDispatch: raise Exception.Create('cannot convert dispatch variant to field value');
    varBoolean:
      case mType of
        TType.TBool,
        TType.TByte,
        TType.TUByte,
        TType.TShort,
        TType.TUShort,
        TType.TInt,
        TType.TUInt:
          if val then mIVal := 1 else mIVal := 0;
        TType.TString:
          if val then mSVal := 'true' else mSVal := 'false';
        else
          raise Exception.Create('cannot convert boolean variant to field value');
      end;
    varVariant: raise Exception.Create('cannot convert variant variant to field value');
    varUnknown: raise Exception.Create('cannot convert unknown variant to field value');
    varByte,
    varWord,
    varShortInt,
    varSmallint,
    varInteger:
      setInt32(val);
    varInt64:
      if (val < Int64(LongInt($80000000))) or (val > LongInt($7FFFFFFF)) then
        raise Exception.Create('cannot convert boolean variant to field value')
      else
        mIVal := LongInt(val);
    varLongWord:
      if (val > LongWord($7FFFFFFF)) then raise Exception.Create('cannot convert longword variant to field value')
      else setInt32(Integer(val));
    varQWord: raise Exception.Create('cannot convert uint64 variant to field value');
    varError: raise Exception.Create('cannot convert error variant to field value');
    else raise Exception.Create('cannot convert undetermined variant to field value');
  end;
  mDefined := true;
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
    TType.TTrigData:
      begin
        if (mRecRef = nil) then begin result := (fld.mRecRef = nil); exit; end;
        result := mRecRef.isSimpleEqu(fld.mRecRef);
      end;
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
  if (mEBS = TEBS.TRec) then mRecRef := mDefRecRef;
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


function TDynField.getListCount (): Integer; inline;
begin
  if (mRVal <> nil) then result := mRVal.count else result := 0;
end;


function TDynField.getListItem (idx: Integer): TDynRecord; inline; overload;
begin
  if (mRVal <> nil) and (idx >= 0) and (idx < mRVal.count) then result := mRVal[idx] else result := nil;
end;


function TDynField.getListItem (const aname: AnsiString): TDynRecord; inline; overload;
var
  idx: Integer;
begin
  if (mRVal <> nil) and mRHash.get(aname, idx) then result := mRVal[idx] else result := nil;
end;


function TDynField.addListItem (rec: TDynRecord): Boolean; inline;
begin
  result := false;
  if (mRVal <> nil) then
  begin
    mRVal.append(rec);
    if (Length(rec.mId) > 0) then result := mRHash.put(rec.mId, mRVal.count-1);
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
  result := mPasName+' is '+quoteStr(mName)+' type ';
  result += getTypeName(mType);
  if (mMaxDim >= 0) then result += Format('[%d]', [mMaxDim]);
  if (mBinOfs >= 0) then result += Format(' offset %d', [mBinOfs]);
  case mEBS of
    TEBS.TNone: begin end;
    TEBS.TRec: result += ' '+mEBSTypeName;
    TEBS.TEnum: result += ' enum '+mEBSTypeName;
    TEBS.TBitSet: begin result += ' bitset '; if mBitSetUnique then result += 'unique '; result += mEBSTypeName; end;
  end;
  if mAsMonsterId then result += ' as monsterid';
  if mHasDefault and (Length(mDefUnparsed) > 0) then result += ' default '+mDefUnparsed;
  if mSepPosSize then
  begin
         if (mType = TType.TPoint) then begin if (mAsT) then result += ' as txy' else result += ' as xy'; end
    else if (mType = TType.TSize) then begin if (mAsT) then result += ' as twh' else result += ' as wh'; end;
  end;
  if mOmitDef then result += ' omitdefault';
  if mInternal then result += ' internal';
end;


function TDynField.pasdef (): AnsiString;
begin
  result := mPasName+': ';
  case mType of
    TType.TBool: result += 'Boolean;';
    TType.TChar: if (mMaxDim > 0) then result += formatstrf('Char%d;', [mMaxDim]) else result += 'Char;';
    TType.TByte: result += 'ShortInt;';
    TType.TUByte: result += 'Byte;';
    TType.TShort: result += 'SmallInt;';
    TType.TUShort: result += 'Word;';
    TType.TInt: result += 'LongInt;';
    TType.TUInt: result += 'LongWord;';
    TType.TString: result += 'AnsiString;';
    TType.TPoint:
           if mAsT then result := 'tX, tY: Integer;'
      else if mSepPosSize then result := 'X, Y: Integer;'
      else result += 'TDFPoint;';
    TType.TSize:
           if mAsT then result := 'tWidth, tHeight: Word;'
      else if mSepPosSize then result := 'Width, Height: Word;'
      else result += 'TSize;';
    TType.TList: assert(false);
    TType.TTrigData: result += formatstrf('Byte%d;', [mMaxDim]);
    else raise Exception.Create('ketmar forgot to handle some field type');
  end;
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
  asmonid: Boolean;
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
  asmonid := false;
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
    if (lmaxdim < 1) then raise Exception.Create(Format('invalid field ''%s'' array size', [fldname]));
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
      else if pr.eatId('monsterid') then begin asmonid := true; end
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

  if (lmaxdim > 0) and (mType <> TType.TChar) and (mType <> TType.TTrigData) then raise Exception.Create(Format('field ''%s'' of type ''%s'' cannot be array', [fldname, fldtype]));
  if (mType = TType.TTrigData) then
  begin
    if (lmaxdim < 1) then raise Exception.Create(Format('field ''%s'' of type ''%s'' cannot be array', [fldname, fldtype]));
    if (Length(fldrecname) > 0) then raise Exception.Create(Format('field ''%s'' of type ''%s'' cannot have another type', [fldname, fldtype]));
    lebs := TDynField.TEBS.TRec;
  end;

       if hasdefStr then self.mDefUnparsed := quoteStr(defstr)
  else if hasdefInt then self.mDefUnparsed := Format('%d', [defint])
  else if hasdefId then self.mDefUnparsed := defstr;

  self.mHasDefault := (hasdefStr or hasdefId or hasdefInt);
  self.mPasName := fldpasname;
  self.mEBS := lebs;
  self.mEBSTypeName := fldrecname;
  self.mBitSetUnique := unique;
  self.mAsMonsterId := asmonid;
  self.mMaxDim := lmaxdim;
  self.mBinOfs := fldofs;
  self.mSepPosSize := (asxy or aswh);
  self.mAsT := ast;
  self.mOmitDef := omitdef;
  self.mInternal := ainternal;
end;


function TDynField.getRecRefIndex (): Integer;
begin
  if (mRecRef = nil) then begin result := -1; exit; end;
  result := mOwner.findRecordNumByType(mEBSTypeName, mRecRef);
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
          if (mType <> TType.TTrigData) then
          begin
            raise Exception.Create(Format('record reference type ''%s'' in field ''%s'' cannot be written', [mEBSTypeName, mName]));
          end;
          // write triggerdata
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
        // record reference
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
        if (mRecRef <> nil) then
        begin
          f := mOwner.findRecordNumByType(mEBSTypeName, mRecRef);
          if (f < 0) then raise Exception.Create(Format('record reference type ''%s'' in field ''%s'' not found in record list', [mEBSTypeName, mName]));
          if mAsMonsterId then Inc(f);
          if (f > maxv) then raise Exception.Create(Format('record reference type ''%s'' in field ''%s'' has too big index', [mEBSTypeName, mName]));
        end
        else
        begin
          if mAsMonsterId then f := 0 else f := -1;
        end;
        case mType of
          TType.TByte, TType.TUByte: writeInt(st, Byte(f));
          TType.TShort, TType.TUShort: writeInt(st, SmallInt(f));
          TType.TInt, TType.TUInt: writeInt(st, LongWord(f));
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
        if not mNegBool then
        begin
          if (mIVal <> 0) then writeInt(st, Byte(1)) else writeInt(st, Byte(0));
        end
        else
        begin
          if (mIVal = 0) then writeInt(st, Byte(1)) else writeInt(st, Byte(0));
        end;
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
          s := utf2win(mSVal);
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
    TType.TPoint:
      begin
        if (mMaxDim >= 0) then raise Exception.Create(Format('pos/size array in field ''%s'' cannot be written', [mName]));
        writeInt(st, LongInt(mIVal));
        writeInt(st, LongInt(mIVal2));
        exit;
      end;
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
  es: TDynEBS = nil;
  f, mask: Integer;
  first, found: Boolean;
begin
  wr.put(mName);
  wr.put(' ');
  case mEBS of
    TEBS.TNone: begin end;
    TEBS.TRec:
      begin
        if (mRecRef = nil) then
        begin
          if (mType = TType.TTrigData) then wr.put('{}'#10) else wr.put('null;'#10);
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
        //def := mOwner.mOwner;
        //es := def.findEBSType(mEBSTypeName);
        es := nil;
        if (mEBSType <> nil) and (mEBSType is TDynEBS) then es := (mEBSType as TDynEBS);
        if (es = nil) or (not es.mIsEnum) then raise Exception.Create(Format('record enum type ''%s'' for field ''%s'' not found', [mEBSTypeName, mName]));
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
        //def := mOwner.mOwner;
        //es := def.findEBSType(mEBSTypeName);
        es := nil;
        if (mEBSType <> nil) and (mEBSType is TDynEBS) then es := (mEBSType as TDynEBS);
        if (es = nil) or es.mIsEnum then raise Exception.Create(Format('record bitset type ''%s'' for field ''%s'' not found', [mEBSTypeName, mName]));
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
        wr.put(quoteStr(mSVal));
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
        wr.put(quoteStr(mSVal));
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


procedure TDynField.parseBinValue (st: TStream);
var
  rec, rc: TDynRecord;
  tfld: TDynField;
  es: TDynEBS = nil;
  tdata: PByte = nil;
  f, mask: Integer;
  s: AnsiString;
begin
  case mEBS of
    TEBS.TNone: begin end;
    TEBS.TRec:
      begin
        // this must be triggerdata
        if (mType = TType.TTrigData) then
        begin
          assert(mMaxDim > 0);
          rec := mOwner;
          // find trigger definition
          tfld := rec.trigTypeField();
          if (tfld = nil) then raise Exception.Create(Format('triggerdata value for field ''%s'' in record ''%s'' without TriggerType field', [mName, rec.mName]));
          rc := mOwner.mOwner.findTrigFor(tfld.mSVal); // find in mapdef
          if (rc = nil) then raise Exception.Create(Format('triggerdata definition for field ''%s'' in record ''%s'' with type ''%s'' not found', [mName, rec.mName, tfld.mSVal]));
          rc := rc.clone();
          rc.mHeaderRec := mOwner.mHeaderRec;
          try
            rc.parseBinValue(st, true);
            mRecRef := rc;
            rc := nil;
          finally
            rc.Free();
          end;
          mDefined := true;
          exit;
        end
        else
        begin
          // not a trigger data
          case mType of
            TType.TByte: f := readShortInt(st);
            TType.TUByte: f := readByte(st);
            TType.TShort: f := readSmallInt(st);
            TType.TUShort: f := readWord(st);
            TType.TInt: f := readLongInt(st);
            TType.TUInt: f := readLongWord(st);
            else raise Exception.Create(Format('invalid non-numeric type ''%s'' for field ''%s'' of record ''%s''', [getTypeName(mType), mName, mEBSTypeName]));
          end;
          if mAsMonsterId then Dec(f);
          if (f < 0) then mRecRefId := '' else mRecRefId := Format('%s%d', [mEBSTypeName, f]);
        end;
        mDefined := true;
        exit;
      end;
    TEBS.TEnum,
    TEBS.TBitSet:
      begin
        assert(mMaxDim < 0);
        case mType of
          TType.TByte: f := readShortInt(st);
          TType.TUByte: f := readByte(st);
          TType.TShort: f := readSmallInt(st);
          TType.TUShort: f := readWord(st);
          TType.TInt: f := readLongInt(st);
          TType.TUInt: f := readLongWord(st);
          else raise Exception.Create(Format('invalid non-numeric type ''%s'' for field ''%s'' of record ''%s''', [getTypeName(mType), mName, mEBSTypeName]));
        end;
        es := nil;
        if (mEBSType <> nil) and (mEBSType is TDynEBS) then es := (mEBSType as TDynEBS);
        if (es = nil) or (es.mIsEnum <> (mEBS = TEBS.TEnum)) then raise Exception.Create(Format('record enum type ''%s'' for field ''%s'' not found', [mEBSTypeName, mName]));
        mIVal := f;
        // build enum/bitfield values
        if (mEBS = TEBS.TEnum) then
        begin
          mSVal := es.nameByValue(mIVal);
          if (Length(mSVal) = 0) then raise Exception.Create(Format('record enum type ''%s'' for field ''%s'' has invalid value %d', [mEBSTypeName, mName, mIVal]));
        end
        else
        begin
          // special for 'none'
          if (mIVal = 0) then
          begin
            mSVal := es.nameByValue(mIVal);
            if (Length(mSVal) = 0) then raise Exception.Create(Format('record bitset type ''%s'' for field ''%s'' has invalid value %d', [mEBSTypeName, mName, mIVal]));
          end
          else
          begin
            mSVal := '';
            mask := 1;
            while (mask <> 0) do
            begin
              if ((mIVal and mask) <> 0) then
              begin
                s := es.nameByValue(mask);
                if (Length(s) = 0) then raise Exception.Create(Format('record bitset type ''%s'' for field ''%s'' has invalid value %d', [mEBSTypeName, mName, mask]));
                if (Length(mSVal) <> 0) then mSVal += '+';
                mSVal += s;
              end;
              mask := mask shl 1;
            end;
          end;
        end;
        //writeln('ebs <', es.mName, '>: ', mSVal);
        mDefined := true;
        exit;
      end;
    else raise Exception.Create('ketmar forgot to handle some EBS type');
  end;

  case mType of
    TType.TBool:
      begin
        f := readByte(st);
        if (f <> 0) then f := 1;
        if mNegBool then f := 1-f;
        mIVal := f;
        mDefined := true;
        exit;
      end;
    TType.TChar:
      begin
        if (mMaxDim < 0) then
        begin
          mIVal := readByte(st);
        end
        else
        begin
          mSVal := '';
          GetMem(tdata, mMaxDim);
          try
            st.ReadBuffer(tdata^, mMaxDim);
            f := 0;
            while (f < mMaxDim) and (tdata[f] <> 0) do Inc(f);
            if (f > 0) then
            begin
              SetLength(mSVal, f);
              Move(tdata^, PChar(mSVal)^, f);
              mSVal := win2utf(mSVal);
            end;
          finally
            FreeMem(tdata);
          end;
        end;
        mDefined := true;
        exit;
      end;
    TType.TByte: begin mIVal := readShortInt(st); mDefined := true; exit; end;
    TType.TUByte: begin mIVal := readByte(st); mDefined := true; exit; end;
    TType.TShort: begin mIVal := readSmallInt(st); mDefined := true; exit; end;
    TType.TUShort: begin mIVal := readWord(st); mDefined := true; exit; end;
    TType.TInt: begin mIVal := readLongInt(st); mDefined := true; exit; end;
    TType.TUInt: begin mIVal := readLongWord(st); mDefined := true; exit; end;
    TType.TString:
      begin
        raise Exception.Create('cannot read strings from binaries yet');
        exit;
      end;
    TType.TPoint:
      begin
        mIVal := readLongInt(st);
        mIVal2 := readLongInt(st);
        mDefined := true;
        exit;
      end;
    TType.TSize:
      begin
        mIVal := readWord(st);
        mIVal2 := readWord(st);
        mDefined := true;
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
  edim: AnsiChar;
begin
  // if this field should contain struct, convert type and parse struct
  case mEBS of
    TEBS.TNone: begin end;
    TEBS.TRec:
      begin
        // ugly hack. sorry.
        if (mType = TType.TTrigData) then
        begin
          pr.expectTT(pr.TTBegin);
          if (pr.tokType = pr.TTEnd) then
          begin
            // '{}'
            mRecRef := nil;
            pr.expectTT(pr.TTEnd);
          end
          else
          begin
            rec := mOwner;
            // find trigger definition
            tfld := rec.trigTypeField();
            if (tfld = nil) then raise Exception.Create(Format('triggerdata value for field ''%s'' in record ''%s'' without ''type'' field', [mName, rec.mName]));
            rc := mOwner.mOwner.findTrigFor(tfld.mSVal); // find in mapdef
            if (rc = nil) then raise Exception.Create(Format('triggerdata definition for field ''%s'' in record ''%s'' with type ''%s'' not found', [mName, rec.mName, tfld.mSVal]));
            rc := rc.clone();
            rc.mHeaderRec := mOwner.mHeaderRec;
            //writeln(rc.definition);
            try
              rc.parseValue(pr, true);
              mRecRef := rc;
              rc := nil;
            finally
              rc.Free();
            end;
          end;
          mDefined := true;
          pr.eatTT(pr.TTSemi); // hack: allow (but don't require) semicolon after inline records
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
          //rec := mOwner.mOwner.findRecType(mEBSTypeName); // find in mapdef
          rec := nil;
          if (mEBSType <> nil) and (mEBSType is TDynRecord) then rec := (mEBSType as TDynRecord);
          if (rec = nil) then raise Exception.Create(Format('record type ''%s'' for field ''%s'' not found', [mEBSTypeName, mName]));
          rc := rec.clone();
          rc.mHeaderRec := mOwner.mHeaderRec;
          rc.parseValue(pr);
          mRecRef := rc;
          mDefined := true;
          if mOwner.addRecordByType(mEBSTypeName, rc) then
          begin
            //raise Exception.Create(Format('record type ''%s'' for field ''%s'' not found', [mEBSTypeName, mName]));
            e_LogWritefln('duplicate record with id ''%s'' for field ''%s'' in record ''%s''', [rc.mId, mName, mOwner.mName]);
          end;
          pr.eatTT(pr.TTSemi); // hack: allow (but don't require) semicolon after inline records
          exit;
        end;
        pr.expectTT(pr.TTBegin);
      end;
    TEBS.TEnum:
      begin
        //es := mOwner.mOwner.findEBSType(mEBSTypeName); // find in mapdef
        es := nil;
        if (mEBSType <> nil) and (mEBSType is TDynEBS) then es := (mEBSType as TDynEBS);
        if (es = nil) or (not es.mIsEnum) then raise Exception.Create(Format('record enum type ''%s'' for field ''%s'' not found', [mEBSTypeName, mName]));
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
        //es := mOwner.mOwner.findEBSType(mEBSTypeName); // find in mapdef
        es := nil;
        if (mEBSType <> nil) and (mEBSType is TDynEBS) then es := (mEBSType as TDynEBS);
        if (es = nil) or es.mIsEnum then raise Exception.Create(Format('record bitset type ''%s'' for field ''%s'' not found', [mEBSTypeName, mName]));
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
        if pr.eatDelim('[') then edim := ']' else begin pr.expectDelim('('); edim := ')'; end;
        mIVal := pr.expectInt();
        if (mType = TType.TSize) then
        begin
          if (mIVal < 0) or (mIVal > 32767) then raise Exception.Create(Format('invalid %s value for field ''%s''', [getTypeName(mType), mName]));
        end;
        mIVal2 := pr.expectInt();
        if (mType = TType.TSize) then
        begin
          if (mIVal2 < 0) or (mIVal2 > 32767) then raise Exception.Create(Format('invalid %s value for field ''%s''', [getTypeName(mType), mName]));
        end;
        mDefined := true;
        pr.expectDelim(edim);
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


// ////////////////////////////////////////////////////////////////////////// //
constructor TDynRecord.Create (pr: TTextParser);
begin
  if (pr = nil) then raise Exception.Create('cannot create record type without type definition');
  mId := '';
  mName := '';
  mSize := 0;
  mFields := TDynFieldList.Create();
  {$IF DEFINED(XDYNREC_USE_FIELDHASH)}
  mFieldsHash := hashNewStrInt();
  {$ENDIF}
  mTrigTypes := nil;
  mHeader := false;
  mHeaderRec := nil;
  mBinBlock := -1;
  mTagInt := 0;
  mTagPtr := nil;
  parseDef(pr);
end;


constructor TDynRecord.Create ();
begin
  mName := '';
  mSize := 0;
  mFields := TDynFieldList.Create();
  {$IF DEFINED(XDYNREC_USE_FIELDHASH)}
  mFieldsHash := hashNewStrInt();
  {$ENDIF}
  mTrigTypes := nil;
  mHeader := false;
  mHeaderRec := nil;
  mTagInt := 0;
  mTagPtr := nil;
end;


destructor TDynRecord.Destroy ();
begin
  mName := '';
  mFields.Free();
  mFields := nil;
  {$IF DEFINED(XDYNREC_USE_FIELDHASH)}
  mFieldsHash.Free();
  mFieldsHash := nil;
  {$ENDIF}
  mTrigTypes := nil;
  mHeaderRec := nil;
  mTagInt := 0;
  mTagPtr := nil;
  inherited;
end;


procedure TDynRecord.addField (fld: TDynField); inline;
begin
  if (fld = nil) then raise Exception.Create('cannot append nil field to record');
  mFields.append(fld);
  {$IF DEFINED(XDYNREC_USE_FIELDHASH)}
  if (Length(fld.mName) > 0) then mFieldsHash.put(fld.mName, mFields.count-1);
  {$ENDIF}
end;


function TDynRecord.addFieldChecked (fld: TDynField): Boolean; inline; // `true`: duplicate name
begin
  result := false;
  if (fld = nil) then raise Exception.Create('cannot append nil field to record');
  {$IF not DEFINED(XDYNREC_USE_FIELDHASH)}
  if (Length(fld.mName) > 0) then result := hasByName(fld.mName);
  {$ENDIF}
  mFields.append(fld);
  {$IF DEFINED(XDYNREC_USE_FIELDHASH)}
  if (Length(fld.mName) > 0) then result := mFieldsHash.put(fld.mName, mFields.count-1);
  {$ENDIF}
end;


function TDynRecord.findByName (const aname: AnsiString): Integer; inline;
begin
  {$IF DEFINED(XDYNREC_USE_FIELDHASH)}
  if not mFieldsHash.get(aname, result) then result := -1;
  {$ELSE}
  result := 0;
  while (result < mFields.count) do
  begin
    if StrEqu(aname, mFields[result].mName) then exit;
    Inc(result);
  end;
  result := -1;
  {$ENDIF}
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


function TDynRecord.getFieldAt (idx: Integer): TDynField; inline;
begin
  if (idx >= 0) and (idx < mFields.count) then result := mFields[idx] else result := nil;
end;


function TDynRecord.getCount (): Integer; inline;
begin
  result := mFields.count;
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
  for f := 0 to High(mTrigTypes) do if StrEqu(mTrigTypes[f], aname) then exit;
  result := false;
end;


function TDynRecord.getForTrigCount (): Integer; inline;
begin
  result := Length(mTrigTypes);
end;


function TDynRecord.getForTrigAt (idx: Integer): AnsiString; inline;
begin
  if (idx >= 0) and (idx < Length(mTrigTypes)) then result := mTrigTypes[idx] else result := '';
end;


function TDynRecord.clone (): TDynRecord;
var
  fld: TDynField;
  f: Integer;
begin
  result := TDynRecord.Create();
  result.mOwner := mOwner;
  result.mId := mId;
  result.mPasName := mPasName;
  result.mName := mName;
  result.mSize := mSize;
  if (mFields.count > 0) then
  begin
    result.mFields.capacity := mFields.count;
    for fld in mFields do result.addField(fld.clone(result));
  end;
  SetLength(result.mTrigTypes, Length(mTrigTypes));
  for f := 0 to High(mTrigTypes) do result.mTrigTypes[f] := mTrigTypes[f];
  result.mHeader := mHeader;
  result.mBinBlock := mBinBlock;
  result.mHeaderRec := mHeaderRec;
  result.mTagInt := mTagInt;
  result.mTagPtr := mTagPtr;
end;


function TDynRecord.findRecordByTypeId (const atypename, aid: AnsiString): TDynRecord;
var
  fld: TDynField;
  idx: Integer;
begin
  result := nil;
  if (Length(aid) = 0) then exit;
  // find record data
  fld := mHeaderRec.field[atypename];
  if (fld = nil) then exit;
  if (fld.mType <> fld.TType.TList) then raise Exception.Create(Format('cannot get record of type ''%s'' due to name conflict with ordinary field', [atypename]));
  // find by id
  if (fld.mRVal <> nil) then
  begin
    if fld.mRHash.get(aid, idx) then begin result := fld.mRVal[idx]; exit; end;
  end;
  // alas
end;


function TDynRecord.findRecordNumByType (const atypename: AnsiString; rc: TDynRecord): Integer;
var
  fld: TDynField;
  idx: Integer;
begin
  result := -1;
  // find record data
  fld := mHeaderRec.field[atypename];
  if (fld = nil) then exit;
  if (fld.mType <> fld.TType.TList) then raise Exception.Create(Format('cannot get record of type ''%s'' due to name conflict with ordinary field', [atypename]));
  // find by ref
  if (fld.mRVal <> nil) then
  begin
    for idx := 0 to fld.mRVal.count-1 do
    begin
      if (fld.mRVal[idx] = rc) then begin result := idx; exit; end;
    end;
  end;
  // alas
end;


function TDynRecord.addRecordByType (const atypename: AnsiString; rc: TDynRecord): Boolean;
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
    mHeaderRec.addField(fld);
  end;
  if (fld.mType <> fld.TType.TList) then raise Exception.Create(Format('cannot append record of type ''%s'' due to name conflict with ordinary field', [atypename]));
  // append
  if (fld.mRVal = nil) then
  begin
    fld.mRVal := TDynRecList.Create();
    fld.mRHash := hashNewStrInt();
  end;
  result := fld.addListItem(rc);
end;


function TDynRecord.isSimpleEqu (rec: TDynRecord): Boolean;
var
  f: Integer;
begin
  if (rec = nil) then begin result := false; exit; end; // self.mRecRef can't be `nil` here
  if (rec = self) then begin result := true; exit; end;
  if (mFields.count <> rec.mFields.count) then begin result := false; exit; end;
  result := false;
  for f := 0 to mFields.count-1 do
  begin
    if not mFields[f].isSimpleEqu(rec.mFields[f]) then exit;
  end;
  result := true;
end;


function TDynRecord.trigTypeField (): TDynField;
var
  fld: TDynField;
  es: TDynEBS = nil;
begin
  for fld in mFields do
  begin
    if (fld.mEBS <> TDynField.TEBS.TEnum) then continue;
    if not (fld.mEBSType is TDynEBS) then continue;
    es := (fld.mEBSType as TDynEBS);
    assert(es <> nil);
    if StrEqu(es.mName, 'TriggerType') then begin result := fld; exit; end;
  end;
  result := nil;
end;


// number of records of the given instance
function TDynRecord.instanceCount (const typename: AnsiString): Integer;
var
  fld: TDynField;
begin
  result := 0;
  fld := field[typename];
  if (fld <> nil) and (fld.mType = fld.TType.TList) then result := fld.mRVal.count;
end;


function TDynRecord.getUserVar (const aname: AnsiString): Variant;
var
  fld: TDynField;
begin
  fld := getFieldByName(aname);
  if (fld = nil) then result := Unassigned else result := fld.varvalue;
end;


procedure TDynRecord.setUserVar (const aname: AnsiString; val: Variant);
var
  fld: TDynField;
begin
  fld := getFieldByName(aname);
  if (fld = nil) then
  begin
    if (Length(aname) = 0) then raise Exception.Create('cannot create nameless user field');
    fld := TDynField.Create(aname, val);
    fld.mOwner := self;
    fld.mInternal := true;
    addField(fld);
  end
  else
  begin
    fld.varvalue := val;
  end;
end;


{
procedure TDynRecord.setUserField (const fldname: AnsiString; v: LongInt);
var
  fld: TDynField;
begin
  if (Length(fldname) = 0) then exit;
  fld := field[fldname];
  if (fld <> nil) then
  begin
    if (fld.mType <> fld.TType.TInt) or (fld.mEBS <> fld.TEBS.TNone) then
    begin
      raise Exception.Create(Format('invalid user field ''%s'' type', [fld.name]));
    end;
  end
  else
  begin
    fld := TDynField.Create(fldname, fld.TType.TInt);
    fld.mOwner := self;
    fld.mIVal := v;
    fld.mInternal := true;
    fld.mDefined := true;
    addField(fld);
  end;
end;


procedure TDynRecord.setUserField (const fldname: AnsiString; v: AnsiString);
var
  fld: TDynField;
begin
  if (Length(fldname) = 0) then exit;
  fld := field[fldname];
  if (fld <> nil) then
  begin
    if (fld.mType <> fld.TType.TString) or (fld.mEBS <> fld.TEBS.TNone) then
    begin
      raise Exception.Create(Format('invalid user field ''%s'' type', [fld.name]));
    end;
  end
  else
  begin
    fld := TDynField.Create(fldname, fld.TType.TString);
    fld.mOwner := self;
    fld.mSVal := v;
    fld.mInternal := true;
    fld.mDefined := true;
    addField(fld);
  end;
end;


procedure TDynRecord.setUserField (const fldname: AnsiString; v: Boolean);
var
  fld: TDynField;
begin
  if (Length(fldname) = 0) then exit;
  fld := field[fldname];
  if (fld <> nil) then
  begin
    if (fld.mType <> fld.TType.TBool) or (fld.mEBS <> fld.TEBS.TNone) then
    begin
      raise Exception.Create(Format('invalid user field ''%s'' type', [fld.name]));
    end;
  end
  else
  begin
    fld := TDynField.Create(fldname, fld.TType.TBool);
    fld.mOwner := self;
    fld.mIVal := Integer(v);
    fld.mInternal := true;
    fld.mDefined := true;
    addField(fld);
  end;
end;
}


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
    mName := 'TriggerData';
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
    //if hasByName(fld.name) then begin fld.Free(); raise Exception.Create(Format('duplicate field ''%s''', [fld.name])); end;
    // append
    fld.mOwner := self;
    if addFieldChecked(fld) then
    begin
      fld.Free();
      raise Exception.Create(Format('duplicate field ''%s''', [fld.name]));
    end;
    // done with field
  end;
  pr.expectTT(pr.TTEnd);
end;


function TDynRecord.pasdef (): AnsiString;
var
  fld: TDynField;
begin
  if isTrigData then
  begin
    assert(false);
    result := '';
  end
  else
  begin
    // record
    result := '  '+mPasName+' = packed record'#10;
  end;
  for fld in mFields do
  begin
    if fld.mInternal then continue;
    if (fld.mBinOfs < 0) then continue;
    result += '    '+fld.pasdef+#10;
  end;
  result += '  end;'#10;
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
    result := mPasName+' is '+quoteStr(mName);
    if (mSize >= 0) then result += Format(' size %d bytes', [mSize]);
    if mHeader then result += ' header';
  end;
  result += ' {'#10;
  for f := 0 to mFields.count-1 do
  begin
    result += '  ';
    result += mFields[f].definition;
    result += ';'#10;
  end;
  result += '}';
end;


procedure TDynRecord.parseBinValue (st: TStream; forceData: Boolean=false);
var
  sign: string[4];
  btype: Integer;
  bsize: Integer;
  buf: PByte = nil;
  loaded: array[0..255] of Boolean;
  rec, rect: TDynRecord;
  fld: TDynField;
  f: Integer;
  mst: TSFSMemoryChunkStream = nil;

  procedure linkNames (rec: TDynRecord);
  var
    fld: TDynField;
    rt: TDynRecord;
  begin
    //writeln('*** rec: ', rec.mName, '.', rec.mId, ' (', rec.mFields.count, ')');
    for fld in rec.mFields do
    begin
      if (fld.mType = TDynField.TType.TTrigData) then
      begin
        if (fld.mRecRef <> nil) then linkNames(fld.mRecRef);
        continue;
      end;
      if (Length(fld.mRecRefId) = 0) then continue;
      assert(fld.mEBSType <> nil);
      rt := findRecordByTypeId(fld.mEBSTypeName, fld.mRecRefId);
      if (rt = nil) then
      begin
        e_LogWritefln('record of type ''%s'' with id ''%s'' links to inexistant record of type ''%s'' with id ''%s''', [rec.mName, rec.mId, fld.mEBSTypeName, fld.mRecRefId], MSG_WARNING);
        //raise Exception.Create(Format('record of type ''%s'' with id ''%s'' links to inexistant record of type ''%s'' with id ''%s''', [rec.mName, rec.mId, fld.mEBSTypeName, fld.mRecRefId]));
      end;
      //writeln(' ', rec.mName, '.', rec.mId, ':', fld.mName, ' -> ', rt.mName, '.', rt.mId, ' (', fld.mEBSTypeName, '.', fld.mRecRefId, ')');
      fld.mRecRefId := '';
      fld.mRecRef := rt;
      fld.mDefined := true;
    end;
    for fld in rec.mFields do
    begin
      //writeln('  ', fld.mName);
      fld.fixDefaultValue(); // just in case
    end;
  end;

begin
  for f := 0 to High(loaded) do loaded[f] := false;
  mst := TSFSMemoryChunkStream.Create(nil, 0);
  try
    if mHeader and not forceData then
    begin
      // parse map file as sequence of blocks
      sign[0] := #4;
      st.ReadBuffer(sign[1], 4);
      if (sign <> 'MAP'#1) then raise Exception.Create('invalid binary map signature');
      // parse blocks
      while (st.position < st.size) do
      begin
        btype := readByte(st);
        if (btype = 0) then break; // no more blocks
        readLongWord(st); // reserved
        bsize := readLongInt(st);
        {$IF DEFINED(D2D_XDYN_DEBUG)}writeln('btype=', btype, '; bsize=', bsize);{$ENDIF}
        if (bsize < 0) or (bsize > $1fffffff) then raise Exception.Create(Format('block of type %d has invalid size %d', [btype, bsize]));
        if loaded[btype] then raise Exception.Create(Format('block of type %d already loaded', [btype]));
        loaded[btype] := true;
        // find record type for this block
        rect := nil;
        for rec in mOwner.recTypes do if (rec.mBinBlock = btype) then begin rect := rec; break; end;
        if (rect = nil) then raise Exception.Create(Format('block of type %d has no corresponding record', [btype]));
        //writeln('found type ''', rec.mName, ''' for block type ', btype);
        if (rec.mSize = 0) or ((bsize mod rec.mSize) <> 0) then raise Exception.Create(Format('block of type %d has invalid number of records', [btype]));
        // header?
        if (rect.mHeader) then
        begin
          if (bsize <> mSize) then raise Exception.Create(Format('header block of type %d has invalid number of records', [btype]));
          GetMem(buf, bsize);
          st.ReadBuffer(buf^, bsize);
          mst.setup(buf, mSize);
          parseBinValue(mst, true); // force parsing data
        end
        else
        begin
          // create list for this type
          fld := TDynField.Create(rec.mName, TDynField.TType.TList);
          fld.mOwner := self;
          addField(fld);
          if (bsize > 0) then
          begin
            GetMem(buf, bsize);
            st.ReadBuffer(buf^, bsize);
            for f := 0 to (bsize div rec.mSize)-1 do
            begin
              mst.setup(buf+f*rec.mSize, rec.mSize);
              rec := rect.clone();
              rec.mHeaderRec := self;
              rec.parseBinValue(mst);
              rec.mId := Format('%s%d', [rec.mName, f]);
              fld.addListItem(rec);
              //writeln('parsed ''', rec.mId, '''...');
            end;
          end;
        end;
        FreeMem(buf);
        buf := nil;
        //st.position := st.position+bsize;
      end;
      // link fields
      for fld in mFields do
      begin
        if (fld.mType <> TDynField.TType.TList) then continue;
        for rec in fld.mRVal do linkNames(rec);
      end;
      exit;
    end;

    // read fields
    if StrEqu(mName, 'TriggerData') then mSize := Integer(st.size-st.position);
    if (mSize < 1) then raise Exception.Create(Format('cannot read record of type ''%s'' with unknown size', [mName]));
    GetMem(buf, mSize);
    st.ReadBuffer(buf^, mSize);
    for fld in mFields do
    begin
      if fld.mInternal then continue;
      if (fld.mBinOfs < 0) then continue;
      if (fld.mBinOfs >= st.size) then raise Exception.Create(Format('record of type ''%s'' has invalid field ''%s''', [fld.mName]));
      mst.setup(buf+fld.mBinOfs, mSize-fld.mBinOfs);
      //writeln('parsing ''', mName, '.', fld.mName, '''...');
      fld.parseBinValue(mst);
    end;
  finally
    mst.Free();
    if (buf <> nil) then FreeMem(buf);
  end;
end;


procedure TDynRecord.writeBinTo (st: TStream; trigbufsz: Integer=-1; onlyFields: Boolean=false);
var
  fld: TDynField;
  rec, rv: TDynRecord;
  buf: PByte = nil;
  ws: TStream = nil;
  blk, blkmax: Integer;
  //f, c: Integer;
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
    for fld in mFields do
    begin
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
    if mHeader and not onlyFields then
    begin
      //writeln('writing header...');
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
    if mHeader and not onlyFields then
    begin
      // calculate blkmax
      blkmax := 0;
      for fld in mFields do
      begin
        // record list?
        if (fld.mType = fld.TType.TList) then
        begin
          if (fld.mRVal = nil) or (fld.mRVal.count = 0) then continue;
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
        for fld in mFields do
        begin
          // record list?
          if (fld.mType = fld.TType.TList) then
          begin
            if (fld.mRVal = nil) or (fld.mRVal.count = 0) then continue;
            rec := mOwner.findRecType(fld.mName);
            if (rec = nil) then continue;
            if (rec.mBinBlock <> blk) then continue;
            if (ws = nil) then ws := TMemoryStream.Create();
            for rv in fld.mRVal do rv.writeBinTo(ws);
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
      // write end marker
      writeInt(st, Byte(0));
      writeInt(st, LongWord(0));
      writeInt(st, LongWord(0));
    end;
  finally
    ws.Free();
    if (buf <> nil) then FreeMem(buf);
  end;
end;


procedure TDynRecord.writeTo (wr: TTextWriter; putHeader: Boolean=true);
var
  fld: TDynField;
  rec: TDynRecord;
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
    for fld in mFields do
    begin
      // record list?
      if (fld.mType = fld.TType.TList) then
      begin
        if not mHeader then raise Exception.Create('record list in non-header record');
        if (fld.mRVal <> nil) then
        begin
          for rec in fld.mRVal do
          begin
            if (Length(rec.mId) = 0) then continue;
            wr.putIndent();
            rec.writeTo(wr, true);
          end;
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


{$IF DEFINED(D2D_DYNREC_PROFILER)}
var
  profCloneRec: UInt64 = 0;
  profFindRecType: UInt64 = 0;
  profFieldSearching: UInt64 = 0;
  profListDupChecking: UInt64 = 0;
  profAddRecByType: UInt64 = 0;
  profFieldValParsing: UInt64 = 0;
  profFixDefaults: UInt64 = 0;
  profRecValParse: UInt64 = 0;

procedure xdynDumpProfiles ();
begin
  writeln('=== XDYNREC PROFILES ===');
  writeln('record cloning: ', profCloneRec div 1000, '.', profCloneRec mod 1000, ' milliseconds');
  writeln('findRecType   : ', profFindRecType div 1000, '.', profFindRecType mod 1000, ' milliseconds');
  writeln('field[]       : ', profFieldSearching div 1000, '.', profFieldSearching mod 1000, ' milliseconds');
  writeln('list dup check: ', profListDupChecking div 1000, '.', profListDupChecking mod 1000, ' milliseconds');
  writeln('addRecByType  : ', profAddRecByType div 1000, '.', profAddRecByType mod 1000, ' milliseconds');
  writeln('field valparse: ', profFieldValParsing div 1000, '.', profFieldValParsing mod 1000, ' milliseconds');
  writeln('fix defaults  : ', profFixDefaults div 1000, '.', profFixDefaults mod 1000, ' milliseconds');
  writeln('recvalparse   : ', profRecValParse div 1000, '.', profRecValParse mod 1000, ' milliseconds');
end;
{$ENDIF}


procedure TDynRecord.parseValue (pr: TTextParser; beginEaten: Boolean=false);
var
  fld: TDynField;
  rec: TDynRecord = nil;
  trc{, rv}: TDynRecord;
  {$IF DEFINED(D2D_DYNREC_PROFILER)}
  stt, stall: UInt64;
  {$ENDIF}
begin
  if (mOwner = nil) then raise Exception.Create(Format('can''t parse record ''%s'' value without owner', [mName]));

  {$IF DEFINED(D2D_DYNREC_PROFILER)}stall := curTimeMicro();{$ENDIF}

  // not a header?
  if not mHeader then
  begin
    // id?
    if (not beginEaten) and (pr.tokType = pr.TTId) then mId := pr.expectId();
  end
  else
  begin
    assert(mHeaderRec = self);
  end;

  //writeln('parsing record <', mName, '>');
  if not beginEaten then pr.expectTT(pr.TTBegin);
  while (pr.tokType <> pr.TTEnd) do
  begin
    if (pr.tokType <> pr.TTId) then raise Exception.Create('identifier expected');
    //writeln('<', mName, '.', pr.tokStr, '>');

    // records
    if mHeader then
    begin
      // add records with this type (if any)
      {$IF DEFINED(D2D_DYNREC_PROFILER)}stt := curTimeMicro();{$ENDIF}
      trc := mOwner.findRecType(pr.tokStr);
      {$IF DEFINED(D2D_DYNREC_PROFILER)}profFindRecType := curTimeMicro()-stt;{$ENDIF}
      if (trc <> nil) then
      begin
        {$IF DEFINED(D2D_DYNREC_PROFILER)}stt := curTimeMicro();{$ENDIF}
        rec := trc.clone();
        {$IF DEFINED(D2D_DYNREC_PROFILER)}profCloneRec := curTimeMicro()-stt;{$ENDIF}
        rec.mHeaderRec := mHeaderRec;
        try
          pr.skipToken();
          rec.parseValue(pr);
          (*
          if (Length(rec.mId) > 0) then
          begin
            {$IF DEFINED(D2D_DYNREC_PROFILER)}stt := curTimeMicro();{$ENDIF}
            fld := field[pr.tokStr];
            {$IF DEFINED(D2D_DYNREC_PROFILER)}profFieldSearching := curTimeMicro()-stt;{$ENDIF}
            (*
            if (fld <> nil) and (fld.mRVal <> nil) then
            begin
              {$IF DEFINED(D2D_DYNREC_PROFILER)}stt := curTimeMicro();{$ENDIF}
              //idtmp := trc.mName+':'+rec.mId;
              //if ids.put(idtmp, 1) then raise Exception.Create(Format('duplicate thing ''%s'' in record ''%s''', [fld.mName, mName]));
              if fld.mRHash.has(rec.mId) then raise Exception.Create(Format('duplicate thing ''%s'' in record ''%s''', [fld.mName, mName]));
              {$IF DEFINED(D2D_DYNREC_PROFILER)}profListDupChecking := curTimeMicro()-stt;{$ENDIF}
            end;
          end;
          *)
          {$IF DEFINED(D2D_DYNREC_PROFILER)}stt := curTimeMicro();{$ENDIF}
          addRecordByType(rec.mName, rec);
          {$IF DEFINED(D2D_DYNREC_PROFILER)}profAddRecByType := curTimeMicro()-stt;{$ENDIF}
          rec := nil;
        finally
          rec.Free();
        end;
        continue;
      end;
    end;

    // fields
    {$IF DEFINED(D2D_DYNREC_PROFILER)}stt := curTimeMicro();{$ENDIF}
    fld := field[pr.tokStr];
    {$IF DEFINED(D2D_DYNREC_PROFILER)}profFieldSearching := curTimeMicro()-stt;{$ENDIF}
    if (fld <> nil) then
    begin
      if fld.defined then raise Exception.Create(Format('duplicate field ''%s'' in record ''%s''', [fld.mName, mName]));
      if fld.internal then raise Exception.Create(Format('internal field ''%s'' in record ''%s''', [fld.mName, mName]));
      pr.skipToken();
      {$IF DEFINED(D2D_DYNREC_PROFILER)}stt := curTimeMicro();{$ENDIF}
      fld.parseValue(pr);
      {$IF DEFINED(D2D_DYNREC_PROFILER)}profFieldValParsing := curTimeMicro()-stt;{$ENDIF}
      continue;
    end;

    // something is wrong
    raise Exception.Create(Format('unknown field ''%s'' in record ''%s''', [pr.tokStr, mName]));
  end;
  pr.expectTT(pr.TTEnd);
  // fix field defaults
  {$IF DEFINED(D2D_DYNREC_PROFILER)}stt := curTimeMicro();{$ENDIF}
  for fld in mFields do fld.fixDefaultValue();
  {$IF DEFINED(D2D_DYNREC_PROFILER)}profFixDefaults := curTimeMicro()-stt;{$ENDIF}
  //writeln('done parsing record <', mName, '>');
  //{$IF DEFINED(D2D_DYNREC_PROFILER)}writeln('stall: ', curTimeMicro()-stall);{$ENDIF}
  {$IF DEFINED(D2D_DYNREC_PROFILER)}profRecValParse := curTimeMicro()-stall;{$ENDIF}
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
    if StrEqu(aname, mIds[result]) then exit;
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


function TDynEBS.pasdef (): AnsiString;
var
  f: Integer;
begin
  result := '// '+mName+#10'const'#10;
  // fields
  for f := 0 to High(mIds) do
  begin
    result += formatstrf('  %s = %d;'#10, [mIds[f], mVals[f]]);
  end;
end;


function TDynEBS.nameByValue (v: Integer): AnsiString;
var
  f: Integer;
begin
  for f := 0 to High(mVals) do
  begin
    if (mVals[f] = v) then begin result := mIds[f]; exit; end;
  end;
  result := '';
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
      if StrEqu(mIds[f], idname) then raise Exception.Create(Format('duplicate field ''%s'' in enum/bitset ''%s''', [idname, mName]));
    end;
    if StrEqu(mMaxName, idname) then raise Exception.Create(Format('duplicate field ''%s'' in enum/bitset ''%s''', [idname, mName]));
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
  recTypes := TDynRecList.Create();
  trigTypes := TDynRecList.Create();
  ebsTypes := TDynEBSList.Create();
  parseDef(pr);
end;


destructor TDynMapDef.Destroy ();
var
  rec: TDynRecord;
  ebs: TDynEBS;
begin
  for rec in recTypes do rec.Free();
  for rec in trigTypes do rec.Free();
  for ebs in ebsTypes do ebs.Free();
  recTypes.Free();
  trigTypes.Free();
  ebsTypes.Free();
  recTypes := nil;
  trigTypes := nil;
  ebsTypes := nil;
  inherited;
end;


function TDynMapDef.getHeaderRecType (): TDynRecord; inline;
begin
  if (recTypes.count = 0) then raise Exception.Create('no header in empty mapdef');
  result := recTypes[0];
end;


function TDynMapDef.findRecType (const aname: AnsiString): TDynRecord;
var
  rec: TDynRecord;
begin
  for rec in recTypes do
  begin
    if StrEqu(rec.name, aname) then begin result := rec; exit; end;
  end;
  result := nil;
end;


function TDynMapDef.findTrigFor (const aname: AnsiString): TDynRecord;
var
  rec: TDynRecord;
begin
  for rec in trigTypes do
  begin
    if (rec.isForTrig[aname]) then begin result := rec; exit; end;
  end;
  result := nil;
end;


function TDynMapDef.findEBSType (const aname: AnsiString): TDynEBS;
var
  ebs: TDynEBS;
begin
  for ebs in ebsTypes do
  begin
    if StrEqu(ebs.name, aname) then begin result := ebs; exit; end;
  end;
  result := nil;
end;


procedure TDynMapDef.parseDef (pr: TTextParser);
var
  rec, hdr: TDynRecord;
  eb: TDynEBS;
  f: Integer;

  // setup header links and type links
  procedure linkRecord (rec: TDynRecord);
  var
    fld: TDynField;
  begin
    rec.mHeaderRec := recTypes[0];
    for fld in rec.mFields do
    begin
      if (fld.mType = fld.TType.TTrigData) then continue;
      case fld.mEBS of
        TDynField.TEBS.TNone: begin end;
        TDynField.TEBS.TRec:
          begin
            fld.mEBSType := findRecType(fld.mEBSTypeName);
            if (fld.mEBSType = nil) then raise Exception.Create(Format('field ''%s'' of type ''%s'' has no correcponding record definition', [fld.mName, fld.mEBSTypeName]));
          end;
        TDynField.TEBS.TEnum,
        TDynField.TEBS.TBitSet:
          begin
            fld.mEBSType := findEBSType(fld.mEBSTypeName);
            if (fld.mEBSType = nil) then raise Exception.Create(Format('field ''%s'' of type ''%s'' has no correcponding enum/bitset', [fld.mName, fld.mEBSTypeName]));
            if ((fld.mEBS = TDynField.TEBS.TEnum) <> (fld.mEBSType as TDynEBS).mIsEnum) then raise Exception.Create(Format('field ''%s'' of type ''%s'' enum/bitset type conflict', [fld.mName, fld.mEBSTypeName]));
          end;
      end;
    end;
  end;

  // setup default values
  procedure fixRecordDefaults (rec: TDynRecord);
  var
    fld: TDynField;
  begin
    for fld in rec.mFields do if fld.mHasDefault then fld.parseDefaultValue();
  end;

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
      ebsTypes.append(eb);
      //writeln(eb.definition); writeln;
      continue;
    end;

    if (pr.tokStr = 'TriggerData') then
    begin
      rec := TDynRecord.Create(pr);
      for f := 0 to High(rec.mTrigTypes) do
      begin
        if (findTrigFor(rec.mTrigTypes[f]) <> nil) then
        begin
          rec.Free();
          raise Exception.Create(Format('duplicate trigdata ''%s''', [rec.mTrigTypes[f]]));
        end;
      end;
      rec.mOwner := self;
      trigTypes.append(rec);
      //writeln(dr.definition); writeln;
      continue;
    end;

    rec := TDynRecord.Create(pr);
    //writeln(dr.definition); writeln;
    if (findRecType(rec.name) <> nil) then begin rec.Free(); raise Exception.Create(Format('duplicate record ''%s''', [rec.name])); end;
    if (hdr <> nil) and StrEqu(rec.name, hdr.name) then begin rec.Free(); raise Exception.Create(Format('duplicate record ''%s''', [rec.name])); end;
    rec.mOwner := self;
    if rec.mHeader then
    begin
      if (hdr <> nil) then begin rec.Free(); raise Exception.Create(Format('duplicate header record ''%s'' (previous is ''%s'')', [rec.name, hdr.name])); end;
      hdr := rec;
    end
    else
    begin
      recTypes.append(rec);
    end;
  end;

  // put header record to top
  if (hdr = nil) then raise Exception.Create('header definition not found in mapdef');
  recTypes.append(nil);
  for f := recTypes.count-1 downto 1 do recTypes[f] := recTypes[f-1];
  recTypes[0] := hdr;

  // setup header links and type links
  for rec in recTypes do linkRecord(rec);
  for rec in trigTypes do linkRecord(rec);

  // setup default values
  for rec in recTypes do fixRecordDefaults(rec);
  for rec in trigTypes do fixRecordDefaults(rec);
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
  finally
    res.Free();
  end;
end;


function TDynMapDef.parseBinMap (st: TStream): TDynRecord;
var
  res: TDynRecord = nil;
begin
  result := nil;
  try
    res := headerType.clone();
    res.mHeaderRec := res;
    res.parseBinValue(st);
    result := res;
    res := nil;
  finally
    res.Free();
  end;
end;


function TDynMapDef.pasdef (): AnsiString;
var
  ebs: TDynEBS;
  rec: TDynRecord;
  fld: TDynField;
  needComma: Boolean;
  tn: AnsiString;
begin
  result := '';
  result += '// ////////////////////////////////////////////////////////////////////////// //'#10;
  result += '// enums and bitsets'#10;
  for ebs in ebsTypes do result += #10+ebs.pasdef();
  result += #10#10'// ////////////////////////////////////////////////////////////////////////// //'#10;
  result += '// records'#10'type'#10;
  for rec in recTypes do
  begin
    if (rec.mSize < 1) then continue;
    result += rec.pasdef();
    result += #10;
  end;
  result += #10#10'// ////////////////////////////////////////////////////////////////////////// //'#10;
  result += '// triggerdata'#10'type'#10;
  result += '  TTriggerData = record'#10;
  result += '    case Byte of'#10;
  result += '      0: (Default: Byte128);'#10;
  for rec in trigTypes do
  begin
    result += '      ';
    needComma := false;
    for tn in rec.mTrigTypes do
    begin
      if needComma then result += ', ' else needComma := true;
      result += tn;
    end;
    result += ': ('#10;
    for fld in rec.mFields do
    begin
      if fld.mInternal then continue;
      if (fld.mBinOfs < 0) then continue;
      result += '        '+fld.pasdef+#10;
    end;
    result += '      );'#10;
  end;
  result += '  end;'#10;
end;


function TDynMapDef.pasdefconst (): AnsiString;
var
  ebs: TDynEBS;
begin
  result := '';
  result += '// ////////////////////////////////////////////////////////////////////////// //'#10;
  result += '// enums and bitsets'#10;
  for ebs in ebsTypes do result += #10+ebs.pasdef();
end;


function TDynMapDef.getTrigTypeCount (): Integer; inline; begin result := trigTypes.count; end;
function TDynMapDef.getTrigTypeAt (idx: Integer): TDynRecord; inline; begin if (idx >= 0) and (idx < trigTypes.count) then result := trigTypes[idx] else result := nil; end;


end.
