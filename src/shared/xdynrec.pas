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
  SysUtils, Variants, Classes,
  xparser, xstreams, utils, hashtable, mempool;


// ////////////////////////////////////////////////////////////////////////// //
type
  TDynRecException = class(Exception)
  public
    constructor Create (const amsg: AnsiString);
    constructor CreateFmt (const afmt: AnsiString; const args: array of const);
  end;

  TDynParseException = class(TDynRecException)
  public
    tokLine, tokCol: Integer;

  public
    constructor Create (pr: TTextParser; const amsg: AnsiString);
    constructor CreateFmt (pr: TTextParser; const afmt: AnsiString; const args: array of const);
  end;


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
  TDynField = class(TPoolObject)
  public
    type
      TType = (TBool, TChar, TByte, TUByte, TShort, TUShort, TInt, TUInt, TString, TPoint, TSize, TColor, TList, TTrigData);
      // TPoint: pair of Integers
      // TSize: pair of UShorts
      // TList: actually, array of records
      // TTrigData: array of mMaxDim bytes, but internally a record (mRecRef)
      // in binary: arrays of chars are pascal shortstrings (with counter in the first byte)

  private
    type
      TEBS = (TNone, TRec, TEnum, TBitSet);

  private
    mOwner: TDynRecord; // owner record
    mName: AnsiString; // field name
    mTip: AnsiString; // short tip
    mHelp: AnsiString; // long help
    mType: TType; // field type
    mIVal: Integer; // for all integer types
    mIVal2: Integer; // for point and size
    mIVal3: Integer; // for TColor
    mIVal4: Integer; // for TColor
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
    mWriteDef: Boolean;
    mInternal: Boolean;
    mNegBool: Boolean;
    mBitSetUnique: Boolean; // bitset can contain only one value
    mAsMonsterId: Boolean; // special hack for triggers: monster record number+1 in binary (so 0 means "none")
    // default value
    mDefUnparsed: AnsiString;
    mDefSVal: AnsiString; // default string value
    mDefIVal, mDefIVal2, mDefIVal3, mDefIVal4: Integer; // default integer values
    mDefRecRef: TDynRecord;
    mEBS: TEBS; // complex type type
    mEBSTypeName: AnsiString; // name of enum, bitset or record
    mEBSType: TObject; // either TDynRecord or TDynEBS; nil means "simple type"; nil for `TTrigData` too

    // for binary parser
    mRecRefId: AnsiString;

    // for userdata
    mTagInt: Integer;
    mTagPtr: Pointer;

    // for pasgen
    mAlias: AnsiString;

  private
    procedure cleanup ();

    procedure parseDefaultValue (); // parse `mDefUnparsed` to `mDefSVal`, `mDefIVal`, `mDefIVal2`, `mDefRecRef`
    procedure fixDefaultValue (); // this will NOT clone `mDefRecRef`
    function isDefaultValue (): Boolean;

    function getListCount (): Integer; inline;
    function getListItem (idx: Integer): TDynRecord; inline; overload;
    function getListItem (const aname: AnsiString): TDynRecord; inline; overload;

    function getRecRefIndex (): Integer;

    function getVar (): Variant;
    procedure setVar (val: Variant);

    procedure setRecRef (arec: TDynRecord);

    procedure parseDef (pr: TTextParser); // parse mapdef definition
    function definition (): AnsiString; // generate mapdef definition

  protected
    // returns `true` for duplicate record id
    function addListItem (rec: TDynRecord): Boolean; inline;
    function removeListItem (const aid: AnsiString): TDynRecord; // returns nil or removed record

  public
    // get string name for the given type
    class function getTypeName (t: TType): AnsiString;

  public
    constructor Create (const aname: AnsiString; atype: TType);
    constructor Create (const aname: AnsiString; val: Variant);
    constructor Create (pr: TTextParser);
    destructor Destroy (); override;

    // clone this field; register all list records in `registerIn`
    // "registration" is required to manage record lifetime; use header record if in doubt
    // owner will be set to `newOwner`, if it is not `nil`, or to `owner`
    // for lists, cloning will clone all list members
    function clone (newOwner: TDynRecord=nil; registerIn: TDynRecord=nil): TDynField;

    // compare field values (including trigdata)
    // WARNING: won't work for lists
    function isSimpleEqu (fld: TDynField): Boolean;

    // parse string value to appropriate type and set new field value
    procedure setValue (const s: AnsiString);

    // supports `for rec in field do` (for lists)
    function GetEnumerator (): TDynRecList.TEnumerator; inline;

    function getRed (): Integer; inline;
    procedure setRed (v: Integer); inline;

    function getGreen (): Integer; inline;
    procedure setGreen (v: Integer); inline;

    function getBlue (): Integer; inline;
    procedure setBlue (v: Integer); inline;

    function getAlpha (): Integer; inline;
    procedure setAlpha (v: Integer); inline;

  public
    // text parser and writer
    procedure parseValue (pr: TTextParser);
    procedure writeTo (wr: TTextWriter);

    // binary parser and writer (DO NOT USE!)
    procedure parseBinValue (st: TStream);
    procedure writeBinTo (var hasLostData: Boolean; st: TStream);

  public
    // the following functions are here only for 'mapgen'! DO NOT USE!
    // build "alias name" for pascal code
    function palias (firstUp: Boolean=false): AnsiString;

  public
    property owner: TDynRecord read mOwner;
    property name: AnsiString read mName; // field name
    property baseType: TType read mType; // field type (base for arrays)
    property defined: Boolean read mDefined; // was field value set to something by external code?
    property internal: Boolean read mInternal write mInternal; // internal field?
    property ival: Integer read mIVal; // integer value for int field (for speed), first field (x/w) for `TPoint` and `TSize`
    property ival2: Integer read mIVal2; // for `TPoint` and `TSize`, this is second field (y/h)
    property ival3: Integer read mIVal3; // for `TColor`: blue
    property ival4: Integer read mIVal4; // for `TColor`: alpha
    property red: Integer read getRed write setRed; // for `TColor`: red
    property green: Integer read getGreen write setGreen; // for `TColor`: green
    property blue: Integer read getBlue write setBlue; // for `TColor`: blue
    property alpha: Integer read getAlpha write setAlpha; // for `TColor`: alpha
    property sval: AnsiString read mSVal; // string value for string field (for speed)
    property hasDefault: Boolean read mHasDefault; // `true` if this field has default value in mapdef
    property defsval: AnsiString read mDefSVal; // string representation of default value
    property ebs: TEBS read mEBS; // what kind of reference is this? none, enum, bitset, record
    property ebstype: TObject read mEBSType; // reference type (nil, TDynRecord, TDynEBS); WARNING: don't modify type!
    property ebstypename: AnsiString read mEBSTypeName; // enum/bitset name
    property recref: TDynRecord read mRecRef write setRecRef; // referenced record (actual one, you can modify it)
    property recrefIndex: Integer read getRecRefIndex; // index of referenced record in header; -1: not found
    // for record lists
    property count: Integer read getListCount;
    property itemAt[idx: Integer]: TDynRecord read getListItem;
    property item[const aname: AnsiString]: TDynRecord read getListItem; default; // alas, FPC 3+ lost property overloading feature
    // field value as Variant
    property value: Variant read getVar write setVar;

    property tip: AnsiString read mTip;
    property help: AnsiString read mHelp;

  public
    // userdata (you can use these properties as you want to; they won't be written or read to files)
    property tagInt: Integer read mTagInt write mTagInt;
    property tagPtr: Pointer read mTagPtr write mTagPtr;

  public
    // the following properties are here only for 'mapgen'! DO NOT USE!
    property negbool: Boolean read mNegBool;
    property hasTPrefix: Boolean read mAsT;
    property separatePasFields: Boolean read mSepPosSize;
    property binOfs: Integer read mBinOfs;
    property equToDefault: Boolean read isDefaultValue;
  end;


  // record, either with actual values, or with type definitions
  TDynRecord = class(TPoolObject)
  private
    mOwner: TDynMapDef;
    mId: AnsiString;
    mTypeName: AnsiString;
    mTip: AnsiString; // short tip
    mHelp: AnsiString; // long help
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

    mRec2Free: TDynRecList;

  private
    procedure parseDef (pr: TTextParser); // parse definition
    function definition (): AnsiString;

    function findByName (const aname: AnsiString): Integer; inline;
    function hasByName (const aname: AnsiString): Boolean; inline;
    function getFieldByName (const aname: AnsiString): TDynField; inline;
    function getFieldAt (idx: Integer): TDynField; inline;
    function getCount (): Integer; inline;

    function getIsTrigData (): Boolean; inline;
    function getIsForTrig (const aname: AnsiString): Boolean; inline;

    function getForTrigCount (): Integer; inline;
    function getForTrigAt (idx: Integer): AnsiString; inline;

    procedure regrec (rec: TDynRecord);

    function getUserVar (const aname: AnsiString): Variant;
    procedure setUserVar (const aname: AnsiString; val: Variant);

    procedure clearRefRecs (rec: TDynRecord);

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

    // clone this record; register all list records in `registerIn`
    // "registration" is required to manage record lifetime; use header record if in doubt
    // all fields are cloned too
    function clone (registerIn: TDynRecord): TDynRecord;

    // compare records (values of all fields, including trigdata)
    // WARNING: won't work for records with list fields
    function isSimpleEqu (rec: TDynRecord): Boolean;

    // find field with `TriggerType` type
    function trigTypeField (): TDynField;

    // number of records of the given instance
    function instanceCount (const atypename: AnsiString): Integer;

    // only for headers: create new record with the given type
    // will return cloned record ready for use, or `nil` on unknown type name
    // `aid` must not be empty, and must be unique
    function newTypedRecord (const atypename, aid: AnsiString): TDynRecord;

    // remove record with the given type and id
    // return `true` if record was successfully found and removed
    // this will do all necessary recref cleanup too
    // WARNING: not tested yet
    function removeTypedRecord (const atypename, aid: AnsiString): Boolean;

    //TODO:
    //  [.] API to create triggers
    //  [.] API to properly remove triggers (remove trigdata)
    //  [.] check if `removeTypedRecord()` does the right thing with inline records
    //  [.] for fields: assigning `recref` should remove previously assigned inline record (record without id)
    //  [.] other API i forgot

  public
    // text parser
    // `beginEaten`: `true` if "{" was eaten
    procedure parseValue (pr: TTextParser; beginEaten: Boolean=false);

    // text writer
    // `putHeader`: `true` to write complete header, otherwise only "{...}"
    procedure writeTo (wr: TTextWriter; putHeader: Boolean=true);

    // binary parser and writer (DO NOT USE!)
    procedure parseBinValue (st: TStream; forceData: Boolean=false);
    procedure writeBinTo (var hasLostData: Boolean; st: TStream; trigbufsz: Integer=-1; onlyFields: Boolean=false);

  public
    property mapdef: TDynMapDef read mOwner;
    property id: AnsiString read mId; // record id in text map
    property typeName: AnsiString read mTypeName; // record type name (like "panel", or "trigger")
    property has[const aname: AnsiString]: Boolean read hasByName; // do we have field with the given name?
    property count: Integer read getCount; // number of fields in this record
    property field[const aname: AnsiString]: TDynField read getFieldByName; default; // get field by name
    property fieldAt[idx: Integer]: TDynField read getFieldAt; // get field at the given index
    property isTrigData: Boolean read getIsTrigData; // is this special "TriggerData" record?
    property isForTrig[const aname: AnsiString]: Boolean read getIsForTrig; // can this "TriggerData" be used for the trigger with the given type?
    property forTrigCount: Integer read getForTrigCount; // number of trigger type names for "TriggerData"
    property forTrigAt[idx: Integer]: AnsiString read getForTrigAt; // trigger type name at the given index for "TriggerData"
    property headerRec: TDynRecord read mHeaderRec; // get header record for this one (header contains all other records, enums, bitsets, etc.)
    property isHeader: Boolean read mHeader; // is this a header record?

    property tip: AnsiString read mTip;
    property help: AnsiString read mHelp;

  public
    // user fields; user can add arbitrary custom fields
    // by default, any user field will be marked as "internal"
    // note: you can use this to manipulate non-user fields too
    property user[const aname: AnsiString]: Variant read getUserVar write setUserVar;

  public
    // userdata (you can use these properties as you want to; they won't be written or read to files)
    property tagInt: Integer read mTagInt write mTagInt;
    property tagPtr: Pointer read mTagPtr write mTagPtr;
  end;


  // bitset/enum definition
  TDynEBS = class(TPoolObject)
  private
    mOwner: TDynMapDef;
    mIsEnum: Boolean;
    mTypeName: AnsiString;
    mTip: AnsiString; // short tip
    mHelp: AnsiString; // long help
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

    function definition (): AnsiString;
    function pasdef (): AnsiString;

  public
    constructor Create (pr: TTextParser); // parse definition
    destructor Destroy (); override;

    // find name for the given value
    // return empty string if not found
    function nameByValue (v: Integer): AnsiString;

  public
    property mapdef: TDynMapDef read mOwner;
    property typeName: AnsiString read mTypeName; // enum/bitset type name
    property isEnum: Boolean read mIsEnum; // is this enum? `false` means "bitset"
    property has[const aname: AnsiString]: Boolean read hasByName;
    property field[const aname: AnsiString]: Integer read getFieldByName; default;

    property tip: AnsiString read mTip;
    property help: AnsiString read mHelp;
  end;


  // parsed "mapdef.txt"
  TDynMapDef = class(TPoolObject)
  public
    recTypes: TDynRecList; // [0] is always header
    trigTypes: TDynRecList; // trigdata
    ebsTypes: TDynEBSList; // enums, bitsets

  private
    procedure parseDef (pr: TTextParser);

    function getHeaderRecType (): TDynRecord; inline;

    function getRecTypeCount (): Integer; inline;
    function getRecTypeAt (idx: Integer): TDynRecord; inline;

    function getEBSTypeCount (): Integer; inline;
    function getEBSTypeAt (idx: Integer): TDynEBS; inline;

    function getTrigTypeCount (): Integer; inline;
    function getTrigTypeAt (idx: Integer): TDynRecord; inline;

    // creates new header record
    function parseTextMap (pr: TTextParser): TDynRecord;

    // creates new header record
    function parseBinMap (st: TStream): TDynRecord;

  public
    constructor Create (pr: TTextParser); // parses data definition
    destructor Destroy (); override;

    function findRecType (const aname: AnsiString): TDynRecord;
    function findTrigFor (const aname: AnsiString): TDynRecord;
    function findEBSType (const aname: AnsiString): TDynEBS;

  public
    // parse text or binary map, return new header record
    // WARNING! stream must be seekable
    function parseMap (st: TStream; wasBinary: PBoolean=nil): TDynRecord;

    // returns `true` if the given stream can be a map file
    // stream position is 0 on return
    // WARNING! stream must be seekable
    class function canBeMap (st: TStream): Boolean;

  public
    // the following functions are here only for 'mapgen'! DO NOT USE!
    function pasdefconst (): AnsiString;

  public
    property headerType: TDynRecord read getHeaderRecType;
    // for record types
    property recTypeCount: Integer read getRecTypeCount;
    property recTypeAt[idx: Integer]: TDynRecord read getRecTypeAt;
    property recType[const aname: AnsiString]: TDynRecord read findRecType;
    // for enum/bitset types
    property ebsTypeCount: Integer read getEBSTypeCount;
    property ebsTypeAt[idx: Integer]: TDynEBS read getEBSTypeAt;
    property ebsType[const aname: AnsiString]: TDynEBS read findEBSType;
    // for trigtypes
    property trigTypeCount: Integer read getTrigTypeCount;
    property trigTypeAt[idx: Integer]: TDynRecord read getTrigTypeAt;
    property trigTypeFor[const aname: AnsiString]: TDynRecord read findTrigFor;
  end;


{$IF DEFINED(D2D_DYNREC_PROFILER)}
procedure xdynDumpProfiles ();
{$ENDIF}

var
  DynWarningCB: procedure (const msg: AnsiString; line, col: Integer) = nil;

implementation

{$IF DEFINED(D2D_DYNREC_PROFILER)}
uses
  xprofiler;
{$ENDIF}


// ////////////////////////////////////////////////////////////////////////// //
function StrEqu (const a, b: AnsiString): Boolean; inline; begin result := (a = b); end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TDynRecException.Create (const amsg: AnsiString);
begin
  inherited Create(amsg);
end;

constructor TDynRecException.CreateFmt (const afmt: AnsiString; const args: array of const);
begin
  inherited Create(formatstrf(afmt, args));
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TDynParseException.Create (pr: TTextParser; const amsg: AnsiString);
begin
  if (pr <> nil) then begin tokLine := pr.tokLine; tokCol := pr.tokCol; end else begin tokLine := 0; tokCol := 0; end;
  inherited Create(amsg);
end;

constructor TDynParseException.CreateFmt (pr: TTextParser; const afmt: AnsiString; const args: array of const);
begin
  if (pr <> nil) then begin tokLine := pr.tokLine; tokCol := pr.tokCol; end else begin tokLine := 0; tokCol := 0; end;
  inherited Create(formatstrf(afmt, args));
end;


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
        else raise TDynRecException.Create('cannot convert shortint variant to field value');
      TType.TByte:
             if (v >= -128) and (v <= 127) then mIVal := v
        else raise TDynRecException.Create('cannot convert shortint variant to field value');
      TType.TUByte:
             if (v >= 0) and (v <= 255) then mIVal := v
        else raise TDynRecException.Create('cannot convert shortint variant to field value');
      TType.TShort:
             if (v >= -32768) and (v <= 32767) then mIVal := v
        else raise TDynRecException.Create('cannot convert shortint variant to field value');
      TType.TUShort:
             if (v >= 0) and (v <= 65535) then mIVal := v
        else raise TDynRecException.Create('cannot convert shortint variant to field value');
      TType.TInt:
        mIVal := v;
      TType.TUInt:
        mIVal := v;
      TType.TString:
        mSVal := formatstrf('%s', [v]);
      else
        raise TDynRecException.Create('cannot convert integral variant to field value');
    end;
  end;
begin
  mRVal := nil;
  mRecRef := nil;
  mRHash := nil;
  cleanup();
  mName := aname;
  case varType(val) of
    varEmpty: raise TDynRecException.Create('cannot convert empty variant to field value');
    varNull: raise TDynRecException.Create('cannot convert null variant to field value');
    varSingle: raise TDynRecException.Create('cannot convert single variant to field value');
    varDouble: raise TDynRecException.Create('cannot convert double variant to field value');
    varDecimal: raise TDynRecException.Create('cannot convert decimal variant to field value');
    varCurrency: raise TDynRecException.Create('cannot convert currency variant to field value');
    varDate: raise TDynRecException.Create('cannot convert date variant to field value');
    varOleStr: raise TDynRecException.Create('cannot convert olestr variant to field value');
    varStrArg: raise TDynRecException.Create('cannot convert stdarg variant to field value');
    varString: mType := TType.TString;
    varDispatch: raise TDynRecException.Create('cannot convert dispatch variant to field value');
    varBoolean: mType := TType.TBool;
    varVariant: raise TDynRecException.Create('cannot convert variant variant to field value');
    varUnknown: raise TDynRecException.Create('cannot convert unknown variant to field value');
    varByte: mType := TType.TUByte;
    varWord: mType := TType.TUShort;
    varShortInt: mType := TType.TByte;
    varSmallint: mType := TType.TShort;
    varInteger: mType := TType.TInt;
    varInt64: raise TDynRecException.Create('cannot convert int64 variant to field value');
    varLongWord: raise TDynRecException.Create('cannot convert longword variant to field value');
    varQWord: raise TDynRecException.Create('cannot convert uint64 variant to field value');
    varError: raise TDynRecException.Create('cannot convert error variant to field value');
    else raise TDynRecException.Create('cannot convert undetermined variant to field value');
  end;
  value := val;
end;


destructor TDynField.Destroy ();
begin
  cleanup();
  inherited;
end;


procedure TDynField.cleanup ();
begin
  mName := '';
  mTip := '';
  mHelp := '';
  mType := TType.TInt;
  mIVal := 0;
  mIVal2 := 0;
  mIVal3 := 0;
  mIVal4 := 0; // default alpha value
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
  mWriteDef := false;
  mInternal := true;
  mDefUnparsed := '';
  mDefSVal := '';
  mDefIVal := 0;
  mDefIVal2 := 0;
  mDefIVal3 := 0;
  mDefIVal4 := 0; // default value for alpha
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
  mAlias := '';
end;


function TDynField.clone (newOwner: TDynRecord=nil; registerIn: TDynRecord=nil): TDynField;
var
  rec: TDynRecord;
begin
  result := TDynField.Create(mName, mType);
  result.mOwner := mOwner;
  if (newOwner <> nil) then result.mOwner := newOwner else result.mOwner := mOwner;
  result.mName := mName;
  result.mTip := mTip;
  result.mHelp := mHelp;
  result.mType := mType;
  result.mIVal := mIVal;
  result.mIVal2 := mIVal2;
  result.mIVal3 := mIVal3;
  result.mIVal4 := mIVal4;
  result.mSVal := mSVal;
  if (mRVal <> nil) then
  begin
    if (result.mRVal = nil) then result.mRVal := TDynRecList.Create(mRVal.count);
    if (result.mRHash = nil) then result.mRHash := hashNewStrInt();
    for rec in mRVal do result.addListItem(rec.clone(registerIn));
  end;
  result.mRecRef := mRecRef;
  result.mMaxDim := mMaxDim;
  result.mBinOfs := mBinOfs;
  result.mSepPosSize := mSepPosSize;
  result.mAsT := mAsT;
  result.mDefined := mDefined;
  result.mHasDefault := mHasDefault;
  result.mWriteDef := mWriteDef;
  result.mInternal := mInternal;
  result.mNegBool := mNegBool;
  result.mBitSetUnique := mBitSetUnique;
  result.mAsMonsterId := mAsMonsterId;
  result.mDefUnparsed := mDefUnparsed;
  result.mDefSVal := mDefSVal;
  result.mDefIVal := mDefIVal;
  result.mDefIVal2 := mDefIVal2;
  result.mDefIVal3 := mDefIVal3;
  result.mDefIVal4 := mDefIVal4;
  result.mDefRecRef := mDefRecRef;
  result.mEBS := mEBS;
  result.mEBSTypeName := mEBSTypeName;
  result.mEBSType := mEBSType;
  result.mRecRefId := mRecRefId;
  result.mTagInt := mTagInt;
  result.mTagPtr := mTagPtr;
  result.mAlias := mAlias;
end;


function TDynField.palias (firstUp: Boolean=false): AnsiString;
var
  nextUp: Boolean;
  ch: AnsiChar;
begin
  if (Length(mAlias) > 0) then
  begin
    if firstUp then result := UpCase1251(mAlias[1])+Copy(mAlias, 2, Length(mAlias)-1) else result := mAlias;
  end
  else
  begin
    result := '';
    nextUp := firstUp;
    for ch in mName do
    begin
      if (ch = '_') then begin nextUp := true; continue; end;
      if nextUp then result += UpCase1251(ch) else result += ch;
      nextUp := false;
    end;
  end;
end;


procedure TDynField.setRecRef (arec: TDynRecord);
var
  trc: TDynRecord = nil;
begin
  case mEBS of
    TEBS.TNone: raise TDynRecException.CreateFmt('cannot set refrec for non-reference field ''%s''', [mName]);
    TEBS.TRec:
      begin
        if (arec <> nil) then
        begin
          if (mEBSType <> nil) and (mEBSType is TDynRecord) then trc := (mEBSType as TDynRecord);
          if (trc = nil) then raise TDynRecException.CreateFmt('cannot set refrec for field ''%s'' (type conflict: improperly initialized field)', [mName]);
          if (trc.typeName <> arec.typeName) then raise TDynRecException.CreateFmt('cannot set refrec for field ''%s'' (type conflict: expected ''%s'' got ''%s'')', [mName, trc.typeName, arec.typeName]);
        end;
        mRecRef := arec;
        mDefined := true;
        exit;
      end;
    TEBS.TEnum: raise TDynRecException.CreateFmt('cannot set refrec for enum field ''%s''', [mName]);
    TEBS.TBitSet: raise TDynRecException.CreateFmt('cannot set refrec for bitset field ''%s''', [mName]);
    else raise TDynRecException.Create('ketmar forgot to process some reftypes');
  end;
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
    TType.TPoint: raise TDynRecException.Create('cannot convert point field to variant');
    TType.TSize: raise TDynRecException.Create('cannot convert size field to variant');
    TType.TColor: raise TDynRecException.Create('cannot convert color field to variant');
    TType.TList: raise TDynRecException.Create('cannot convert list field to variant');
    TType.TTrigData: raise TDynRecException.Create('cannot convert trigdata field to variant');
    else result := Unassigned; raise TDynRecException.Create('ketmar forgot to handle some field type');
  end;
end;


procedure TDynField.setVar (val: Variant);
  procedure setInt32 (v: LongInt);
  begin
    case mType of
      TType.TBool:
             if (v = 0) then mIVal := 0
        else if (v = 1) then mIVal := 1
        else raise TDynRecException.Create('cannot convert shortint variant to field value');
      TType.TByte:
             if (v >= -128) and (v <= 127) then mIVal := v
        else raise TDynRecException.Create('cannot convert shortint variant to field value');
      TType.TUByte:
             if (v >= 0) and (v <= 255) then mIVal := v
        else raise TDynRecException.Create('cannot convert shortint variant to field value');
      TType.TShort:
             if (v >= -32768) and (v <= 32767) then mIVal := v
        else raise TDynRecException.Create('cannot convert shortint variant to field value');
      TType.TUShort:
             if (v >= 0) and (v <= 65535) then mIVal := v
        else raise TDynRecException.Create('cannot convert shortint variant to field value');
      TType.TInt:
        mIVal := v;
      TType.TUInt:
        mIVal := v;
      TType.TString:
        mSVal := formatstrf('%s', [v]);
      else
        raise TDynRecException.Create('cannot convert integral variant to field value');
    end;
  end;
begin
  case varType(val) of
    varEmpty: raise TDynRecException.Create('cannot convert empty variant to field value');
    varNull: raise TDynRecException.Create('cannot convert null variant to field value');
    varSingle: raise TDynRecException.Create('cannot convert single variant to field value');
    varDouble: raise TDynRecException.Create('cannot convert double variant to field value');
    varDecimal: raise TDynRecException.Create('cannot convert decimal variant to field value');
    varCurrency: raise TDynRecException.Create('cannot convert currency variant to field value');
    varDate: raise TDynRecException.Create('cannot convert date variant to field value');
    varOleStr: raise TDynRecException.Create('cannot convert olestr variant to field value');
    varStrArg: raise TDynRecException.Create('cannot convert stdarg variant to field value');
    varString:
      if (mType = TType.TChar) or (mType = TType.TString) then
      begin
        mSVal := val;
      end
      else
      begin
        raise TDynRecException.Create('cannot convert string variant to field value');
      end;
    varDispatch: raise TDynRecException.Create('cannot convert dispatch variant to field value');
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
          raise TDynRecException.Create('cannot convert boolean variant to field value');
      end;
    varVariant: raise TDynRecException.Create('cannot convert variant variant to field value');
    varUnknown: raise TDynRecException.Create('cannot convert unknown variant to field value');
    varByte,
    varWord,
    varShortInt,
    varSmallint,
    varInteger:
      setInt32(val);
    varInt64:
      if (val < Int64(LongInt($80000000))) or (val > LongInt($7FFFFFFF)) then
        raise TDynRecException.Create('cannot convert boolean variant to field value')
      else
        mIVal := LongInt(val);
    varLongWord:
      if (val > LongWord($7FFFFFFF)) then raise TDynRecException.Create('cannot convert longword variant to field value')
      else setInt32(Integer(val));
    varQWord: raise TDynRecException.Create('cannot convert uint64 variant to field value');
    varError: raise TDynRecException.Create('cannot convert error variant to field value');
    else raise TDynRecException.Create('cannot convert undetermined variant to field value');
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
    TType.TColor:
      result := ((mIVal = fld.mIVal) and (mIVal2 = fld.mIVal2) and (mIVal3 = fld.mIVal3) and (mIVal4 = fld.mIVal4));
    TType.TList: result := false;
    TType.TTrigData:
      begin
        if (mRecRef = nil) then begin result := (fld.mRecRef = nil); exit; end;
        result := mRecRef.isSimpleEqu(fld.mRecRef);
      end;
    else raise TDynRecException.Create('ketmar forgot to handle some field type');
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


function TDynField.getRed (): Integer; inline; begin result := mIVal; if (result < 0) then result := 0 else if (result > 255) then result := 255; end;
procedure TDynField.setRed (v: Integer); inline; begin if (v < 0) then v := 0 else if (v > 255) then v := 255; mIVal := v; end;

function TDynField.getGreen (): Integer; inline; begin result := mIVal2; if (result < 0) then result := 0 else if (result > 255) then result := 255; end;
procedure TDynField.setGreen (v: Integer); inline; begin if (v < 0) then v := 0 else if (v > 255) then v := 255; mIVal2 := v; end;

function TDynField.getBlue (): Integer; inline; begin result := mIVal3; if (result < 0) then result := 0 else if (result > 255) then result := 255; end;
procedure TDynField.setBlue (v: Integer); inline; begin if (v < 0) then v := 0 else if (v > 255) then v := 255; mIVal3 := v; end;

function TDynField.getAlpha (): Integer; inline; begin result := mIVal4; if (result < 0) then result := 0 else if (result > 255) then result := 255; end;
procedure TDynField.setAlpha (v: Integer); inline; begin if (v < 0) then v := 0 else if (v > 255) then v := 255; mIVal4 := v; end;


procedure TDynField.parseDefaultValue ();
var
  stp: TTextParser = nil;
  oSVal: AnsiString;
  oIVal, oIVal2, oIVal3, oIVal4: Integer;
  oRRef: TDynRecord;
  oDef: Boolean;
begin
  if not mHasDefault then
  begin
    mDefSVal := '';
    mDefIVal := 0;
    mDefIVal2 := 0;
    mDefIVal3 := 0;
    mDefIVal4 := 0; // default value for alpha
    mDefRecRef := nil;
  end
  else
  begin
    oSVal := mSVal;
    oIVal := mIVal;
    oIVal2 := mIVal2;
    oIVal3 := mIVal3;
    oIVal4 := mIVal4;
    oRRef := mRecRef;
    oDef := mDefined;
    try
      stp := TStrTextParser.Create(mDefUnparsed+';');
      parseValue(stp);
      //if (mType = TType.TColor) then writeln('4=[', mIVal4, ']');
      mDefSVal := mSVal;
      mDefIVal := mIVal;
      mDefIVal2 := mIVal2;
      mDefIVal3 := mIVal3;
      mDefIVal4 := mIVal4;
      mDefRecRef := mRecRef;
    finally
      mSVal := oSVal;
      mIVal := oIVal;
      mIVal2 := oIVal2;
      mIVal3 := oIVal3;
      mIVal4 := oIVal4;
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
    raise TDynRecException.CreateFmt('field ''%s'' in record ''%s'' of record type ''%s'' is not set', [mName, mOwner.mId, mOwner.mTypeName]);
  end;
  if (mEBS = TEBS.TRec) then mRecRef := mDefRecRef;
  mSVal := mDefSVal;
  mIVal := mDefIVal;
  mIVal2 := mDefIVal2;
  mIVal3 := mDefIVal3;
  mIVal4 := mDefIVal4;
  //if (mType = TType.TColor) then writeln('4=[', mDefIVal4, ']');
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
    TType.TColor: result := (mIVal = mDefIVal2) and (mIVal2 = mDefIVal2) and (mIVal3 = mDefIVal3) and (mIVal4 = mDefIVal4);
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


function TDynField.removeListItem (const aid: AnsiString): TDynRecord;
var
  f, idx: Integer;
begin
  result := nil;
  if mRHash.get(aid, idx) then
  begin
    assert((idx >= 0) and (idx < mRVal.count));
    result := mRVal[idx];
    // fix hash and list
    for f := idx+1 to mRVal.count-1 do
    begin
      if (Length(mRVal[f].mId) > 0) then mRHash.put(mRVal[f].mId, f-1);
    end;
    mRHash.del(aid);
    mRVal.delete(idx);
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
    TType.TColor: result := 'color';
    TType.TList: result := 'array';
    TType.TTrigData: result := 'trigdata';
    else raise TDynRecException.Create('ketmar forgot to handle some field type');
  end;
end;


function TDynField.definition (): AnsiString;
begin
  result := quoteStr(mName)+' type ';
  result += getTypeName(mType);
  if (Length(mAlias) > 0) then result += ' alias '+mAlias;
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
  if mWriteDef then result += ' writedefault';
  if mInternal then result += ' internal';
end;


procedure TDynField.parseDef (pr: TTextParser);
var
  fldname: AnsiString;
  fldtype: AnsiString;
  fldofs: Integer;
  fldrecname: AnsiString;
  asxy, aswh, ast: Boolean;
  ainternal: Boolean;
  writedef: Boolean;
  defstr: AnsiString;
  defint, defint2, defint3, defint4: Integer;
  hasdefStr: Boolean;
  hasdefInt: Boolean;
  hasdefId: Boolean;
  lmaxdim: Integer;
  lebs: TDynField.TEBS;
  unique: Boolean;
  asmonid: Boolean;
  defech: AnsiChar;
  xalias: AnsiString;
  atip, ahelp: AnsiString;
begin
  fldname := '';
  fldtype := '';
  fldofs := -1;
  fldrecname := '';
  asxy := false;
  aswh := false;
  ast := false;
  ainternal := false;
  writedef := false;
  defstr := '';
  defint := 0;
  defint2 := 0;
  defint3 := 0;
  defint4 := 0;
  hasdefStr := false;
  hasdefInt := false;
  hasdefId := false;
  unique := false;
  asmonid := false;
  lmaxdim := -1;
  lebs := TDynField.TEBS.TNone;
  xalias := '';
  atip := '';
  ahelp := '';

  // field name
  fldname := pr.expectStrOrId();

  while (not pr.isDelim(';')) do
  begin
    if pr.eatId('type') then
    begin
      if (Length(fldtype) > 0) then raise TDynParseException.CreateFmt(pr, 'duplicate type definition for field ''%s''', [fldname]);
      // field type
      fldtype := pr.expectId();
      // fixed-size array?
      if pr.eatDelim('[') then
      begin
        lmaxdim := pr.expectInt();
        // arbitrary limits
        if (lmaxdim < 1) or (lmaxdim > 32768) then raise TDynParseException.CreateFmt(pr, 'invalid field ''%s'' array size', [fldname]);
        pr.expectDelim(']');
      end;
      continue;
    end;

    if pr.eatId('alias') then
    begin
      if (Length(xalias) > 0) then raise TDynParseException.CreateFmt(pr, 'duplicate alias definition for field ''%s''', [fldname]);
      xalias := pr.expectId();
      continue;
    end;

    if pr.eatId('tip') then
    begin
      if (Length(atip) > 0) then raise TDynParseException.CreateFmt(pr, 'duplicate tip definition for field ''%s''', [fldname]);
      atip := pr.expectStr(false);
      continue;
    end;

    if pr.eatId('help') then
    begin
      if (Length(ahelp) > 0) then raise TDynParseException.CreateFmt(pr, 'duplicate tip definition for field ''%s''', [fldname]);
      ahelp := pr.expectStr(false);
      continue;
    end;

    if pr.eatId('offset') then
    begin
      if (fldofs >= 0) then raise TDynParseException.CreateFmt(pr, 'duplicate field ''%s'' offset', [fldname]);
      fldofs := pr.expectInt();
      if (fldofs < 0) then raise TDynParseException.CreateFmt(pr, 'invalid field ''%s'' offset', [fldname]);
      continue;
    end;

    if pr.eatId('as') then
    begin
           if pr.eatId('xy') then asxy := true
      else if pr.eatId('wh') then aswh := true
      else if pr.eatId('txy') then begin asxy := true; ast := true; end
      else if pr.eatId('twh') then begin aswh := true; ast := true; end
      else if pr.eatId('monsterid') then begin asmonid := true; end
      else raise TDynParseException.CreateFmt(pr, 'invalid field ''%s'' as what?', [fldname]);
      continue;
    end;

    if pr.eatId('enum') then
    begin
      lebs := TDynField.TEBS.TEnum;
      if (Length(fldrecname) <> 0) then raise TDynParseException.CreateFmt(pr, 'field ''%s'' already typed as ''%s''', [fldname, fldrecname]);
      fldrecname := pr.expectId();
      continue;
    end;

    if pr.eatId('bitset') then
    begin
      lebs := TDynField.TEBS.TBitSet;
      if (Length(fldrecname) <> 0) then raise TDynParseException.CreateFmt(pr, 'field ''%s'' already typed as ''%s''', [fldname, fldrecname]);
      unique := pr.eatId('unique');
      fldrecname := pr.expectId();
      continue;
    end;

    if pr.eatId('default') then
    begin
      if hasdefStr or hasdefInt or hasdefId then raise TDynParseException.CreateFmt(pr, 'field ''%s'' has duplicate default', [fldname]);
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
        pr.TTDelim:
          begin
            hasdefInt := true;
            if pr.eatDelim('[') then defech := ']' else begin pr.expectDelim('('); defech := ')'; end;
            defint := pr.expectInt();
            defint2 := pr.expectInt();
            if (pr.tokType = pr.TTInt) then
            begin
              defint3 := pr.expectInt();
              if (pr.tokType = pr.TTInt) then defint4 := pr.expectInt();
            end;
            pr.expectDelim(defech);
          end;
        else
          raise TDynParseException.CreateFmt(pr, 'field ''%s'' has invalid default', [fldname]);
      end;
      continue;
    end;

    if pr.eatId('writedefault') then
    begin
      writedef := true;
      continue;
    end;

    if pr.eatId('internal') then
    begin
      ainternal := true;
      continue;
    end;

    // record type, no special modifiers
    if (pr.tokType <> pr.TTId) then raise TDynParseException.CreateFmt(pr, 'field ''%s'' has something unexpected in definition', [fldname]);

    if (Length(fldrecname) <> 0) then raise TDynParseException.CreateFmt(pr, 'field ''%s'' already typed as ''%s''', [fldname, fldrecname]);
    fldrecname := pr.expectId();
    lebs := TDynField.TEBS.TRec;
  end;

  pr.expectDelim(';');

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
  else if (fldtype = 'color') then mType := TType.TColor
  else if (fldtype = 'trigdata') then mType := TType.TTrigData
  else
  begin
    // record types defaults to int
    if (Length(fldrecname) > 0) then
    begin
      mType := TType.TInt;
    end
    else
    begin
      if (Length(fldtype) = 0) then raise TDynParseException.CreateFmt(pr, 'field ''%s'' has no type', [fldname])
      else raise TDynParseException.CreateFmt(pr, 'field ''%s'' has invalid type ''%s''', [fldname, fldtype]);
    end;
  end;

  // check for valid arrays
  if (lmaxdim > 0) and (mType <> TType.TChar) and (mType <> TType.TTrigData) then raise TDynParseException.CreateFmt(pr, 'field ''%s'' of type ''%s'' cannot be array', [fldname, fldtype]);

  // check for valid trigdata or record type
  if (mType = TType.TTrigData) then
  begin
    // trigdata
    if (lmaxdim < 1) then raise TDynParseException.CreateFmt(pr, 'field ''%s'' of type ''%s'' cannot be non-array', [fldname, 'trigdata']);
    if (Length(fldrecname) > 0) then raise TDynParseException.CreateFmt(pr, 'field ''%s'' of type ''%s'' cannot have another type', [fldname, 'trigdata']);
    lebs := TDynField.TEBS.TRec;
  end
  else if (Length(fldrecname) > 0) then
  begin
    // record
    if not (mType in [TType.TByte, TType.TUByte, TType.TShort, TType.TUShort, TType.TInt, TType.TUInt]) then
    begin
      raise TDynParseException.CreateFmt(pr, 'field ''%s'' of record type ''%s'' cannot have type ''%s''', [fldname, fldrecname, fldtype]);
    end;
  end;

  // setup default value
       if hasdefStr then self.mDefUnparsed := quoteStr(defstr)
  else if hasdefId then self.mDefUnparsed := defstr
  else if hasdefInt then
  begin
         if (mType = TType.TPoint) then self.mDefUnparsed := Format('(%d %d)', [defint, defint2])
    else if (mType = TType.TSize) then self.mDefUnparsed := Format('[%d %d]', [defint, defint2])
    else if (mType = TType.TColor) then self.mDefUnparsed := Format('(%d %d %d %d)', [defint, defint2, defint3, defint4])
    else self.mDefUnparsed := Format('%d', [defint]);
  end;

  self.mHasDefault := (hasdefStr or hasdefId or hasdefInt);
  self.mEBS := lebs;
  self.mEBSTypeName := fldrecname;
  self.mBitSetUnique := unique;
  self.mAsMonsterId := asmonid;
  self.mMaxDim := lmaxdim;
  self.mBinOfs := fldofs;
  self.mSepPosSize := (asxy or aswh);
  self.mAsT := ast;
  self.mWriteDef := writedef;
  self.mInternal := ainternal;
  self.mAlias := xalias;
  self.mTip := atip;
  self.mHelp := ahelp;
end;


function TDynField.getRecRefIndex (): Integer;
begin
  if (mRecRef = nil) then begin result := -1; exit; end;
  result := mOwner.findRecordNumByType(mEBSTypeName, mRecRef);
end;


procedure TDynField.writeBinTo (var hasLostData: Boolean; st: TStream);
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
            raise TDynRecException.CreateFmt('record reference type ''%s'' in field ''%s'' cannot be written', [mEBSTypeName, mName]);
          end;
          // write triggerdata
          GetMem(buf, mMaxDim);
          if (buf = nil) then raise TDynRecException.CreateFmt('record reference type ''%s'' in field ''%s'' cannot be written', [mEBSTypeName, mName]);
          try
            FillChar(buf^, mMaxDim, 0);
            if (mRecRef <> nil) then
            begin
              ws := TSFSMemoryChunkStream.Create(buf, mMaxDim);
              mRecRef.writeBinTo(hasLostData, ws, mMaxDim); // as trigdata
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
          else raise TDynRecException.CreateFmt('record reference type ''%s'' in field ''%s'' cannot be written', [mEBSTypeName, mName]);
        end;
        // find record number
        if (mRecRef <> nil) then
        begin
          f := mOwner.findRecordNumByType(mEBSTypeName, mRecRef);
          if (f < 0) then raise TDynRecException.CreateFmt('record reference type ''%s'' in field ''%s'' not found in record list', [mEBSTypeName, mName]);
          if mAsMonsterId then Inc(f);
          if (f > maxv) then raise TDynRecException.CreateFmt('record reference type ''%s'' in field ''%s'' has too big index', [mEBSTypeName, mName]);
        end
        else
        begin
          if mAsMonsterId then f := 0 else f := -1;
        end;
        case mType of
          TType.TByte, TType.TUByte: writeInt(st, Byte(f));
          TType.TShort, TType.TUShort: writeInt(st, SmallInt(f));
          TType.TInt, TType.TUInt: writeInt(st, LongWord(f));
          else raise TDynRecException.CreateFmt('record reference type ''%s'' in field ''%s'' cannot be written', [mEBSTypeName, mName]);
        end;
        exit;
      end;
    TEBS.TEnum: begin end;
    TEBS.TBitSet: begin end;
    else raise TDynRecException.Create('ketmar forgot to handle some EBS type');
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
        if (mMaxDim = 0) then raise TDynRecException.CreateFmt('invalid string size definition for field ''%s''', [mName]);
        if (mMaxDim < 0) then
        begin
          if (Length(mSVal) <> 1) then raise TDynRecException.CreateFmt('invalid string size definition for field ''%s''', [mName]);
          writeInt(st, Byte(mSVal[1]));
        end
        else
        begin
          if (Length(mSVal) > mMaxDim) then raise TDynRecException.CreateFmt('invalid string size definition for field ''%s''', [mName]);
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
        if (mMaxDim >= 0) then TDynRecException.CreateFmt('byte array in field ''%s'' cannot be written', [mName]);
        writeInt(st, Byte(mIVal));
        exit;
      end;
    TType.TShort,
    TType.TUShort:
      begin
        if (mMaxDim >= 0) then raise TDynRecException.CreateFmt('short array in field ''%s'' cannot be written', [mName]);
        writeInt(st, Word(mIVal));
        exit;
      end;
    TType.TInt,
    TType.TUInt:
      begin
        if (mMaxDim >= 0) then raise TDynRecException.CreateFmt('int array in field ''%s'' cannot be written', [mName]);
        writeInt(st, LongWord(mIVal));
        exit;
      end;
    TType.TString:
      begin
        raise TDynRecException.CreateFmt('cannot write string field ''%s''', [mName]);
      end;
    TType.TPoint:
      begin
        if (mMaxDim >= 0) then raise TDynRecException.CreateFmt('pos/size array in field ''%s'' cannot be written', [mName]);
        writeInt(st, LongInt(mIVal));
        writeInt(st, LongInt(mIVal2));
        exit;
      end;
    TType.TSize:
      begin
        if (mMaxDim >= 0) then raise TDynRecException.CreateFmt('pos/size array in field ''%s'' cannot be written', [mName]);
        writeInt(st, Word(mIVal));
        writeInt(st, Word(mIVal2));
        exit;
      end;
    TType.TColor:
      begin
        if (mMaxDim >= 0) then raise TDynRecException.CreateFmt('color array in field ''%s'' cannot be written', [mName]);
        writeInt(st, Byte(mIVal));
        writeInt(st, Byte(mIVal2));
        writeInt(st, Byte(mIVal3));
        //writeInt(st, Byte(mIVal4)); // the only place we have RGB in binary map is effect trigger, and it has no alpha
        if (mIVal4 <> 255) then hasLostData := true;
        exit;
      end;
    TType.TList:
      raise TDynRecException.Create('cannot write lists to binary format');
    TType.TTrigData:
      raise TDynRecException.Create('cannot write triggers to binary format (internal error)');
    else raise TDynRecException.Create('ketmar forgot to handle some field type');
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
        //es := def.ebsType[mEBSTypeName];
        es := nil;
        if (mEBSType <> nil) and (mEBSType is TDynEBS) then es := (mEBSType as TDynEBS);
        if (es = nil) or (not es.mIsEnum) then raise TDynRecException.CreateFmt('record enum type ''%s'' for field ''%s'' not found', [mEBSTypeName, mName]);
        for f := 0 to High(es.mVals) do
        begin
          if (es.mVals[f] = mIVal) then
          begin
            wr.put(es.mIds[f]);
            wr.put(';'#10);
            exit;
          end;
        end;
        raise TDynRecException.CreateFmt('value %d in record enum type ''%s'' for field ''%s'' not found', [mIVal, mEBSTypeName, mName]);
      end;
    TEBS.TBitSet:
      begin
        //def := mOwner.mOwner;
        //es := def.ebsType[mEBSTypeName];
        es := nil;
        if (mEBSType <> nil) and (mEBSType is TDynEBS) then es := (mEBSType as TDynEBS);
        if (es = nil) or es.mIsEnum then raise TDynRecException.CreateFmt('record bitset type ''%s'' for field ''%s'' not found', [mEBSTypeName, mName]);
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
          raise TDynRecException.CreateFmt('value %d in record bitset type ''%s'' for field ''%s'' not found', [0, mEBSTypeName, mName]);
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
                if not first then wr.put(' | ') else first := false;
                wr.put(es.mIds[f]);
                found := true;
                break;
              end;
            end;
            if not found then raise TDynRecException.CreateFmt('value %d in record bitset type ''%s'' for field ''%s'' not found', [mask, mEBSTypeName, mName]);
          end;
          mask := mask shl 1;
        end;
        wr.put(';'#10);
        exit;
      end;
    else raise TDynRecException.Create('ketmar forgot to handle some EBS type');
  end;

  case mType of
    TType.TBool:
      begin
        if (mIVal = 0) then wr.put('false;'#10) else wr.put('true;'#10);
        exit;
      end;
    TType.TChar:
      begin
        if (mMaxDim = 0) then raise TDynRecException.CreateFmt('invalid string size definition for field ''%s''', [mName]);
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
    TType.TColor:
      begin
        if (mIVal3 = 255) then wr.put('(%d %d %d);'#10, [mIVal, mIVal2, mIVal3])
        else wr.put('(%d %d %d %d);'#10, [mIVal, mIVal2, mIVal3, mIVal4]);
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
    else raise TDynRecException.Create('ketmar forgot to handle some field type');
  end;
  raise TDynRecException.CreateFmt('cannot parse field ''%s'' yet', [mName]);
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
          if (tfld = nil) then raise TDynRecException.CreateFmt('triggerdata value for field ''%s'' in record ''%s'' without TriggerType field', [mName, rec.mTypeName]);
          rc := mOwner.mOwner.trigTypeFor[tfld.mSVal]; // find in mapdef
          if (rc = nil) then raise TDynRecException.CreateFmt('triggerdata definition for field ''%s'' in record ''%s'' with type ''%s'' not found', [mName, rec.mTypeName, tfld.mSVal]);
          rc := rc.clone(mOwner.mHeaderRec);
          rc.mHeaderRec := mOwner.mHeaderRec;
          // on error, it will be freed by memowner
          rc.parseBinValue(st, true);
          mRecRef := rc;
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
            else raise TDynRecException.CreateFmt('invalid non-numeric type ''%s'' for field ''%s'' of record ''%s''', [getTypeName(mType), mName, mEBSTypeName]);
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
          else raise TDynRecException.CreateFmt('invalid non-numeric type ''%s'' for field ''%s'' of record ''%s''', [getTypeName(mType), mName, mEBSTypeName]);
        end;
        es := nil;
        if (mEBSType <> nil) and (mEBSType is TDynEBS) then es := (mEBSType as TDynEBS);
        if (es = nil) or (es.mIsEnum <> (mEBS = TEBS.TEnum)) then raise TDynRecException.CreateFmt('record enum type ''%s'' for field ''%s'' not found', [mEBSTypeName, mName]);
        mIVal := f;
        // build enum/bitfield values
        if (mEBS = TEBS.TEnum) then
        begin
          mSVal := es.nameByValue(mIVal);
          if (Length(mSVal) = 0) then raise TDynRecException.CreateFmt('record enum type ''%s'' for field ''%s'' has invalid value %d', [mEBSTypeName, mName, mIVal]);
        end
        else
        begin
          // special for 'none'
          if (mIVal = 0) then
          begin
            mSVal := es.nameByValue(mIVal);
            if (Length(mSVal) = 0) then raise TDynRecException.CreateFmt('record bitset type ''%s'' for field ''%s'' has invalid value %d', [mEBSTypeName, mName, mIVal]);
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
                if (Length(s) = 0) then raise TDynRecException.CreateFmt('record bitset type ''%s'' for field ''%s'' has invalid value %d', [mEBSTypeName, mName, mask]);
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
    else raise TDynRecException.Create('ketmar forgot to handle some EBS type');
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
        raise TDynRecException.Create('cannot read strings from binaries yet');
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
    TType.TColor:
      begin
        mIVal := readByte(st);
        mIVal2 := readByte(st);
        mIVal3 := readByte(st);
        //mIVal4 := readByte(st); // the only place we have RGB in binary map is effect trigger, and it has no alpha
        mIVal4 := 255;
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
    else raise TDynRecException.Create('ketmar forgot to handle some field type');
  end;
  raise TDynRecException.CreateFmt('cannot parse field ''%s'' yet', [mName]);
end;


procedure TDynField.parseValue (pr: TTextParser);

  procedure parseInt (min, max: Integer);
  begin
    mIVal := pr.expectInt();
    if (mIVal < min) or (mIVal > max) then raise TDynParseException.CreateFmt(pr, 'invalid %s value for field ''%s''', [getTypeName(mType), mName]);
    mDefined := true;
  end;

var
  rec, rc: TDynRecord;
  es: TDynEBS = nil;
  tfld: TDynField;
  tk: AnsiString;
  edim: AnsiChar;
begin
  if (pr.tokType = pr.TTEOF) then raise TDynParseException.Create(pr, 'field value expected');
  if (pr.isDelim(';')) then raise TDynParseException.Create(pr, 'extra semicolon');
  // if this field should contain struct, convert type and parse struct
  case mEBS of
    TEBS.TNone: begin end;
    TEBS.TRec:
      begin
        // ugly hack. sorry.
        if (mType = TType.TTrigData) then
        begin
          pr.expectDelim('{');
          if (pr.eatDelim('}')) then
          begin
            // '{}'
            mRecRef := nil;
          end
          else
          begin
            rec := mOwner;
            // find trigger definition
            tfld := rec.trigTypeField();
            if (tfld = nil) then raise TDynParseException.CreateFmt(pr, 'triggerdata value for field ''%s'' in record ''%s'' without ''type'' field', [mName, rec.mTypeName]);
            rc := mOwner.mOwner.trigTypeFor[tfld.mSVal]; // find in mapdef
            if (rc = nil) then raise TDynParseException.CreateFmt(pr, 'triggerdata definition for field ''%s'' in record ''%s'' with type ''%s'' not found', [mName, rec.mTypeName, tfld.mSVal]);
            rc := rc.clone(mOwner.mHeaderRec);
            rc.mHeaderRec := mOwner.mHeaderRec;
            //writeln(rc.definition);
            // on error, it will be freed by memowner
            rc.parseValue(pr, true);
            mRecRef := rc;
          end;
          mDefined := true;
          pr.eatDelim(';'); // hack: allow (but don't require) semicolon after inline records
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
            if (rec = nil) then
            begin
              mRecRefId := pr.tokStr;
            end
            else
            begin
              mRecRef := rec;
              mRecRefId := '';
            end;
            pr.expectId();
          end;
          mDefined := true;
          pr.expectDelim(';');
          exit;
        end
        else if (pr.isDelim('{')) then
        begin
          //rec := mOwner.mOwner.recType[mEBSTypeName]; // find in mapdef
          rec := nil;
          if (mEBSType <> nil) and (mEBSType is TDynRecord) then rec := (mEBSType as TDynRecord);
          if (rec = nil) then raise TDynParseException.CreateFmt(pr, 'record type ''%s'' for field ''%s'' not found', [mEBSTypeName, mName]);
          rc := rec.clone(mOwner.mHeaderRec);
          rc.mHeaderRec := mOwner.mHeaderRec;
          rc.parseValue(pr);
          mRecRef := rc;
          mDefined := true;
          if mOwner.addRecordByType(mEBSTypeName, rc) then
          begin
            raise TDynParseException.CreateFmt(pr, 'duplicate record with id ''%s'' for field ''%s'' in record ''%s''', [rc.mId, mName, mOwner.mTypeName]);
          end;
          pr.eatDelim(';'); // hack: allow (but don't require) semicolon after inline records
          exit;
        end;
        pr.expectDelim('{');
      end;
    TEBS.TEnum:
      begin
        //es := mOwner.mOwner.ebsType[mEBSTypeName]; // find in mapdef
        es := nil;
        if (mEBSType <> nil) and (mEBSType is TDynEBS) then es := (mEBSType as TDynEBS);
        if (es = nil) or (not es.mIsEnum) then raise TDynParseException.CreateFmt(pr, 'record enum type ''%s'' for field ''%s'' not found', [mEBSTypeName, mName]);
        tk := pr.expectId();
        if not es.has[tk] then raise TDynParseException.CreateFmt(pr, 'record enum value ''%s'' of type ''%s'' for field ''%s'' not found', [tk, mEBSTypeName, mName]);
        mIVal := es.field[tk];
        mSVal := tk;
        //writeln('ENUM ', mEBSName, '; element <', mSVal, '> with value ', mIVal);
        mDefined := true;
        pr.expectDelim(';');
        exit;
      end;
    TEBS.TBitSet:
      begin
        //es := mOwner.mOwner.ebsType[mEBSTypeName]; // find in mapdef
        es := nil;
        if (mEBSType <> nil) and (mEBSType is TDynEBS) then es := (mEBSType as TDynEBS);
        if (es = nil) or es.mIsEnum then raise TDynParseException.CreateFmt(pr, 'record bitset type ''%s'' for field ''%s'' not found', [mEBSTypeName, mName]);
        mIVal := 0;
        while true do
        begin
          tk := pr.expectId();
          if not es.has[tk] then raise TDynParseException.CreateFmt(pr, 'record bitset value ''%s'' of type ''%s'' for field ''%s'' not found', [tk, mEBSTypeName, mName]);
          mIVal := mIVal or es.field[tk];
          mSVal := tk;
          if (pr.tokType <> pr.TTDelim) or ((pr.tokChar <> '|') and (pr.tokChar <> '+')) then break;
          if mBitSetUnique then raise TDynParseException.CreateFmt(pr, 'record bitset of type ''%s'' for field ''%s'' expects only one value', [tk, mEBSTypeName, mName]);
          pr.skipToken(); // plus or pipe
        end;
        mDefined := true;
        pr.expectDelim(';');
        exit;
      end;
    else raise TDynParseException.Create(pr, 'ketmar forgot to handle some EBS type');
  end;

  case mType of
    TType.TBool:
      begin
             if pr.eatId('true') or pr.eatId('tan') or pr.eatId('yes') then mIVal := 1
        else if pr.eatId('false') or pr.eatId('ona') or pr.eatId('no') then mIVal := 0
        else raise TDynParseException.CreateFmt(pr, 'invalid bool value for field ''%s''', [mName]);
        mDefined := true;
        pr.expectDelim(';');
        exit;
      end;
    TType.TChar:
      begin
        if (mMaxDim = 0) then raise TDynParseException.CreateFmt(pr, 'invalid string size definition for field ''%s''', [mName]);
        mSVal := pr.expectStr(true);
        if (mMaxDim < 0) then
        begin
          // single char
          if (Length(mSVal) <> 1) then raise TDynParseException.CreateFmt(pr, 'invalid string size for field ''%s''', [mName]);
          mIVal := Integer(mSVal[1]);
          mSVal := '';
        end
        else
        begin
          // string
          if (Length(mSVal) > mMaxDim) then raise TDynParseException.CreateFmt(pr, 'invalid string size for field ''%s''', [mName]);
        end;
        mDefined := true;
        pr.expectDelim(';');
        exit;
      end;
    TType.TByte:
      begin
        parseInt(-128, 127);
        pr.expectDelim(';');
        exit;
      end;
    TType.TUByte:
      begin
        parseInt(0, 255);
        pr.expectDelim(';');
        exit;
      end;
    TType.TShort:
      begin
        parseInt(-32768, 32768);
        pr.expectDelim(';');
        exit;
      end;
    TType.TUShort:
      begin
        parseInt(0, 65535);
        pr.expectDelim(';');
        exit;
      end;
    TType.TInt:
      begin
        parseInt(Integer($80000000), $7fffffff);
        pr.expectDelim(';');
        exit;
      end;
    TType.TUInt:
      begin
        parseInt(0, $7fffffff); //FIXME
        pr.expectDelim(';');
        exit;
      end;
    TType.TString:
      begin
        mSVal := pr.expectStr(true);
        mDefined := true;
        pr.expectDelim(';');
        exit;
      end;
    TType.TPoint,
    TType.TSize:
      begin
        if pr.eatDelim('[') then edim := ']' else begin pr.expectDelim('('); edim := ')'; end;
        mIVal := pr.expectInt();
        if (mType = TType.TSize) then
        begin
          if (mIVal < 0) or (mIVal > 65535) then raise TDynParseException.CreateFmt(pr, 'invalid %s value for field ''%s''', [getTypeName(mType), mName]);
        end;
        mIVal2 := pr.expectInt();
        if (mType = TType.TSize) then
        begin
          if (mIVal2 < 0) or (mIVal2 > 65535) then raise TDynParseException.CreateFmt(pr, 'invalid %s value for field ''%s''', [getTypeName(mType), mName]);
        end;
        mDefined := true;
        pr.expectDelim(edim);
        pr.expectDelim(';');
        exit;
      end;
    TType.TColor:
      begin
        if pr.eatDelim('[') then edim := ']' else begin pr.expectDelim('('); edim := ')'; end;
        mIVal := pr.expectInt();
        if (mIVal < 0) or (mIVal > 255) then raise TDynParseException.CreateFmt(pr, 'invalid %s value for field ''%s''', [getTypeName(mType), mName]);
        mIVal2 := pr.expectInt();
        if (mIVal2 < 0) or (mIVal2 > 255) then raise TDynParseException.CreateFmt(pr, 'invalid %s value for field ''%s''', [getTypeName(mType), mName]);
        mIVal3 := pr.expectInt();
        if (mIVal3 < 0) or (mIVal3 > 255) then raise TDynParseException.CreateFmt(pr, 'invalid %s value for field ''%s''', [getTypeName(mType), mName]);
        if (pr.tokType = pr.TTInt) then
        begin
          mIVal4 := pr.expectInt();
          if (mIVal4 < 0) or (mIVal4 > 255) then raise TDynParseException.CreateFmt(pr, 'invalid %s value for field ''%s''', [getTypeName(mType), mName]);
        end
        else
        begin
          mIVal4 := 255;
        end;
        mDefined := true;
        pr.expectDelim(edim);
        pr.expectDelim(';');
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
    else raise TDynParseException.Create(pr, 'ketmar forgot to handle some field type');
  end;
  raise TDynParseException.CreateFmt(pr, 'cannot parse field ''%s'' yet', [mName]);
end;


// ////////////////////////////////////////////////////////////////////////// //
constructor TDynRecord.Create (pr: TTextParser);
begin
  if (pr = nil) then raise TDynParseException.Create(pr, 'cannot create record type without type definition');
  mId := '';
  mTypeName := '';
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
  mTypeName := '';
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
  mRec2Free := nil;
end;


destructor TDynRecord.Destroy ();
var
  fld: TDynField;
  rec: TDynRecord;
begin
  if (mRec2Free <> nil) then
  begin
    for rec in mRec2Free do
    begin
      if (rec <> self) then
      begin
        //writeln(formatstrf('freeing: 0x%08x; name=%s; id=%s', [Pointer(rec), rec.mName, rec.mId]));
        rec.Free();
      end;
    end;
    mRec2Free.Free();
    mRec2Free := nil;
  end;
  mTypeName := '';
  for fld in mFields do fld.Free();
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


procedure TDynRecord.regrec (rec: TDynRecord);
begin
  if (rec <> nil) and (rec <> self) then
  begin
    if (mRec2Free = nil) then mRec2Free := TDynRecList.Create();
    mRec2Free.append(rec);
  end;
end;


procedure TDynRecord.addField (fld: TDynField); inline;
begin
  if (fld = nil) then raise TDynRecException.Create('cannot append nil field to record');
  mFields.append(fld);
  {$IF DEFINED(XDYNREC_USE_FIELDHASH)}
  if (Length(fld.mName) > 0) then mFieldsHash.put(fld.mName, mFields.count-1);
  {$ENDIF}
end;


function TDynRecord.addFieldChecked (fld: TDynField): Boolean; inline; // `true`: duplicate name
begin
  result := false;
  if (fld = nil) then raise TDynRecException.Create('cannot append nil field to record');
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


function TDynRecord.clone (registerIn: TDynRecord): TDynRecord;
var
  fld: TDynField;
  f: Integer;
begin
  result := TDynRecord.Create();
  result.mOwner := mOwner;
  result.mId := mId;
  result.mTypeName := mTypeName;
  result.mTip := mTip;
  result.mHelp := mHelp;
  result.mSize := mSize;
  result.mHeader := mHeader;
  result.mBinBlock := mBinBlock;
  result.mHeaderRec := mHeaderRec;
  result.mTagInt := mTagInt;
  result.mTagPtr := mTagPtr;
  if (mFields.count > 0) then
  begin
    result.mFields.capacity := mFields.count;
    for fld in mFields do result.addField(fld.clone(result, registerIn));
  end;
  SetLength(result.mTrigTypes, Length(mTrigTypes));
  for f := 0 to High(mTrigTypes) do result.mTrigTypes[f] := mTrigTypes[f];
  if (registerIn <> nil) then registerIn.regrec(result);
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
  if (fld.mType <> fld.TType.TList) then raise TDynRecException.CreateFmt('cannot get record of type ''%s'' due to name conflict with ordinary field', [atypename]);
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
  if (fld.mType <> fld.TType.TList) then raise TDynRecException.CreateFmt('cannot get record of type ''%s'' due to name conflict with ordinary field', [atypename]);
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
  if (fld.mType <> fld.TType.TList) then raise TDynRecException.CreateFmt('cannot append record of type ''%s'' due to name conflict with ordinary field', [atypename]);
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
    if StrEqu(es.mTypeName, 'TriggerType') then begin result := fld; exit; end;
  end;
  result := nil;
end;


// number of records of the given instance
function TDynRecord.instanceCount (const atypename: AnsiString): Integer;
var
  fld: TDynField;
begin
  result := 0;
  fld := field[atypename];
  if (fld <> nil) and (fld.mType = fld.TType.TList) then result := fld.mRVal.count;
end;


function TDynRecord.newTypedRecord (const atypename, aid: AnsiString): TDynRecord;
var
  trc: TDynRecord;
  fld: TDynField;
begin
  if not mHeader then raise TDynRecException.Create('cannot create new records with non-header');
  if (Length(aid) = 0) then raise TDynRecException.CreateFmt('cannot create new record of type ''%s'' without id', [atypename]);
  trc := mapdef.recType[atypename];
  if (trc = nil) then begin result := nil; exit; end;
  // check if aid is unique
  fld := field[atypename];
  if (fld <> nil) and (fld.getListItem(aid) <> nil) then raise TDynRecException.CreateFmt('cannot create record of type ''%s'' with duplicate id ''%s''', [atypename, aid]);
  result := trc.clone(self);
  result.mId := aid;
  addRecordByType(atypename, result);
end;


procedure TDynRecord.clearRefRecs (rec: TDynRecord);
  procedure clearRefs (fld: TDynField);
  var
    rc: TDynRecord;
  begin
    if (fld = nil) then exit;
    if (fld.mRecRef = rec) then fld.mRecRef := nil;
    if (fld.mType = fld.TType.TList) then for rc in fld.mRVal do rc.clearRefRecs(rec);
  end;
var
  fld: TDynField;
begin
  if (rec = nil) or (mFields = nil) then exit;
  for fld in mFields do clearRefs(fld);
end;


// remove record with the given type and id
// return `true` if record was successfully found and removed
// this will do all necessary recref cleanup too
function TDynRecord.removeTypedRecord (const atypename, aid: AnsiString): Boolean;
var
  trc, rec: TDynRecord;
  fld: TDynField;
  f: Integer;
  doFree: Boolean = false;
begin
  result := false;
  if not mHeader then raise TDynRecException.Create('cannot remove records with non-header');
  if (Length(aid) = 0) then exit;
  trc := mapdef.recType[atypename];
  if (trc = nil) then exit;
  fld := field[atypename];
  if (fld = nil) then exit;
  rec := fld.removeListItem(aid);
  if (rec = nil) then exit;
  clearRefRecs(rec);
  for f := 0 to mRec2Free.count-1 do
  begin
    if (mRec2Free[f] = rec) then
    begin
      mRec2Free[f] := nil;
      doFree := true;
    end;
  end;
  if doFree then rec.Free();
end;


function TDynRecord.getUserVar (const aname: AnsiString): Variant;
var
  fld: TDynField;
begin
  fld := getFieldByName(aname);
  if (fld = nil) then result := Unassigned else result := fld.value;
end;


procedure TDynRecord.setUserVar (const aname: AnsiString; val: Variant);
var
  fld: TDynField;
begin
  fld := getFieldByName(aname);
  if (fld = nil) then
  begin
    if (Length(aname) = 0) then raise TDynRecException.Create('cannot create nameless user field');
    fld := TDynField.Create(aname, val);
    fld.mOwner := self;
    fld.mInternal := true;
    addField(fld);
  end
  else
  begin
    fld.value := val;
  end;
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
        while (pr.eatDelim(',')) do begin end;
        if pr.eatDelim(')') then break;
        tdn := pr.expectId();
        if isForTrig[tdn] then raise TDynParseException.CreateFmt(pr, 'duplicate trigdata ''%s'' trigtype ''%s''', [mTypeName, tdn]);
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
    mTypeName := 'TriggerData';
  end
  else
  begin
    mTypeName := pr.expectStrOrId();
    while (not pr.isDelim('{')) do
    begin
      if pr.eatId('header') then begin mHeader := true; continue; end;
      if pr.eatId('size') then
      begin
        if (mSize > 0) then raise TDynParseException.CreateFmt(pr, 'duplicate `size` in record ''%s''', [mTypeName]);
        mSize := pr.expectInt();
        if (mSize < 1) then raise TDynParseException.CreateFmt(pr, 'invalid record ''%s'' size: %d', [mTypeName, mSize]);
        pr.expectId('bytes');
        continue;
      end;
      if pr.eatId('binblock') then
      begin
        if (mBinBlock >= 0) then raise TDynParseException.CreateFmt(pr, 'duplicate `binblock` in record ''%s''', [mTypeName]);
        mBinBlock := pr.expectInt();
        if (mBinBlock < 1) then raise TDynParseException.CreateFmt(pr, 'invalid record ''%s'' binblock: %d', [mTypeName, mBinBlock]);
        continue;
      end;
      if pr.eatId('tip') then
      begin
        if (Length(mTip) > 0) then raise TDynParseException.CreateFmt(pr, 'duplicate tip definition for record ''%s''', [mTypeName]);
        mTip := pr.expectStr(false);
        continue;
      end;
      if pr.eatId('help') then
      begin
        if (Length(mHelp) > 0) then raise TDynParseException.CreateFmt(pr, 'duplicate help definition for record ''%s''', [mTypeName]);
        mHelp := pr.expectStr(false);
        continue;
      end;
    end;
  end;

  pr.expectDelim('{');
  // load fields
  while (not pr.isDelim('}')) do
  begin
    fld := TDynField.Create(pr);
    // append
    fld.mOwner := self;
    if addFieldChecked(fld) then
    begin
      fld.Free();
      raise TDynParseException.CreateFmt(pr, 'duplicate field ''%s''', [fld.name]);
    end;
    // done with field
  end;
  pr.expectDelim('}');
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
    result := quoteStr(mTypeName);
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
        if assigned(DynWarningCB) then
        begin
          DynWarningCB(formatstrf('record of type ''%s'' with id ''%s'' links to inexistant record of type ''%s'' with id ''%s''', [rec.mTypeName, rec.mId, fld.mEBSTypeName, fld.mRecRefId]), -1, -1);
        end;
        //raise TDynRecException.CreateFmt('record of type ''%s'' with id ''%s'' links to inexistant record of type ''%s'' with id ''%s''', [rec.mName, rec.mId, fld.mEBSTypeName, fld.mRecRefId]);
      end;
      //writeln(' ', rec.mName, '.', rec.mId, ':', fld.mName, ' -> ', rt.mName, '.', rt.mId, ' (', fld.mEBSTypeName, '.', fld.mRecRefId, ')');
      fld.mRecRefId := '';
      fld.mRecRef := rt;
      fld.mDefined := true;
    end;
    for fld in rec.mFields do
    begin
      //if (fld.mName = 'ambient_color') then writeln('****', fld.mName);
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
      if (sign <> 'MAP'#1) then raise TDynRecException.Create('invalid binary map signature');
      // parse blocks
      while (st.position < st.size) do
      begin
        btype := readByte(st);
        if (btype = 0) then break; // no more blocks
        readLongWord(st); // reserved
        bsize := readLongInt(st);
        {$IF DEFINED(D2D_XDYN_DEBUG)}writeln('btype=', btype, '; bsize=', bsize);{$ENDIF}
        if (bsize < 0) or (bsize > $1fffffff) then raise TDynRecException.CreateFmt('block of type %d has invalid size %d', [btype, bsize]);
        if loaded[btype] then raise TDynRecException.CreateFmt('block of type %d already loaded', [btype]);
        loaded[btype] := true;
        // find record type for this block
        rect := nil;
        for rec in mOwner.recTypes do if (rec.mBinBlock = btype) then begin rect := rec; break; end;
        if (rect = nil) then raise TDynRecException.CreateFmt('block of type %d has no corresponding record', [btype]);
        //writeln('found type ''', rec.mName, ''' for block type ', btype);
        if (rec.mSize = 0) or ((bsize mod rec.mSize) <> 0) then raise TDynRecException.CreateFmt('block of type %d has invalid number of records', [btype]);
        // header?
        if (rect.mHeader) then
        begin
          if (bsize <> mSize) then raise TDynRecException.CreateFmt('header block of type %d has invalid number of records', [btype]);
          GetMem(buf, bsize);
          st.ReadBuffer(buf^, bsize);
          mst.setup(buf, mSize);
          parseBinValue(mst, true); // force parsing data
        end
        else
        begin
          // create list for this type
          fld := TDynField.Create(rec.mTypeName, TDynField.TType.TList);
          fld.mOwner := self;
          addField(fld);
          if (bsize > 0) then
          begin
            GetMem(buf, bsize);
            st.ReadBuffer(buf^, bsize);
            for f := 0 to (bsize div rec.mSize)-1 do
            begin
              mst.setup(buf+f*rec.mSize, rec.mSize);
              rec := rect.clone(self);
              rec.mHeaderRec := self;
              rec.parseBinValue(mst);
              rec.mId := Format('%s%d', [rec.mTypeName, f]);
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
    if StrEqu(mTypeName, 'TriggerData') then mSize := Integer(st.size-st.position);
    if (mSize < 1) then raise TDynRecException.CreateFmt('cannot read record of type ''%s'' with unknown size', [mTypeName]);
    GetMem(buf, mSize);
    st.ReadBuffer(buf^, mSize);
    for fld in mFields do
    begin
      if fld.mInternal then continue;
      if (fld.mBinOfs < 0) then continue;
      if (fld.mBinOfs >= st.size) then raise TDynRecException.CreateFmt('record of type ''%s'' has invalid field ''%s''', [fld.mName]);
      mst.setup(buf+fld.mBinOfs, mSize-fld.mBinOfs);
      //writeln('parsing ''', mName, '.', fld.mName, '''...');
      fld.parseBinValue(mst);
    end;
    // fix default values
    for fld in mFields do
    begin
      if (fld.mType = TDynField.TType.TList) then continue;
      fld.fixDefaultValue();
    end;
  finally
    mst.Free();
    if (buf <> nil) then FreeMem(buf);
  end;
end;


procedure TDynRecord.writeBinTo (var hasLostData: Boolean; st: TStream; trigbufsz: Integer=-1; onlyFields: Boolean=false);
var
  fld: TDynField;
  rec, rv: TDynRecord;
  buf: PByte = nil;
  ws: TStream = nil;
  blk, blkmax: Integer;
  bufsz: Integer = 0;
  blksz: Integer;
begin
  if (trigbufsz < 0) then
  begin
    if (mBinBlock < 1) then raise TDynRecException.Create('cannot write binary record without block number');
    if (mSize < 1) then raise TDynRecException.Create('cannot write binary record without size');
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
      if (fld.mBinOfs < 0) then
      begin
        if not fld.equToDefault then hasLostData := true;
        continue;
      end;
      if (fld.mBinOfs >= bufsz) then raise TDynRecException.Create('binary value offset is outside of the buffer');
      TSFSMemoryChunkStream(ws).setup(buf+fld.mBinOfs, bufsz-fld.mBinOfs);
      //writeln('writing field <', fld.mName, '>');
      fld.writeBinTo(hasLostData, ws);
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
          rec := mOwner.recType[fld.mName];
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
            rec := mOwner.recType[fld.mName];
            if (rec = nil) then continue;
            if (rec.mBinBlock <> blk) then continue;
            if (ws = nil) then ws := TMemoryStream.Create();
            for rv in fld.mRVal do rv.writeBinTo(hasLostData, ws);
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
  putTypeComment: Boolean;
  f: Integer;
begin
  if putHeader then
  begin
    wr.put(mTypeName);
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
        if not mHeader then raise TDynRecException.Create('record list in non-header record');
        if (fld.mRVal <> nil) and (fld.mRVal.count > 0) then
        begin
          putTypeComment := true;
          for rec in fld.mRVal do
          begin
            if (rec = nil) or (Length(rec.mId) = 0) then continue;
            if putTypeComment then
            begin
              wr.put(#10);
              if (80-wr.curIndent*2 >= 2) then
              begin
                wr.putIndent();
                for f := wr.curIndent to 80-wr.curIndent do wr.put('/');
                wr.put(#10);
              end;
              putTypeComment := false;
              wr.putIndent();
              wr.put('// ');
              wr.put(fld.name);
              wr.put(#10);
            end
            else
            begin
              wr.put(#10);
            end;
            wr.putIndent();
            rec.writeTo(wr, true);
          end;
        end;
        continue;
      end;
      if fld.mInternal then continue;
      if (not fld.mWriteDef) and fld.isDefaultValue then continue;
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

  procedure linkNames (rec: TDynRecord);
  var
    fld: TDynField;
    rt, rvc: TDynRecord;
  begin
    if (rec = nil) then exit;
    //writeln('*** rec: ', rec.mName, '.', rec.mId, ' (', rec.mFields.count, ')');
    for fld in rec.mFields do
    begin
      if (fld.mType = TDynField.TType.TList) then
      begin
        for rvc in fld.mRVal do linkNames(rvc);
      end;
      if (fld.mType = TDynField.TType.TTrigData) then
      begin
        //if (fld.mRecRef <> nil) then linkNames(fld.mRecRef);
        continue;
      end;
      if (Length(fld.mRecRefId) = 0) then continue;
      assert(fld.mEBSType <> nil);
      rt := findRecordByTypeId(fld.mEBSTypeName, fld.mRecRefId);
      if (rt = nil) then
      begin
        //e_LogWritefln('record of type ''%s'' with id ''%s'' links to inexistant record of type ''%s'' with id ''%s''', [rec.mName, rec.mId, fld.mEBSTypeName, fld.mRecRefId], MSG_WARNING);
        raise TDynParseException.CreateFmt(pr, 'record of type ''%s'' with id ''%s'' links to inexistant record of type ''%s'' with id ''%s''', [rec.mTypeName, rec.mId, fld.mEBSTypeName, fld.mRecRefId]);
      end;
      //writeln(' ', rec.mName, '.', rec.mId, ':', fld.mName, ' -> ', rt.mName, '.', rt.mId, ' (', fld.mEBSTypeName, '.', fld.mRecRefId, ')');
      fld.mRecRefId := '';
      fld.mRecRef := rt;
      fld.mDefined := true;
    end;
    for fld in rec.mFields do
    begin
      //writeln('  ', fld.mName);
      fld.fixDefaultValue();
    end;
  end;

begin
  if (mOwner = nil) then raise TDynParseException.CreateFmt(pr, 'can''t parse record ''%s'' value without owner', [mTypeName]);

  {$IF DEFINED(D2D_DYNREC_PROFILER)}stall := getTimeMicro();{$ENDIF}

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
  if not beginEaten then pr.expectDelim('{');
  while (not pr.isDelim('}')) do
  begin
    if (pr.tokType <> pr.TTId) then raise TDynParseException.Create(pr, 'identifier expected');
    //writeln('<', mName, '.', pr.tokStr, '>');

    // records
    if mHeader then
    begin
      // add records with this type (if any)
      {$IF DEFINED(D2D_DYNREC_PROFILER)}stt := getTimeMicro();{$ENDIF}
      trc := mOwner.recType[pr.tokStr];
      {$IF DEFINED(D2D_DYNREC_PROFILER)}profFindRecType := getTimeMicro()-stt;{$ENDIF}
      if (trc <> nil) then
      begin
        {$IF DEFINED(D2D_DYNREC_PROFILER)}stt := getTimeMicro();{$ENDIF}
        rec := trc.clone(mHeaderRec);
        {$IF DEFINED(D2D_DYNREC_PROFILER)}profCloneRec := getTimeMicro()-stt;{$ENDIF}
        rec.mHeaderRec := mHeaderRec;
        // on error, it will be freed by memowner
        pr.skipToken();
        rec.parseValue(pr);
        {$IF DEFINED(D2D_DYNREC_PROFILER)}stt := getTimeMicro();{$ENDIF}
        addRecordByType(rec.mTypeName, rec);
        {$IF DEFINED(D2D_DYNREC_PROFILER)}profAddRecByType := getTimeMicro()-stt;{$ENDIF}
        continue;
      end;
    end;

    // fields
    {$IF DEFINED(D2D_DYNREC_PROFILER)}stt := getTimeMicro();{$ENDIF}
    //writeln('0: <', mName, '.', pr.tokStr, '>');
    fld := field[pr.tokStr];
    //writeln('1: <', mName, '.', pr.tokStr, '>');
    {$IF DEFINED(D2D_DYNREC_PROFILER)}profFieldSearching := getTimeMicro()-stt;{$ENDIF}
    if (fld <> nil) then
    begin
      //writeln('2: <', mName, '.', pr.tokStr, '>');
      if fld.defined then raise TDynParseException.CreateFmt(pr, 'duplicate field ''%s'' in record ''%s''', [fld.mName, mTypeName]);
      if fld.internal then raise TDynParseException.CreateFmt(pr, 'internal field ''%s'' in record ''%s''', [fld.mName, mTypeName]);
      pr.skipToken(); // skip field name
      //writeln('3: <', mName, '.', pr.tokStr, '>:', pr.tokType);
      {$IF DEFINED(D2D_DYNREC_PROFILER)}stt := getTimeMicro();{$ENDIF}
      fld.parseValue(pr);
      {$IF DEFINED(D2D_DYNREC_PROFILER)}profFieldValParsing := getTimeMicro()-stt;{$ENDIF}
      continue;
    end;

    // something is wrong
    raise TDynParseException.CreateFmt(pr, 'unknown field ''%s'' in record ''%s''', [pr.tokStr, mTypeName]);
  end;
  pr.expectDelim('}');

  if mHeader then
  begin
    // link fields
    linkNames(self);
    for rec in mRec2Free do if (rec <> nil) then linkNames(rec);
  end;
  //writeln('done parsing record <', mName, '>');
  //{$IF DEFINED(D2D_DYNREC_PROFILER)}writeln('stall: ', getTimeMicro()-stall);{$ENDIF}
  {$IF DEFINED(D2D_DYNREC_PROFILER)}profRecValParse := getTimeMicro()-stall;{$ENDIF}
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
  mTypeName := '';
  mTip := '';
  mHelp := '';
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
  result += mTypeName;
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
  result := '// '+mTypeName+#10'const'#10;
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
  mTypeName := pr.expectId();
  mMaxVal := Integer($80000000);
  if mIsEnum then cv := 0 else cv := 1;
  while (not pr.isDelim('{')) do
  begin
    if pr.eatId('tip') then
    begin
      if (Length(mTip) > 0) then raise TDynParseException.CreateFmt(pr, 'duplicate tip definition for enum/bitset ''%s''', [mTypeName]);
      mTip := pr.expectStr(false);
      continue;
    end;
    if pr.eatId('help') then
    begin
      if (Length(mHelp) > 0) then raise TDynParseException.CreateFmt(pr, 'duplicate help definition for enum/bitset ''%s''', [mTypeName]);
      mHelp := pr.expectStr(false);
      continue;
    end;
    break;
  end;
  pr.expectDelim('{');
  while (not pr.isDelim('}')) do
  begin
    idname := pr.expectId();
    for f := 0 to High(mIds) do
    begin
      if StrEqu(mIds[f], idname) then raise TDynParseException.CreateFmt(pr, 'duplicate field ''%s'' in enum/bitset ''%s''', [idname, mTypeName]);
    end;
    if StrEqu(mMaxName, idname) then raise TDynParseException.CreateFmt(pr, 'duplicate field ''%s'' in enum/bitset ''%s''', [idname, mTypeName]);
    skipAdd := false;
    hasV := false;
    v := cv;
    // has value?
    if pr.eatDelim('=') then
    begin
      if pr.eatId('MAX') then
      begin
        if (Length(mMaxName) > 0) then raise TDynParseException.CreateFmt(pr, 'duplicate max field ''%s'' in enum/bitset ''%s''', [idname, mTypeName]);
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
    if (pr.isDelim('}')) then break;
    pr.expectDelim(',');
    while (pr.eatDelim(',')) do begin end;
  end;
  pr.expectDelim('}');
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
  //!!!FIXME!!! check who owns trigs and recs!
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
  if (recTypes.count = 0) then raise TDynRecException.Create('no header in empty mapdef');
  result := recTypes[0];
end;


function TDynMapDef.findRecType (const aname: AnsiString): TDynRecord;
var
  rec: TDynRecord;
begin
  for rec in recTypes do
  begin
    if StrEqu(rec.typeName, aname) then begin result := rec; exit; end;
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
    if StrEqu(ebs.typeName, aname) then begin result := ebs; exit; end;
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
            if (fld.mEBSType = nil) then raise TDynParseException.CreateFmt(pr, 'field ''%s'' of type ''%s'' has no correcponding record definition', [fld.mName, fld.mEBSTypeName]);
          end;
        TDynField.TEBS.TEnum,
        TDynField.TEBS.TBitSet:
          begin
            fld.mEBSType := findEBSType(fld.mEBSTypeName);
            if (fld.mEBSType = nil) then raise TDynParseException.CreateFmt(pr, 'field ''%s'' of type ''%s'' has no correcponding enum/bitset', [fld.mName, fld.mEBSTypeName]);
            if ((fld.mEBS = TDynField.TEBS.TEnum) <> (fld.mEBSType as TDynEBS).mIsEnum) then raise TDynParseException.CreateFmt(pr, 'field ''%s'' of type ''%s'' enum/bitset type conflict', [fld.mName, fld.mEBSTypeName]);
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

    if (pr.tokType = pr.TTId) then
    begin
      // enum or bitset
      if (pr.tokStr = 'enum') or (pr.tokStr = 'bitset') then
      begin
        eb := TDynEBS.Create(pr);
        if (findEBSType(eb.typeName) <> nil) then
        begin
          eb.Free();
          raise TDynParseException.CreateFmt(pr, 'duplicate enum/bitset ''%s''', [eb.typeName]);
        end;
        eb.mOwner := self;
        ebsTypes.append(eb);
        //writeln(eb.definition); writeln;
        continue;
      end;

      // triggerdata
      if (pr.tokStr = 'TriggerData') then
      begin
        rec := TDynRecord.Create(pr);
        for f := 0 to High(rec.mTrigTypes) do
        begin
          if (findTrigFor(rec.mTrigTypes[f]) <> nil) then
          begin
            rec.Free();
            raise TDynParseException.CreateFmt(pr, 'duplicate trigdata ''%s''', [rec.mTrigTypes[f]]);
          end;
        end;
        rec.mOwner := self;
        trigTypes.append(rec);
        //writeln(dr.definition); writeln;
        continue;
      end;
    end;

    rec := TDynRecord.Create(pr);
    //writeln(dr.definition); writeln;
    if (findRecType(rec.typeName) <> nil) then begin rec.Free(); raise TDynParseException.CreateFmt(pr, 'duplicate record ''%s''', [rec.typeName]); end;
    if (hdr <> nil) and StrEqu(rec.typeName, hdr.typeName) then begin rec.Free(); raise TDynParseException.CreateFmt(pr, 'duplicate record ''%s''', [rec.typeName]); end;
    rec.mOwner := self;
    if rec.mHeader then
    begin
      if (hdr <> nil) then begin rec.Free(); raise TDynParseException.CreateFmt(pr, 'duplicate header record ''%s'' (previous is ''%s'')', [rec.typeName, hdr.typeName]); end;
      hdr := rec;
    end
    else
    begin
      recTypes.append(rec);
    end;
  end;

  // put header record to top
  if (hdr = nil) then raise TDynParseException.Create(pr, 'header definition not found in mapdef');
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
function TDynMapDef.parseTextMap (pr: TTextParser): TDynRecord;
var
  res: TDynRecord = nil;
begin
  result := nil;
  try
    pr.expectId(headerType.typeName);
    res := headerType.clone(nil);
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
    res := headerType.clone(nil);
    res.mHeaderRec := res;
    res.parseBinValue(st);
    result := res;
    res := nil;
  finally
    res.Free();
  end;
end;


// WARNING! stream must be seekable
function TDynMapDef.parseMap (st: TStream; wasBinary: PBoolean=nil): TDynRecord;
var
  sign: packed array[0..3] of AnsiChar;
  pr: TTextParser;
begin
  if (wasBinary <> nil) then wasBinary^ := false;
  st.position := 0;
  st.ReadBuffer(sign[0], 4);
  st.position := 0;
  if (sign[0] = 'M') and (sign[1] = 'A') and (sign[2] = 'P') then
  begin
    if (sign[3] = #1) then
    begin
      if (wasBinary <> nil) then wasBinary^ := true;
      result := parseBinMap(st);
      exit;
    end;
    raise TDynRecException.Create('invalid binary map version');
  end
  else
  begin
    pr := TFileTextParser.Create(st, false); // `st` is not owned
    try
      try
        result := parseTextMap(pr);
      except on e: Exception do
        raise TDynParseException.Create(pr, e.message);
      end;
    finally
      pr.Free();
    end;
  end;
end;


// returns `true` if the given stream can be a map file
// stream position is 0 on return
// WARNING! stream must be seekable
class function TDynMapDef.canBeMap (st: TStream): Boolean;
var
  sign: packed array[0..3] of AnsiChar;
  pr: TTextParser;
begin
  result := false;
  st.position := 0;
  st.ReadBuffer(sign[0], 4);
  if (sign[0] = 'M') and (sign[1] = 'A') and (sign[2] = 'P') then
  begin
    result := (sign[3] = #1);
  end
  else
  begin
    st.position := 0;
    pr := TFileTextParser.Create(st, false); // `st` is not owned
    result := (pr.tokType = pr.TTId) and (pr.tokStr = 'map');
    pr.Free();
  end;
  st.position := 0;
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


function TDynMapDef.getRecTypeCount (): Integer; inline; begin result := recTypes.count; end;
function TDynMapDef.getRecTypeAt (idx: Integer): TDynRecord; inline; begin if (idx >= 0) and (idx < recTypes.count) then result := recTypes[idx] else result := nil; end;

function TDynMapDef.getEBSTypeCount (): Integer; inline; begin result := ebsTypes.count; end;
function TDynMapDef.getEBSTypeAt (idx: Integer): TDynEBS; inline; begin if (idx >= 0) and (idx < ebsTypes.count) then result := ebsTypes[idx] else result := nil; end;

function TDynMapDef.getTrigTypeCount (): Integer; inline; begin result := trigTypes.count; end;
function TDynMapDef.getTrigTypeAt (idx: Integer): TDynRecord; inline; begin if (idx >= 0) and (idx < trigTypes.count) then result := trigTypes[idx] else result := nil; end;


end.
