(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower OnGuard
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * Andrew Haines         andrew@haines.name                        {AH.01}
 *                       conversion to CLX                         {AH.01}
 *                       December 30, 2003                         {AH.01}
 *
 * ***** END LICENSE BLOCK ***** *)
{*********************************************************}
{*                  ONGUARD.PAS 1.13                     *}
{*     Copyright (c) 1996-02 TurboPower Software Co      *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$I onguard.inc}
{$I-} { I/O checks disabled}
{$Q-} {Integer overflow check disabled.
Warning : at least one function (MixBlock) causes overflow}

unit onguard;
  {-code and key classes and routines}

interface

uses
  Classes,
  SysUtils
{$IFDEF WINDOWS}
  ,Windows
  ,idesn { Serial number info from an IDE hard drive }
{$ENDIF}
{$IFDEF IBO_CONSOLE}
  ,ConsoleStubs
{$ENDIF}  
{$IFDEF UsingZLib}
  ,ZLib
{$ENDIF}
  ,ogutil
  ;

{$IFNDEF IBO_CONSOLE}
const
  {magic values}
  DaysCheckCode    = $649B;
  DateCheckCode    = $A4CB;
  NetCheckCode     = $9341;
  RegCheckCode     = $D9F6;
  SerialCheckCode  = $3C69;
  UsageCheckCode   = $F3D5;
  SpecialCheckCode = $9C5B;

type
  {code tyes}
  PCode = ^TCode;
  TCode = packed record
    CheckValue : Word;               {magic value}
    Expiration : Word;               {expiration date or 0, if none}
    InvalidCount : Word;             {count of respected invalid code accidents,normally 0}
    case Byte of
      0 : (FirstDate    : Word;      {for date code}
           EndDate      : Word);
      1 : (Days         : Word;      {for days code}
           LastAccess   : Word);
      2 : (RegString    : LongInt);  {for reg code}
      3 : (SerialNumber : LongInt);  {for serial number code}
      4 : (UsageCount   : Word;      {for usage count code}            {!!.02}
           LastChange   : Word);                                       {!!.02}
      5 : (Value        : LongInt);  {for specail codes}
      6 : (NetIndex     : LongInt);  {for net codes}
  end;

type
  TCodeType = (ctDate, ctDays, ctRegistration, ctSerialNumber,
               ctUsage, ctNetwork, ctSpecial, ctUnknown);
  {order must match tab order for code generation notebook}

type
  TKey     = array [0..15] of Byte;
  TKeyType = (ktRandom, ktMessageDigest, ktMessageDigestCS);
  {order must match order for key generation combobox string list}

type
  {contexts}
  TTMDContext = array [0..279] of Byte;

  {bit mixing types}
  T128Bit     = array [0..3] of LongInt;
  T256Bit     = array [0..7] of LongInt;

const
  DefCodeType      = ctDate;
  DefKeyType       = ktRandom;

type
  TEsMachineInfoSet =                                                  {!!.05}
    set of (midUser, midSystem, midNetwork, midDrives);                {!!.05}

type
  {result of code verification}
  TCodeStatus = (ogValidCode,    {code is valid but may still be expired}
                 ogInvalidCode,  {code is invalid}
                 ogPastEndDate,  {end date has been reached}
                 ogDayCountUsed, {number of days authorized have been used}
                 ogRunCountUsed, {number of runs authorized have been used}
                 ogNetCountUsed, {number of authorized users has been exceeded}
                 ogCodeExpired); {expiration date has been reached}

{$ENDIF}

type
  EOnGuardException = class(Exception);

type
  TChangeCodeEvent =
    procedure(Sender : TObject; Code : TCode)
    of object;
  TCheckedCodeEvent =
    procedure(Sender : TObject; Status : TCodeStatus)
    of object;
  TGetCodeEvent =
    procedure(Sender : TObject; var Code : TCode)
    of object;
  TGetKeyEvent =
    procedure(Sender : TObject; var Key : TKey)
    of object;
  TGetModifierEvent =
    procedure(Sender : TObject; var Value : LongInt)
    of object;
  TGetRegStringEvent =
    procedure(Sender : TObject; var Value : string)
    of object;

  {base registration code component}
  TOgCodeBase = class(TComponent)
  protected {private}
    {property variables}
    FAutoCheck     : Boolean;          {true to test code when loaded}
    FCode          : TCode;            {release code}
    FModifier      : LongInt;          {key modifier}
    FStoreCode     : Boolean;          {true to store release code on stream}
    FStoreModifier : Boolean;          {true to store key modifier on stream}

    {event variables}
    FOnChecked     : TCheckedCodeEvent;{called after auto check is made}
    FOnGetCode     : TGetCodeEvent;    {called to retrieve release code}
    FOnGetKey      : TGetKeyEvent;     {called to retrieve key}
    FOnGetModifier : TGetModifierEvent;{called to retrieve key modifier}

    {property methods}
    function GetCode : string;
    function GetModifier : string;
    function GetAbout : string;                                      {!!.08}
    procedure SetCode(const Value : string);
    procedure SetModifier(const Value : string);
    procedure SetAbout(const Value : string);                        {!!.08}

  protected
    procedure Loaded;
      override;

    procedure DoOnChecked(Value : TCodeStatus);
      dynamic;
    function DoOnGetCode : TCode;
      dynamic;
    procedure DoOnGetKey(var Key : TKey);
      dynamic;
    function DoOnGetModifier : LongInt;
      dynamic;

    {protected properties}
    property Code : string
      read GetCode
      write SetCode;

    property StoreCode : Boolean
      read FStoreCode
      write FStoreCode;

  public
    constructor Create(AOwner : TComponent);
      override;

    function CheckCode(Report : Boolean) : TCodeStatus;
      virtual; abstract;
    function IsCodeValid : Boolean;
      {-return true if code is valid}

  published
    {properties}
    property AutoCheck : Boolean
      read FAutoCheck
      write FAutoCheck
      default DefAutoCheck;

    property Modifier : string
      read GetModifier
      write SetModifier
      stored FStoreModifier;

    property StoreModifier : Boolean
      read FStoreModifier
      write FStoreModifier
      default DefStoreModifier;

    property About : string                                          {!!.08}
      read GetAbout                                                  {!!.08}
      write SetAbout                                                 {!!.08}
      stored False;

    {events}
    property OnChecked : TCheckedCodeEvent
      read FOnChecked
      write FOnChecked;

    property OnGetKey : TGetKeyEvent
      read FOnGetKey
      write FOnGetKey;

    property OnGetCode : TGetCodeEvent
      read FOnGetCode
      write FOnGetCode;

    property OnGetModifier : TGetModifierEvent
      read FOnGetModifier
      write FOnGetModifier;
  end;

  TOgMakeCodes = class(TComponent)
  protected {private}
    {property variables}
    FCode        : TCode;
    FCodeType    : TCodeType;
    FKey         : TKey;
    FKeyFileName : string;
    FKeyType     : TKeyType;
    FShowHints   : Boolean;

    {property methods}
    function GetAbout : string;                                      {!!.08}
    procedure SetAbout(const Value : string);                        {!!.08}

  public
    constructor Create(AOwner : TComponent);
      override;
    function Execute : Boolean;
    procedure GetCode(var Value : TCode);                            {!!.08}
    procedure SetCode(Value : TCode);                                {!!.08}
    procedure GetKey(var Value : TKey);                              {!!.08}
    procedure SetKey(Value : TKey);                                  {!!.08}

    property KeyType : TKeyType
      read FKeyType
      write FKeyType;

  published

    {properties}
    property CodeType : TCodeType
      read FCodeType
      write FCodeType
      default DefCodeType;

     property KeyFileName : string
      read FKeyFileName
      write FKeyFileName;

    property ShowHints : Boolean
      read FShowHints
      write FShowHints
      default False;

    property About : string                                          {!!.08}
      read GetAbout                                                  {!!.08}
      write SetAbout                                                 {!!.08}
      stored False;
  end;

type
  TOgMakeKeys = class(TComponent)
  protected {private}
    {property variables}
    FKeyFileName : string;
    FKey         : TKey;
    FKeyType     : TKeyType;
    FShowHints   : Boolean;

    {property methods}
    function GetAbout : string;                                      {!!.08}
    procedure SetAbout(const Value : string);                        {!!.08}

  public
    constructor Create(AOwner : TComponent);
      override;
    function Execute : Boolean;

    procedure ApplyModifierToKey(Modifier : LongInt; var Key; KeySize : Cardinal);
      {-signs the key with the modifier}
    function GenerateDateModifier(D : TDateTime) : LongInt;
      {-returns a modifier based on the current date}
    function GenerateMachineModifier : LongInt;
      {-returns a modifier based on hardware information}
    procedure GenerateMDKey(var Key; KeySize : Cardinal; const Str : string);
      {-generate a key based on the message digest of Str}
    procedure GenerateRandomKey(var Key; KeySize : Cardinal);
      {-generate a random key}
    function GenerateStringModifier(const S : string) : LongInt;
      {-returns a modifier based on S}
    function GenerateUniqueModifier : LongInt;
      {-returns a unique/random modifier}
    procedure SetKey(Value : TKey);                                  {!!.08}
    procedure GetKey(var Value : TKey);                              {!!.08}

  published

    {properties}
    property About : string                                          {!!.08}
      read GetAbout                                                  {!!.08}
      write SetAbout                                                 {!!.08}
      stored False;

    property KeyFileName : string
      read FKeyFileName
      write FKeyFileName;

    property KeyType : TKeyType
      read FKeyType
      write FKeyType
      default DefKeyType;

    property ShowHints : Boolean
      read FShowHints
      write FShowHints
      default False;
  end;

  { TOgDateCode }

  TOgDateCode = class(TOgCodeBase)
  public
    function CheckCode(Report : Boolean) : TCodeStatus;
      override;
    function GetValue : TDateTime;
      {-return expiration date (0 for error)}

    function GetInvalidCount : LongInt;

  published
    {properties}
    property Code
      stored FStoreCode;

    property StoreCode
      default DefStoreCode;
  end;

  TOgDaysCode = class(TOgCodeBase)
  protected {private}
    {property variables}
    FAutoDecrease : Boolean;

    {event variables}
    FOnChangeCode : TChangeCodeEvent;

  protected
    procedure Loaded;
      override;

    procedure DoOnChangeCode(Value : TCode);
      dynamic;

  public
    constructor Create(AOwner : TComponent);
      override;
    function CheckCode(Report : Boolean) : TCodeStatus;
      override;
    procedure Decrease;
      {-reduce days and generate modified code}
    function GetValue : LongInt;
      {-return number of days remaining}

    function GetInvalidCount : LongInt;

  published
    {properties}
    property AutoDecrease : Boolean
      read FAutoDecrease
      write FAutoDecrease
      default DefAutoDecrease;

    {events}
    property OnChangeCode : TChangeCodeEvent
      read FOnChangeCode
      write FOnChangeCode;
  end;

  TOgRegistrationCode = class(TOgCodeBase)
  protected {private}
    {property variables}
    FRegString      : string;
    FStoreRegString : Boolean;

    {event variables}
    FOnGetRegString : TGetRegStringEvent;

  protected
    function DoOnGetRegString : string;
      dynamic;

  public
    constructor Create(AOwner : TComponent);
      override;

    function CheckCode(Report : Boolean) : TCodeStatus;
      override;

  published
    {properties}
    property Code
      stored FStoreCode;

    property StoreCode
      default DefStoreCode;

    property RegString : string
      read FRegString
      write FRegString
      stored FStoreRegString;

    property StoreRegString : Boolean
      read FStoreRegString
      write FStoreRegString
      default DefStoreRegString;

    {events}
    property OnGetRegString : TGetRegStringEvent
      read FOnGetRegString
      write FOnGetRegString;
  end;

  TOgSerialNumberCode = class(TOgCodeBase)
  public
    function CheckCode(Report : Boolean) : TCodeStatus;
      override;
    function GetValue : LongInt;
      {-return serial number (0 for error)}

  published
    {properties}
    property Code
      stored FStoreCode;

    property StoreCode
      default DefStoreCode;

  end;

  TOgSpecialCode = class(TOgCodeBase)
    function CheckCode(Report : Boolean) : TCodeStatus;
      override;
    function GetValue : LongInt;
      {-return serial number (0 for error)}

  published
    {properties}
    property Code
      stored FStoreCode;

    property StoreCode
      default DefStoreCode;
  end;

  TOgUsageCode = class(TOgCodeBase)
  protected {private}
    {property variables}
    FAutoDecrease : Boolean;

    {event variables}
    FOnChangeCode : TChangeCodeEvent;

  protected
    procedure Loaded;
      override;

    procedure DoOnChangeCode(Value : TCode);
      dynamic;

  public
    constructor Create(AOwner : TComponent);
      override;
    function CheckCode(Report : Boolean) : TCodeStatus;
      override;
    procedure Decrease;
      {-reduce number of uses and generate code}
    function GetValue : LongInt;
      {-return number of uses remaining}

  published
    {properties}
    property AutoDecrease : Boolean
      read FAutoDecrease
      write FAutoDecrease
      default DefAutoDecrease;

    {events}
    property OnChangeCode : TChangeCodeEvent
      read FOnChangeCode
      write FOnChangeCode;
  end;

{$IFNDEF IBO_CONSOLE}

function GetInvalidCountValue(const Key : TKey; const Code : TCode) : LongInt;
procedure DecInvalidCountCode(const Key : TKey; var Code : TCode);


function GetCodeType(const Key : TKey; const Code : TCode) : TCodeType;
  {-return the type of code}
function GetExpirationDate(const Key : TKey; const Code : TCode) : TDateTime;
  {-return the date this code expires}

procedure InitDateCode(const Key : TKey; StartDate, EndDate : TDateTime; var Code : TCode; InvalidCount : Word=0);
function IsDateCodeValid(const Key : TKey; const Code : TCode) : Boolean;
function GetDateCodeValue(const Key : TKey; const Code : TCode) : TDateTime;
function IsDateCodeExpired(const Key : TKey; const Code : TCode) : Boolean;

procedure InitDaysCode(const Key : TKey; Days : Word; Expires : TDateTime; var Code : TCode; InvalidCount : Word=0);
function IsDaysCodeValid(const Key : TKey; const Code : TCode) : Boolean;
procedure DecDaysCode(const Key : TKey; var Code : TCode);
function GetDaysCodeValue(const Key : TKey; const Code : TCode) : LongInt;
function IsDaysCodeExpired(const Key : TKey; const Code : TCode) : Boolean;


procedure InitRegCode(const Key : TKey; const RegStr : string; Expires : TDateTime; var Code : TCode);
function IsRegCodeValid(const Key : TKey; const Code : TCode) : Boolean;
function IsRegCodeExpired(const Key : TKey; const Code : TCode) : Boolean;

procedure InitSerialNumberCode(const Key : TKey;  Serial : LongInt; Expires : TDateTime; var Code : TCode);
function IsSerialNumberCodeValid(const Key : TKey; const Code : TCode) : Boolean;
function GetSerialNumberCodeValue(const Key : TKey; const Code : TCode) : LongInt;
function IsSerialNumberCodeExpired(const Key : TKey; const Code : TCode) : Boolean;

procedure InitSpecialCode(const Key : TKey; Value : LongInt; Expires : TDateTime; var Code : TCode);
function IsSpecialCodeValid(const Key : TKey; const Code : TCode) : Boolean;
function GetSpecialCodeValue(const Key : TKey; const Code : TCode) : LongInt;
function IsSpecialCodeExpired(const Key : TKey; const Code : TCode) : Boolean;

procedure InitUsageCode(const Key : TKey; Count : Word; Expires : TDateTime; var Code : TCode;InvalidCount : Word=0);
function IsUsageCodeValid(const Key : TKey; const Code : TCode) : Boolean;
procedure DecUsageCode(const Key : TKey; var Code : TCode);
function GetUsageCodeValue(const Key : TKey; const Code : TCode) : LongInt;
function IsUsageCodeExpired(const Key : TKey; const Code: TCode) : Boolean;


{generate key routines}
procedure GenerateRandomKeyPrim(var Key; KeySize : Cardinal);
procedure GenerateTMDKeyPrim(var Key; KeySize : Cardinal; const Str : string);
procedure GenerateMD5KeyPrim(var Key: TKey; const Str : string);

{modifier routines}
function CreateMachineID(MachineInfo : TEsMachineInfoSet) : LongInt;
function GenerateStringModifierPrim(const S : string) : LongInt;
function GenerateUniqueModifierPrim : LongInt;
function GenerateMachineModifierPrim : LongInt;
function GenerateDateModifierPrim(D : TDateTime) : LongInt;
procedure ApplyModifierToKeyPrim(Modifier : LongInt; var Key; KeySize : Cardinal);

{hash routines}
function StringHashElf(const Str : string) : LongInt;

{mixing routines}
procedure MixBlock(const Matrix : T128Bit; var Block; Encrypt : Boolean);

{utility routines}
function ExpandDate(D : Word) : TDateTime;
function ShrinkDate(D : TDateTime) : Word;

const
  BaseDate : LongInt = 0;
{$ENDIF}

implementation
{$IFNDEF IBO_CONSOLE}
uses
  MD5
  ,ogconst
  {$IFDEF MakeCodesSupport} ,qonguard2 {$ENDIF}
  {$IFDEF MakeKeysSupport}  ,qonguard3  {$ENDIF}
  ,dbugintf
  ;
{$ENDIF}
{$IFNDEF IBO_CONSOLE}
{first 2048 bits of Pi in hexadecimal, low to high, without the leading "3"}
const
  Pi2048: array [0..255] of Byte = (
    $24, $3F, $6A, $88, $85, $A3, $08, $D3, $13, $19, $8A, $2E, $03, $70, $73, $44,
    $A4, $09, $38, $22, $29, $9F, $31, $D0, $08, $2E, $FA, $98, $EC, $4E, $6C, $89,
    $45, $28, $21, $E6, $38, $D0, $13, $77, $BE, $54, $66, $CF, $34, $E9, $0C, $6C,
    $C0, $AC, $29, $B7, $C9, $7C, $50, $DD, $3F, $84, $D5, $B5, $B5, $47, $09, $17,
    $92, $16, $D5, $D9, $89, $79, $FB, $1B, $D1, $31, $0B, $A6, $98, $DF, $B5, $AC,
    $2F, $FD, $72, $DB, $D0, $1A, $DF, $B7, $B8, $E1, $AF, $ED, $6A, $26, $7E, $96,
    $BA, $7C, $90, $45, $F1, $2C, $7F, $99, $24, $A1, $99, $47, $B3, $91, $6C, $F7,
    $08, $01, $F2, $E2, $85, $8E, $FC, $16, $63, $69, $20, $D8, $71, $57, $4E, $69,
    $A4, $58, $FE, $A3, $F4, $93, $3D, $7E, $0D, $95, $74, $8F, $72, $8E, $B6, $58,
    $71, $8B, $CD, $58, $82, $15, $4A, $EE, $7B, $54, $A4, $1D, $C2, $5A, $59, $B5,
    $9C, $30, $D5, $39, $2A, $F2, $60, $13, $C5, $D1, $B0, $23, $28, $60, $85, $F0,
    $CA, $41, $79, $18, $B8, $DB, $38, $EF, $8E, $79, $DC, $B0, $60, $3A, $18, $0E,
    $6C, $9E, $0E, $8B, $B0, $1E, $8A, $3E, $D7, $15, $77, $C1, $BD, $31, $4B, $27,
    $78, $AF, $2F, $DA, $55, $60, $5C, $60, $E6, $55, $25, $F3, $AA, $55, $AB, $94,
    $57, $48, $98, $62, $63, $E8, $14, $40, $55, $CA, $39, $6A, $2A, $AB, $10, $B6,
    $B4, $CC, $5C, $34, $11, $41, $E8, $CE, $A1, $54, $86, $AF, $7C, $72, $E9, $93);


{mixing routines}
procedure Mix128(var X : T128Bit);
var
  AA, BB, CC, DD : LongInt;
begin
  AA := X[0];  BB := X[1];  CC := X[2];  DD := X[3];

  AA := AA + DD;  DD := DD + AA;  AA := AA xor (AA shr 7);
  BB := BB + AA;  AA := AA + BB;  BB := BB xor (BB shl 13);
  CC := CC + BB;  BB := BB + CC;  CC := CC xor (CC shr 17);
  DD := DD + CC;  CC := CC + DD;  DD := DD xor (DD shl 9);
  AA := AA + DD;  DD := DD + AA;  AA := AA xor (AA shr 3);
  BB := BB + AA;  AA := AA + BB;  BB := BB xor (BB shl 7);
  CC := CC + BB;  BB := BB + CC;  CC := CC xor (DD shr 15);
  DD := DD + CC;  CC := CC + DD;  DD := DD xor (DD shl 11);

  X[0] := AA;  X[1] := BB;  X[2] := CC;  X[3] := DD;
end;

{quick (block) mixer routine}
procedure MixBlock(const Matrix : T128bit; var Block; Encrypt : Boolean);
const
  CKeyBox : array [False..True, 0..3, 0..2] of LongInt =
    (((0, 3, 1), (2, 1, 3), (1, 0, 2), (3, 2, 0)),
     ((3, 2, 0), (1, 0, 2), (2, 1, 3), (0, 3, 1)));
var
  Blocks  : array [0..1] of LongInt absolute Block;
  Work    : LongInt;
  Right   : LongInt;
  Left    : LongInt;
  R       : LongInt;
  AA, BB  : LongInt;
  CC, DD  : LongInt;
begin
  Right := Blocks[0];
  Left := Blocks[1];

  for R := 0 to 3 do begin
    {transform the right side}
    AA := Right;
    BB := Matrix[CKeyBox[Encrypt, R, 0]];
    CC := Matrix[CKeyBox[Encrypt, R, 1]];
    DD := Matrix[CKeyBox[Encrypt, R, 2]];

    {commented code does not affect results - removed for speed}
    AA := AA + DD;DD := DD + AA;AA := AA xor (AA shr 7);
    BB := BB + AA; AA := AA + BB; BB := BB xor (BB shl 13);
    CC := CC + BB; BB := BB + CC; CC := CC xor (CC shr 17);
    DD := DD + CC; CC := CC + DD; DD := DD xor (DD shl 9);
    AA := AA + DD; DD := DD + AA; AA := AA xor (AA shr 3);
    BB := BB + AA; {AA := AA + BB;}  BB := BB xor (BB shl 7);
    CC := CC + BB; {BB := BB + CC;}  CC := CC xor (DD shr 15);
    DD := DD + CC; {CC := CC + DD;}  DD := DD xor (DD shl 11);

    Work := Left xor DD;
    Left := Right;
    Right := Work;
  end;

  Blocks[0] := Left;
  Blocks[1] := Right;
end;

function HashElf(const Buf;  BufSize : LongInt) : LongInt;
var
  Bytes : TByteArray absolute Buf;
  I, X  : LongInt;
begin
  Result := 0;
  for I := 0 to BufSize - 1 do begin
    Result := (Result shl 4) + Bytes[I];
    X := Result and $F0000000;
    if (X <> 0) then
      Result := Result xor (X shr 24);
    Result := Result and (not X);
  end;
end;

function StringHashElf(const Str : string) : LongInt;
begin
  Result := HashElf(Str[1], Length(Str));
end;


{message digest routines}
type
  TMDContextEx = record
    DigestIndex : LongInt;
    Digest      : array [0..255] of Byte;
    KeyIndex    : LongInt;
    case Byte of
      0: (KeyInts : array [0..3] of LongInt);
      1: (Key     : TKey);
  end;
  TBlock2048 = array [0..255] of Byte;

procedure InitTMD(var Context : TTMDContext);
var
  ContextEx : TMDContextEx absolute Context;
begin
  ContextEx.DigestIndex := 0;
  TBlock2048(ContextEx.Digest) := TBlock2048(Pi2048);

  ContextEx.KeyIndex := 0;
  ContextEx.KeyInts[0] := $55555555;
  ContextEx.KeyInts[1] := $55555555;
  ContextEx.KeyInts[2] := $55555555;
  ContextEx.KeyInts[3] := $55555555;
end;

procedure UpdateTMD(var Context : TTMDContext; const Buf; BufSize : LongInt);
var
  ContextEx : TMDContextEx absolute Context;
  BufBytes  : TByteArray absolute Buf;
  AA, BB    : LongInt;
  CC, DD    : LongInt;
  I, R      : LongInt;
begin
  for I := 0 to BufSize - 1 do
    with ContextEx do begin
      {update Digest}
      Digest[DigestIndex] := Digest[DigestIndex] xor BufBytes[I];
      DigestIndex := DigestIndex + 1;
      if (DigestIndex = SizeOf(Digest)) then
        DigestIndex := 0;

      {update BlockKey}
      Key[KeyIndex] := Key[KeyIndex] xor BufBytes[I];
      KeyIndex := KeyIndex + 1;
      if (KeyIndex = SizeOf(Key) div 2) then begin
        AA := KeyInts[3];
        BB := KeyInts[2];
        CC := KeyInts[1];
        DD := KeyInts[0];

        {mix all the bits around for 4 rounds}
        {achieves avalanche and eliminates funnels}
        for R := 0 to 3 do begin
          AA := AA + DD; DD := DD + AA; AA := AA xor (AA shr 7);
          BB := BB + AA; AA := AA + BB; BB := BB xor (BB shl 13);
          CC := CC + BB; BB := BB + CC; CC := CC xor (CC shr 17);
          DD := DD + CC; CC := CC + DD; DD := DD xor (DD shl 9);
          AA := AA + DD; DD := DD + AA; AA := AA xor (AA shr 3);
          BB := BB + AA; AA := AA + BB; BB := BB xor (BB shl 7);
          CC := CC + BB; BB := BB + CC; CC := CC xor (DD shr 15);
          DD := DD + CC; CC := CC + DD; DD := DD xor (DD shl 11);
        end;

        KeyInts[0] := AA;
        KeyInts[1] := BB;
        KeyInts[2] := CC;
        KeyInts[3] := DD;

        KeyIndex := 0;
      end;
    end;
end;

procedure FinalizeTMD(var Context : TTMDContext; var Digest; DigestSize : LongInt);
const
  Padding : array [0..7] of Byte = (1, 0, 0, 0, 0, 0, 0, 0);
var
  ContextEx : TMDContextEx absolute Context;
  I         : Integer;
begin
  {pad with "1", followed by as many "0"s as needed to fill the block}
  UpdateTMD(Context, Padding, SizeOf(Padding) - ContextEx.KeyIndex);

  {mix each block within Context with the key}
  for I := 0 to (SizeOf(ContextEx.Digest) div SizeOf(TCode)) - 1 do
    MixBlock(T128Bit(ContextEx.Key), PCode(@ContextEx.Digest[I * SizeOf(TCode)])^, True);

  {return Digest of requested DigestSize}
  {max digest is 2048-bit, although it could be greater if Pi2048 was larger}
  Move(ContextEx.Digest, Digest, Min(SizeOf(ContextEx.Digest), DigestSize));
end;

{message digest hash}
procedure HashTMD(var Digest; DigestSize : LongInt; const Buf; BufSize : LongInt);
var
  Context : TTMDContext;
begin
  InitTMD(Context);
  UpdateTMD(Context, Buf, BufSize);
  FinalizeTMD(Context, Digest, DigestSize);
end;

{$IFDEF WINDOWS}
function CreateMachineID(MachineInfo : TEsMachineInfoSet) : LongInt;
{ Obtains information from:
    - Volume sizes (NOT free space)
    - Volume serial numbers
    - Registration name and company
    - GetSystemInfo relevant info
    - Network card ID (if available)
}
const
  sCurVer   = 'Software\Microsoft\Windows\CurrentVersion';
  sCurVerNT = 'Software\Microsoft\Windows NT\CurrentVersion';
  sRegOwner = 'RegisteredOwner';
  sRegOrg   = 'RegisteredOrganization';

type
  TUuidCreateSequential = function (lpGUID : Pointer): HResult; stdcall;

var
  hRPCTR4 : THandle;
  UuidCreateSequential : TUuidCreateSequential;
  I       : DWord;
  RegKey  : HKEY;
  GUID1   : TGUID;
  GUID2   : TGUID;
  Drive   : AnsiChar;
  SysInfo : TSystemInfo;
  Context : TTMDContext;
  UserInfoFound : Boolean;
  Buf     : array [0..1023] of Byte;
  iController, iDrive, maxController : Integer;
  BufStr : AnsiString;
begin
  InitTMD(Context);

  {include user specific information}
  if midUser in MachineInfo then
  begin
    UserInfoFound := False;
    { first look for registered info in \Windows\CurrentVersion }
    if (RegOpenKeyEx(HKEY_LOCAL_MACHINE, sCurVer, 0,
        KEY_QUERY_VALUE, RegKey) = ERROR_SUCCESS) then
    begin
      I := SizeOf(Buf);
      if RegQueryValueEx(RegKey, sRegOwner, nil, nil, @Buf, @I) = ERROR_SUCCESS then
      begin
        UserInfoFound := True;
        UpdateTMD(Context, Buf, I);
        I := SizeOf(Buf);
        if RegQueryValueEx(RegKey, sRegOrg, nil, nil, @Buf, @I) = ERROR_SUCCESS then
          UpdateTMD(Context, Buf, I);
      end;
      RegCloseKey(RegKey);
    end;

    { if not found, then look in \Windows NT\CurrentVersion }
    if not UserInfoFound then
      if (RegOpenKeyEx(HKEY_LOCAL_MACHINE, sCurVerNT, 0,
          KEY_QUERY_VALUE, RegKey) = ERROR_SUCCESS) then
      begin
        I := SizeOf(Buf);
        if RegQueryValueEx(RegKey, sRegOwner, nil, nil, @Buf, @I) = ERROR_SUCCESS then
        begin
          UpdateTMD(Context, Buf, I);
          I := SizeOf(Buf);
          if RegQueryValueEx(RegKey, sRegOrg, nil, nil, @Buf, @I) = ERROR_SUCCESS then
            UpdateTMD(Context, Buf, I);
        end;
        RegCloseKey(RegKey);
      end;
  end; { midUser }

  if midSystem in MachineInfo then
  begin
    {include system specific information}
    GetSystemInfo(SysInfo);
    PDWord(@Buf[0])^ := SysInfo.dwOemId;
    PDWord(@Buf[4])^ := SysInfo.dwProcessorType;
    UpdateTMD(Context, Buf, 8);
  end;

  if midNetwork in MachineInfo then
  begin
    {include network ID}
    CreateGuid(GUID1);
    CreateGuid(GUID2);

    { use UuidCreateSequential instead of CoCreateGuid if available }
    hRPCTR4 := LoadLibrary('rpcrt4.dll');
    if (hRPCTR4 <> 0) then begin
      UuidCreateSequential := TUuidCreateSequential(GetProcAddress(hRPCTR4, 'UuidCreateSequential'));
      if Assigned(UuidCreateSequential) then
      begin
        UuidCreateSequential(@GUID1);
        UuidCreateSequential(@GUID2);
      end;
      FreeLibrary(hRPCTR4);
    end;

    {check to see if "network" ID is available}
    if (GUID1.D4[2] = GUID2.D4[2]) and
       (GUID1.D4[3] = GUID2.D4[3]) and
       (GUID1.D4[4] = GUID2.D4[4]) and
       (GUID1.D4[5] = GUID2.D4[5]) and
       (GUID1.D4[6] = GUID2.D4[6]) and
       (GUID1.D4[7] = GUID2.D4[7]) then
      UpdateTMD(Context, GUID1.D4[2], 6);
  end; { midNetwork }

  if midDrives in MachineInfo then
  begin
    {include drive specific information}
    maxController := 15;
    if Win32Platform <> VER_PLATFORM_WIN32_NT then
      maxController := 0;
    for iController := 0 to maxController do
    begin
      for iDrive := 0 to 4 do
      begin
        BufStr := '';
        if GetIdeDiskSerialNumber(iController,iDrive,BufStr) then
           if BufStr<>'' then UpdateTMD(Context, BufStr[1], 5);
      end;
    end;
  end;

  FinalizeTMD(Context, Result, SizeOf(Result));
end;
{$ELSE}
{$NOTE Make sure we have some FreeBSD and MacOSX support too at some point }
{ We now assume Linux is used }
function CreateMachineID(MachineInfo : TEsMachineInfoSet) : LongInt;
var
  I       : LongInt;
  RegKey  : DWord;
  GUID1   : TGUID;
  GUID2   : TGUID;
  Drive   : Integer;
  Context : TTMDContext;
  Buf     : array [0..2047] of Byte;
  sl: TStringList;
  iFileHandle : LongInt;
  s: string;

  function lGetUnixUserName: string;
  begin
    // the first two are used when run from a normal login shell
    Result := GetEnvironmentVariable('USERNAME');
    if Result = '' then
      Result := GetEnvironmentVariable('USER');
    // Used if program is run from cron jobs
    if Result = '' then
      Result := GetEnvironmentVariable('LOGNAME');
  end;

begin
  InitTMD(Context);

  {include user specific information}
  if midUser in MachineInfo then
  begin
    // There is no organization specific info, so lets use the user login name
    s := lGetUnixUserName;
    I := Length(s);
    if i > 2048 then
    begin
      s := Copy(s, 1, 2048);  // only first 2048 characters
      i := 2048;
    end;
    FillChar(Buf, Sizeof(Buf), 0);
    Move(s[1], Buf, I);
    UpdateTMD(Context, Buf, I);
  end;

  if midSystem in MachineInfo then
  begin
    {include system specific information}
    iFileHandle := FileOpen('/proc/cpuinfo', fmopenRead or fmShareDenyNone);
    I := FileRead(iFileHandle, Buf,2048);
    if I > 0 then
    begin
      sl := TStringList.Create;
      try
        SetLength(s, Length(Buf));
        Move(Buf, s[1], Length(Buf));
        sl.Text := s;
        { two cpu properties change between reboot. Blank them out }
        for i := 0 to sl.Count-1 do
        begin
          if Pos('cpu MHz', sl[i]) > 0 then
            sl[i] := 'cpu MHz'
          else if Pos('bogomips', sl[i]) > 0 then
            sl[i] := 'bogomips';
        end;
        s := sl.Text;
        { place new data into Buf buffer }
        FillChar(Buf, Sizeof(Buf), 0);
        Move(s[1], Buf, Length(s));
        UpdateTMD(Context, Buf, I-1);
      finally
        sl.Free;
      end;
    end;
    FileClose(iFileHandle);

    iFileHandle := FileOpen('/proc/sys/kernel/version', fmopenRead or fmShareDenyNone);
    I := FileRead(iFileHandle, Buf, 2048);
    if I > 0 then  UpdateTMD(Context, Buf, I-1);
    FileClose(iFileHandle);

    iFileHandle := FileOpen('/proc/sys/kernel/osrelease', fmopenRead or fmShareDenyNone);
    I := FileRead(iFileHandle, Buf, 2048);
    if I > 0 then  UpdateTMD(Context, Buf, I-1);
    FileClose(iFileHandle);

    iFileHandle := FileOpen('/proc/sys/kernel/hostname', fmopenRead or fmShareDenyNone);
    I := FileRead(iFileHandle, Buf, 2048);
    if I > 0 then  UpdateTMD(Context, Buf, I-1);
    FileClose(iFileHandle);
  end;

  if midNetwork in MachineInfo then
  begin
    {include network ID}
    CreateGuid(GUID1);
    CreateGuid(GUID2);
    {check to see if "network" ID is available}
    if (GUID1.D4[2] = GUID2.D4[2]) and
       (GUID1.D4[3] = GUID2.D4[3]) and
       (GUID1.D4[4] = GUID2.D4[4]) and
       (GUID1.D4[5] = GUID2.D4[5]) and
       (GUID1.D4[6] = GUID2.D4[6]) and
       (GUID1.D4[7] = GUID2.D4[7]) then
      UpdateTMD(Context, GUID1.D4[2], 6);
  end;

  // TODO: This midDrives code is rubbish and doesn't work with SATA or SCSI drives.
(*
  if midDrives in MachineInfo then
  begin
    {include drive specific information}
    for Drive := 2 {C} to 25 {Z} do begin
      if GetDriveType(Drive) = 3 {DRIVE_FIXED} then begin
        FillChar(Buf, Sizeof(Buf), 0);
        Buf[0] := Drive;
        PLongInt(@Buf[1])^ := GetDiskSerialNumber(Chr(Drive+Ord('A')));
        UpdateTMD(Context, Buf, 5);
      end;
    end;
  end;
*)

  FinalizeTMD(Context, Result, SizeOf(Result));
end;
{$ENDIF}

{key generation routines }
procedure GenerateRandomKeyPrim(var Key; KeySize: Cardinal);
var
  Bytes : TByteArray absolute Key;
  I     : Integer;
begin
  Randomize;
  for I := 0 to KeySize - 1 do
    Bytes[I] := Random(256);
end;

procedure GenerateTMDKeyPrim(var Key; KeySize: Cardinal; const Str: string);
var
  I  : Integer;
  S2 : string;
begin
  {strip accented characters from the string}                          {!!.06}
  S2 := Str;                                                           {!!.06}
  for I := Length(S2) downto 1 do                                      {!!.06}
    if Ord(S2[I]) > 127 then                                           {!!.06}
      Delete(S2, I, 1);                                                {!!.06}

  HashTMD(Key, KeySize, S2[1], Length(S2));                            {!!.06}
end;

procedure GenerateMD5KeyPrim(var Key: TKey; const Str: string);
var
  D : TMD5Digest;
  I  : Integer;
  S2 : string;
begin
  {strip accented characters from the string}                          {!!.06}
  S2 := Str;                                                           {!!.06}
  for I := Length(S2) downto 1 do                                      {!!.06}
    if Ord(S2[I]) > 127 then                                           {!!.06}
      Delete(S2, I, 1);                                                {!!.06}

  D := MD5String(S2);                                     {!!.06}
  Key := TKey(D);
end;


{modifier routines}
function GenerateStringModifierPrim(const S : string) : LongInt;
var
  I   : Integer;                                                       {!!.06}
  Sig : array [0..4] of AnsiChar;
  S2  : string;                                                        {!!.06}
begin
  FillChar(Sig, SizeOf(Sig), 0);

  {strip accented characters from the string}                          {!!.06}
  S2 := S;                                                             {!!.06}
  for I := Length(S2) downto 1 do                                      {!!.06}
    if Ord(S2[I]) > 127 then                                           {!!.06}
      Delete(S2, I, 1);                                                {!!.06}

  StrPLCopy(Sig, AnsiUpperCase(S2), Min(4, Length(S2)));               {!!.06}
  Result := PLongInt(@Sig[0])^;
end;

function GenerateUniqueModifierPrim : LongInt;
var
  ID : TGUID;
begin
  CreateGuid(ID);
  Mix128(T128Bit(ID));
  Result := T128Bit(ID)[3];
end;

function GenerateMachineModifierPrim : LongInt;
begin
  Result := CreateMachineID([midUser, midSystem, {midNetwork,} midDrives]);
end;

function GenerateDateModifierPrim(D : TDateTime) : LongInt;
begin
  Result := Trunc(D);
  TLongIntRec(Result).Hi := TLongIntRec(Result).Lo xor $AAAA;
end;

procedure ApplyModifierToKeyPrim(Modifier : LongInt; var Key; KeySize : Cardinal);
begin
  if Modifier <> 0 then
    XorMem(Key, Modifier, Min(SizeOf(Modifier), KeySize));
end;
{$ENDIF}

{*** TogCodeBase ***}

constructor TOgCodeBase.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FAutoCheck := DefAutoCheck;
  FStoreCode := DefStoreCode;
  FStoreModifier := DefStoreModifier;

end;

procedure TOgCodeBase.DoOnChecked(Value : TCodeStatus);
begin
  if Assigned(FOnChecked) then
    FOnChecked(Self, Value)
  else if FAutoCheck then
    raise EOnGuardException.CreateFmt(SCNoOnCheck, [Self.ClassName]);
end;

function TOgCodeBase.DoOnGetCode : TCode;
begin
  FillChar(Result, SizeOf(Result), 0);
  if FStoreCode then
    Result := FCode
  else begin
    if Assigned(FOnGetCode) then
      FOnGetCode(Self, Result)
    else
      raise EOnGuardException.CreateFmt(SCNoOnGetCode, [Self.ClassName]);
  end;

  {store code for easy access using the Code property}                 {!!.02}
  FCode := Result;                                                     {!!.02}
end;

procedure TOgCodeBase.DoOnGetKey(var Key : TKey);
begin
  FillChar(Key, SizeOf(TKey), 0);
  if Assigned(FOnGetKey) then
    FOnGetKey(Self, Key)
  else
    raise EOnGuardException.CreateFmt(SCNoOnGetKey, [Self.ClassName]);
end;

{!!.02} {revised}
function TOgCodeBase.DoOnGetModifier : LongInt;
var
  L : LongInt;
begin
  Result := 0;
  if FStoreModifier then
    Result := FModifier
  else if Assigned(FOnGetModifier) then begin
    FOnGetModifier(Self, L);
    if FStoreModifier then begin
      {byte and word swap the modifier}
      TLongIntRec(Result).HiHi := TLongIntRec(L).LoLo;
      TLongIntRec(Result).HiLo := TLongIntRec(L).LoHi;
      TLongIntRec(Result).LoHi := TLongIntRec(L).HiLo;
      TLongIntRec(Result).LoLo := TLongIntRec(L).HiHi;
    end else
      Result := L;
  end;

  {store modifier for easy access using the Modifier property}         {!!.02}
  FModifier := Result;                                                 {!!.02}
end;

function TOgCodeBase.GetCode : string;
var
  Work : TCode;
begin
  Result := '$' + BufferToHex(FCode, SizeOf(FCode));
  if not HexToBuffer(Result, Work, SizeOf(Work)) then
    Result := '';

  if HexStringIsZero(Result) then
    Result := '';
end;

function TOgCodeBase.GetModifier : string;
var
  Work : LongInt;
begin
  Result := '$' + BufferToHex(FModifier, SizeOf(FModifier));
  if not HexToBuffer(Result, Work, SizeOf(Work)) then
    Result := '';

  if HexStringIsZero(Result) then
    Result := '';
end;

function TOgCodeBase.GetAbout : string;                              {!!.08}
begin
  Result := OgVersionStr;
end;

function TOgCodeBase.IsCodeValid : Boolean;
begin
  Result := (CheckCode(False) = ogValidCode);
end;

procedure TOgCodeBase.Loaded;
begin
  inherited Loaded;

  if FAutoCheck and not (csDesigning in ComponentState) then
    CheckCode(True);
end;

procedure TOgCodeBase.SetCode(const Value : string);
begin
  if not HexToBuffer(Value, FCode, SizeOf(FCode)) then
    FillChar(FCode, SizeOf(FCode), 0);
end;

procedure TOgCodeBase.SetModifier(const Value : string);
begin
  if not HexToBuffer(Value, FModifier, SizeOf(FModifier)) then
    FModifier := 0;
end;

procedure TOgCodeBase.SetAbout(const Value : string);                {!!.08}
begin
end;


{*** TOgDateCode ***}

function TOgDateCode.CheckCode(Report : Boolean) : TCodeStatus;
var
  ACode     : TCode;
  Key      : TKey;
  AModifier : LongInt;
begin
  Result := ogValidCode;

  DoOnGetKey(Key);
  ACode := DoOnGetCode;
  AModifier := DoOnGetModifier;

  ApplyModifierToKeyPrim(AModifier, Key, SizeOf(Key));
  if IsDateCodeValid(Key, ACode) then begin
    if IsDateCodeExpired(Key, ACode) then
      Result := ogPastEndDate;
  end else
  begin
    Result := ogInvalidCode;
    if GetInvalidCountValue(Key,ACode)=1 then Result := ogCodeExpired;
  end;


  if Report then
    DoOnChecked(Result);
end;

function TOgDateCode.GetValue : TDateTime;
var
  ACode     : TCode;
  Key      : TKey;
  AModifier : LongInt;
begin
  DoOnGetKey(Key);
  ACode := DoOnGetCode;
  AModifier := DoOnGetModifier;

  ApplyModifierToKeyPrim(AModifier, Key, SizeOf(Key));
  Result := GetDateCodeValue(Key, ACode);
end;

function TOgDateCode.GetInvalidCount: LongInt;
var
  ACode     : TCode;
  Key      : TKey;
  AModifier : LongInt;
begin
  DoOnGetKey(Key);
  ACode := DoOnGetCode;
  AModifier := DoOnGetModifier;

  ApplyModifierToKeyPrim(AModifier, Key, SizeOf(Key));
  Result := GetInvalidCountValue(Key, ACode);
end;


{*** TOgDaysCode ***}

function TOgDaysCode.CheckCode(Report : Boolean) : TCodeStatus;
var
  ACode     : TCode;
  Key      : TKey;
  AModifier : LongInt;
begin
  Result := ogValidCode;

  DoOnGetKey(Key);
  ACode := DoOnGetCode;
  AModifier := DoOnGetModifier;

  ApplyModifierToKeyPrim(AModifier, Key, SizeOf(Key));
  if IsDaysCodeValid(Key, ACode) then begin
    if IsDaysCodeExpired(Key, ACode) then begin
      Result := ogDayCountUsed;
      if GetExpirationDate(Key, ACode) < Date then
        Result := ogCodeExpired;
    end;
  end else
  begin
    Result := ogInvalidCode;
    if GetInvalidCountValue(Key,ACode)=1 then Result := ogCodeExpired;
  end;



  if Report then
    DoOnChecked(Result);
end;

constructor TOgDaysCode.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FAutoDecrease := DefAutoDecrease;
end;

procedure TOgDaysCode.Decrease;
var
  ACode     : TCode;
  Work     : TCode;
  Key      : TKey;
  AModifier : LongInt;
begin
  DoOnGetKey(Key);
  ACode := DoOnGetCode;
  Work := ACode;
  AModifier := DoOnGetModifier;
  ApplyModifierToKeyPrim(AModifier, Key, SizeOf(Key));

  {code is only decreased once per day - no matter how many times called}
  DecDaysCode(Key, Work);

  {save code if it was changed}
  if (Work.CheckValue <> ACode.CheckValue) or (Work.Days <> ACode.Days) then
    DoOnChangeCode(Work);
end;

procedure TOgDaysCode.DoOnChangeCode(Value : TCode);
begin
  if Assigned(FOnChangeCode) then
    FOnChangeCode(Self, Value)
  else
    raise EOnGuardException.CreateFmt(SCNoOnChangeCode, [Self.ClassName]);
end;

function TOgDaysCode.GetValue : LongInt;
var
  ACode     : TCode;
  Key      : TKey;
  AModifier : LongInt;
begin
  DoOnGetKey(Key);
  ACode := DoOnGetCode;
  AModifier := DoOnGetModifier;

  ApplyModifierToKeyPrim(AModifier, Key, SizeOf(Key));
  Result := GetDaysCodeValue(Key, ACode);
end;


function TOgDaysCode.GetInvalidCount : LongInt;
var
  ACode     : TCode;
  Key      : TKey;
  AModifier : LongInt;
begin
  DoOnGetKey(Key);
  ACode := DoOnGetCode;
  AModifier := DoOnGetModifier;

  ApplyModifierToKeyPrim(AModifier, Key, SizeOf(Key));
  Result := GetInvalidCountValue(Key, ACode);
end;


procedure TOgDaysCode.Loaded;
begin
  inherited Loaded;

  if FAutoDecrease and not (csDesigning in ComponentState) then
    Decrease;
end;


{*** TOgMakeCodes ***}

constructor TOgMakeCodes.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FCodeType := DefCodeType;
  FShowHints := False;
end;

function TOgMakeCodes.Execute : Boolean;
{$IFDEF MakeCodesSupport}
var
  F : TCodeGenerateFrm;
{$ENDIF}
begin
{$IFDEF MakeCodesSupport}
  F := TCodeGenerateFrm.Create(Owner);
  try
    F.CodeType := FCodeType;
    F.SetKey(FKey);                                                  {!!.08}
    F.KeyType := FKeyType;
    F.KeyFileName := FKeyFileName;
    F.ShowHint := FShowHints;
    Result := F.ShowModal = 1; // was mrOK but that pulls in a GUI framework
    if Result then begin
      FCode := F.Code;
      F.GetKey(FKey);                                                {!!.08}
      FKeyType := F.KeyType;
      FKeyFileName := F.KeyFileName;
    end;
  finally
    F.Free;
  end;
{$ELSE}                                                              {!!.10}
  Result := False;                                                   {!!.10}
{$ENDIF}                                                             {!!.10}
end;

function TOgMakeCodes.GetAbout : string;                             {!!.08}
begin
  Result := OgVersionStr;
end;

procedure TOgMakeCodes.SetAbout(const Value : string);               {!!.08}
begin
end;

procedure TOgMakeCodes.GetCode(var Value : TCode);                   {!!.08}
begin
  Value := FCode;
end;

procedure TOgMakeCodes.SetCode(Value : TCode);                       {!!.08}
begin
  FCode := Value;
end;

procedure TOgMakeCodes.GetKey(var Value : TKey);                     {!!.08}
begin
  Value := FKey;
end;

procedure TOgMakeCodes.SetKey(Value : TKey);                         {!!.08}
begin
  FKey := Value;
end;


{*** TOgMakeKeys ***}

constructor TOgMakeKeys.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FKeyType := DefKeyType;
  FShowHints := False;
end;

function TOgMakeKeys.Execute : Boolean;
{$IFDEF MakeKeysSupport}
var
  F : TKeyMaintFrm;
{$ENDIF}
begin
{$IFDEF MakeKeysSupport}
  F := TKeyMaintFrm.Create(Owner);
  try
    F.SetKey(FKey);                                                  {!!.08}
    F.KeyType := FKeyType;
    F.KeyFileName := FKeyFileName;
    F.ShowHint := FShowHints;
    Result := F.ShowModal = 1;  // was mrOK but that pulls in a GUI framework
    if Result then begin
      F.GetKey(FKey);                                                {!!.08}
      FKeyType := F.KeyType;                                       
      FKeyFileName := F.KeyFileName;
    end;
  finally
    F.Free;
  end;
{$ELSE}                                                              {!!.10}
  Result := False;                                                   {!!.10}
{$ENDIF}                                                             {!!.10}
end;

procedure TOgMakeKeys.ApplyModifierToKey(Modifier : LongInt; var Key; KeySize : Cardinal);
begin
  ApplyModifierToKeyPrim(Modifier, Key, KeySize);
end;

function TOgMakeKeys.GenerateDateModifier(D : TDateTime) : LongInt;
begin
  Result := GenerateDateModifierPrim(D);
end;

function TOgMakeKeys.GenerateMachineModifier : LongInt;
begin
  Result := GenerateMachineModifierPrim;
end;

procedure TOgMakeKeys.GenerateMDKey(var Key; KeySize : Cardinal; const Str : string);
begin
  GenerateTMDKeyPrim(Key, KeySize, Str);
end;

procedure TOgMakeKeys.GenerateRandomKey(var Key; KeySize : Cardinal);
begin
  GenerateRandomKeyPrim(Key, KeySize);
end;

function TOgMakeKeys.GenerateUniqueModifier : LongInt;
begin
  Result := GenerateUniqueModifierPrim;
end;

function TOgMakeKeys.GenerateStringModifier(const S : string) : LongInt;
begin
  Result := GenerateStringModifierPrim(S);
end;

function TOgMakeKeys.GetAbout : string;                              {!!.08}
begin
  Result := OgVersionStr;
end;

procedure TOgMakeKeys.SetAbout(const Value : string);                {!!.08}
begin
end;

procedure TOgMakeKeys.GetKey(var Value : TKey);                      {!!.08}
begin
  Value := FKey;
end;

procedure TOgMakeKeys.SetKey(Value : TKey);                          {!!.08}
begin
  FKey := Value;
end;


{*** TOgRegistrationCode ***}

function TOgRegistrationCode.CheckCode(Report : Boolean) : TCodeStatus;
var
  ACode     : TCode;
  Key      : TKey;
  AModifier : LongInt;
  {RegStr   : string;}                                                 {!!.02}
begin
  Result := ogValidCode;

  FRegString := DoOnGetRegString;                                      {!!.02}
  DoOnGetKey(Key);
  ACode       := DoOnGetCode;
  AModifier   := DoOnGetModifier;

  ApplyModifierToKeyPrim(AModifier, Key, SizeOf(Key));
  if not IsRegCodeValid(Key, ACode) then
    Result := ogInvalidCode
  else if GetExpirationDate(Key, ACode) < Date then
    Result := ogCodeExpired;

  if Report then
    DoOnChecked(Result);
end;

constructor TOgRegistrationCode.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FRegString := '';
  FStoreRegString := DefStoreRegString;
end;

function TOgRegistrationCode.DoOnGetRegString : string;
begin
  Result := '';
  if FStoreRegString then
    Result := FRegString
  else if Assigned(FOnGetRegString) then
    FOnGetRegString(Self, Result)
end;


{*** TOgSerialNumberCode ***}

function TOgSerialNumberCode.CheckCode(Report : Boolean) : TCodeStatus;
var
  ACode     : TCode;
  Key      : TKey;
  AModifier : LongInt;
begin
  Result := ogValidCode;

  DoOnGetKey(Key);
  ACode := DoOnGetCode;
  AModifier := DoOnGetModifier;

  ApplyModifierToKeyPrim(AModifier, Key, SizeOf(Key));
  if not IsSerialNumberCodeValid(Key, ACode) then
    Result := ogInvalidCode
  else if GetExpirationDate(Key, ACode) < Date then
    Result := ogCodeExpired;

  if Report then
    DoOnChecked(Result);
end;

function TOgSerialNumberCode.GetValue : LongInt;
var
  ACode     : TCode;
  Key      : TKey;
  AModifier : LongInt;
begin
  DoOnGetKey(Key);
  ACode := DoOnGetCode;
  AModifier := DoOnGetModifier;

  ApplyModifierToKeyPrim(AModifier, Key, SizeOf(Key));
  Result := GetSerialNumberCodeValue(Key, ACode);
end;


{*** TOgSpecialCode ***}

function TOgSpecialCode.CheckCode(Report : Boolean) : TCodeStatus;
var
  ACode     : TCode;
  Key      : TKey;
  AModifier : LongInt;
begin
  Result := ogValidCode;

  DoOnGetKey(Key);
  ACode := DoOnGetCode;
  AModifier := DoOnGetModifier;

  ApplyModifierToKeyPrim(AModifier, Key, SizeOf(Key));
  if not IsSpecialCodeValid(Key, ACode) then
    Result := ogInvalidCode
  else if GetExpirationDate(Key, ACode) < Date then
    Result := ogCodeExpired;

  if Report then
    DoOnChecked(Result);
end;

function TOgSpecialCode.GetValue : LongInt;
var
  ACode     : TCode;
  Key      : TKey;
  AModifier : LongInt;
begin
  DoOnGetKey(Key);
  ACode := DoOnGetCode;
  AModifier := DoOnGetModifier;

  ApplyModifierToKeyPrim(AModifier, Key, SizeOf(Key));
  Result := GetSpecialCodeValue(Key, ACode);
end;


{*** TOgUsageCode ***}

function TOgUsageCode.CheckCode(Report : Boolean) : TCodeStatus;
var
  ACode     : TCode;
  Key      : TKey;
  AModifier : LongInt;
begin
  Result := ogValidCode;

  DoOnGetKey(Key);
  ACode := DoOnGetCode;
  AModifier := DoOnGetModifier;
  ApplyModifierToKeyPrim(AModifier, Key, SizeOf(Key));

  if IsUsageCodeValid(Key, ACode) then begin
    if IsUsageCodeExpired(Key, ACode) then begin
      Result := ogRunCountUsed;
      if GetExpirationDate(Key, ACode) < Date then
        Result := ogCodeExpired;
    end;
  end else
  begin
    Result := ogInvalidCode;
    if GetInvalidCountValue(Key,ACode)=1 then Result := ogCodeExpired;
  end;



  if Report then
    DoOnChecked(Result);
end;

constructor TOgUsageCode.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  FAutoDecrease := DefAutoDecrease;
end;

procedure TOgUsageCode.Decrease;
var
  ACode     : TCode;
  Work     : TCode;
  Key      : TKey;
  AModifier : LongInt;
begin
  DoOnGetKey(Key);
  ACode := DoOnGetCode;
  Work := ACode;
  AModifier := DoOnGetModifier;
  ApplyModifierToKeyPrim(AModifier, Key, SizeOf(Key));

  {code is decreased each time this routine is called}
  DecUsageCode(Key, Work);
  {save the changed code}
  DoOnChangeCode(Work);
end;

procedure TOgUsageCode.DoOnChangeCode(Value : TCode);
begin
  if Assigned(FOnChangeCode) then
    FOnChangeCode(Self, Value)
  else
    raise EOnGuardException.CreateFmt(SCNoOnChangeCode, [Self.ClassName]);
end;

function TOgUsageCode.GetValue : LongInt;
var
  ACode     : TCode;
  Key      : TKey;
  AModifier : LongInt;
begin
  DoOnGetKey(Key);
  ACode := DoOnGetCode;
  AModifier := DoOnGetModifier;

  ApplyModifierToKeyPrim(AModifier, Key, SizeOf(Key));
  Result := GetUsageCodeValue(Key, ACode);
end;

procedure TOgUsageCode.Loaded;
begin
  inherited Loaded;

  if FAutoDecrease and not (csDesigning in ComponentState) then
    Decrease;
end;

{$IFNDEF IBO_CONSOLE}
{*** general routines ***}

procedure DecInvalidCountCode(const Key : TKey; var Code : TCode);
begin
  MixBlock(T128bit(Key), Code, False);
  if Code.InvalidCount > 0 then  Code.InvalidCount := Code.InvalidCount - 1;
  MixBlock(T128bit(Key), Code, True);
end;


function GetInvalidCountValue(const Key : TKey; const Code : TCode) : LongInt;
var
  Work : TCode;
begin
  Result := 0;
  if Code.CheckValue<>0 then
  begin
   Work := Code;
   MixBlock(T128bit(Key), Work, False);
   Result := Work.InvalidCount;
  end;
end;


function GetCodeType(const Key : TKey; const Code : TCode) : TCodeType;
var
  Work : TCode;
begin
  Work := Code;
  MixBlock(T128bit(Key), Work, False);
  case Work.CheckValue of
    DateCheckCode    : Result := ctDate;
    DaysCheckCode    : Result := ctDays;
    RegCheckCode     : Result := ctRegistration;
    SerialCheckCode  : Result := ctSerialNumber;
    UsageCheckCode   : Result := ctUsage;
    NetCheckCode     : Result := ctNetwork;
    SpecialCheckCode : Result := ctSpecial;
  else
    Result := ctUnknown;
  end;
end;

function ExpandDate(D : Word) : TDateTime;
begin
  if D > 0 then
    Result := LongInt(D) + BaseDate
  else
    Result := EncodeDate(9999, 1, 1);
end;

function ShrinkDate(D : TDateTime) : Word;
begin
  if (Trunc(D) = 0) or (Trunc(D) - BaseDate > High(Word)) then
    Result := 0
  else
    Result := Trunc(D) - BaseDate;
end;

function GetExpirationDate(const Key : TKey; const Code : TCode) : TDateTime;
var
  Work : TCode;
begin
  Work := Code;
  MixBlock(T128bit(Key), Work, False);
  case Work.CheckValue of
    DateCheckCode    : Result := ExpandDate(Work.EndDate);
    DaysCheckCode    : Result := ExpandDate(Work.Expiration);
    RegCheckCode     : Result := ExpandDate(Work.Expiration);
    SerialCheckCode  : Result := ExpandDate(Work.Expiration);
    UsageCheckCode   : Result := ExpandDate(Work.Expiration);
    SpecialCheckCode : Result := ExpandDate(Work.Expiration);
  else
    Result := ExpandDate(0)
  end;
end;

{*** date code ***}

procedure InitDateCode(const Key : TKey;
          StartDate, EndDate : TDateTime; var Code : TCode;InvalidCount : Word=0);
begin
  Code.CheckValue := DateCheckCode;
  Code.Expiration := 0; {not used for date codes}
  Code.FirstDate := ShrinkDate(StartDate);
  Code.EndDate := ShrinkDate(EndDate);
  Code.InvalidCount := InvalidCount;
  MixBlock(T128bit(Key), Code, True);
end;

function IsDateCodeValid(const Key : TKey; const Code : TCode) : Boolean;
var
  Work : TCode;
begin
  Work := Code;
  MixBlock(T128bit(Key), Work, False);
  Result := (Work.CheckValue = DateCheckCode) and
            (ExpandDate(Work.FirstDate) <= Date);
end;

function GetDateCodeValue(const Key : TKey; const Code : TCode) : TDateTime;
var
  Work : TCode;
begin
  Work := Code;
  MixBlock(T128bit(Key), Work, False);
  {return the end date}
  if (Work.CheckValue = DateCheckCode) and
     (ExpandDate(Work.FirstDate) <= Date) then
    Result := ExpandDate(Work.EndDate)
  else
    Result := 0;
end;

function IsDateCodeExpired(const Key : TKey; const Code : TCode) : Boolean;
begin
  Result := (GetDateCodeValue(Key, Code) < Date);
end;


{*** days code ***}

procedure InitDaysCode(const Key : TKey; Days : Word; Expires : TDateTime;
                       var Code : TCode;InvalidCount : Word=0);
begin
  Code.CheckValue := DaysCheckCode;
  Code.Expiration := ShrinkDate(Expires);
  Code.Days := Days;
  Code.LastAccess := ShrinkDate(Date);
  Code.InvalidCount := InvalidCount;
  MixBlock(T128bit(Key), Code, True);
end;

function IsDaysCodeValid(const Key : TKey; const Code : TCode) : Boolean;
var
  Work : TCode;
begin
  Work := Code;
  MixBlock(T128bit(Key), Work, False);
  Result := (Work.CheckValue = DaysCheckCode) and
            (ExpandDate(Work.LastAccess) <= Date);
  { 2014-03-20 graemeg:
    Disabled the InvalidCount check because it is way to easy to circumvent
    the security by simply editing the .ini file. }
//  if (not Result) and (Work.InvalidCount >0) then Result := true;
end;

procedure DecDaysCode(const Key : TKey; var Code : TCode);
var
  X : LongInt;
  Valid : Boolean;
begin
  MixBlock(T128bit(Key), Code, False);
  Valid := (Code.CheckValue = DaysCheckCode) and
            (ExpandDate(Code.LastAccess) <= Date);

  X := ShrinkDate(Date);
  if (Code.LastAccess <> X) then begin
    if Code.Days > 0 then                                              {!!.02}
      Code.Days := Max(0, Code.Days - 1);                              {!!.02}
    Code.LastAccess := X;
  end;
  if (not Valid) and (Code.InvalidCount>0) then
   Code.InvalidCount := Code.InvalidCount-1;
  MixBlock(T128bit(Key), Code, True);
end;

function GetDaysCodeValue(const Key : TKey; const Code : TCode) : LongInt;
var
  Work : TCode;
begin
  Work := Code;
  MixBlock(T128bit(Key), Work, False);
  if (Work.CheckValue = DaysCheckCode) and
     (ExpandDate(Work.LastAccess) <= Date) then
    Result := Work.Days
  else
  begin
    Result := 0;
    if Work.InvalidCount>0 then Result := Work.Days;
  end;
end;



function IsDaysCodeExpired(const Key : TKey; const Code : TCode) : Boolean;
var
  Work : TCode;
begin
  Work := Code;
  MixBlock(T128bit(Key), Work, False);
  Result := (Work.Days = 0) or (ExpandDate(Work.Expiration) < Date);
end;


{*** registration code ***}

procedure InitRegCode(const Key : TKey; const RegStr : string; Expires : TDateTime; var Code : TCode);
var
  S : string;                                                          {!!.06}
  I : Integer;                                                         {!!.06}
begin
  Code.CheckValue := RegCheckCode;
  Code.Expiration := ShrinkDate(Expires);
  {strip accented characters from the registration string}             {!!.06}
  S := RegStr;                                                         {!!.06}
  for I := Length(S) downto 1 do                                       {!!.06}
    if Ord(S[I]) > 127 then                                            {!!.06}
      Delete(S, I, 1);                                                 {!!.06}
  Code.RegString := StringHashElf(AnsiUpperCase(S));                   {!!.06}
  MixBlock(T128bit(Key), Code, True);
end;

function IsRegCodeValid(const Key : TKey; const Code : TCode) : Boolean;
var
  Work : TCode;
begin
  Work := Code;
  MixBlock(T128bit(Key), Work, False);
  Result := (Work.CheckValue = RegCheckCode);
end;

function IsRegCodeExpired(const Key : TKey; const Code : TCode) : Boolean;
var
  Work : TCode;
begin
  Work := Code;
  MixBlock(T128bit(Key), Work, False);
  Result := ExpandDate(Work.Expiration) < Date;
end;


{*** serial number code ***}

procedure InitSerialNumberCode(const Key : TKey; Serial : LongInt; Expires : TDateTime; var Code : TCode);
begin
  Code.CheckValue := SerialCheckCode;
  Code.Expiration := ShrinkDate(Expires);
  Code.SerialNumber := Serial;
  MixBlock(T128bit(Key), Code, True);
end;

function IsSerialNumberCodeValid(const Key : TKey; const Code : TCode) : Boolean;
var
  Work : TCode;
begin
  Work := Code;
  MixBlock(T128bit(Key), Work, False);
  Result := (Work.CheckValue = SerialCheckCode);
end;

function GetSerialNumberCodeValue(const Key : TKey; const Code : TCode) : LongInt;
var
  Work : TCode;
begin
  Work := Code;
  MixBlock(T128bit(Key), Work, False);
  if Work.CheckValue = SerialCheckCode then
    Result := Work.SerialNumber
  else
    Result := 0;
end;

function IsSerialNumberCodeExpired(const Key : TKey; const Code : TCode) : Boolean;
var
  Work : TCode;
begin
  Work := Code;
  MixBlock(T128bit(Key), Work, False);
  Result := ExpandDate(Work.Expiration) < Date;
end;


{*** special code ***}

procedure InitSpecialCode(const Key : TKey; Value : LongInt; Expires : TDateTime; var Code : TCode);
begin
  Code.CheckValue := SpecialCheckCode;
  Code.Expiration := ShrinkDate(Expires);
  Code.Value := Value;
  MixBlock(T128bit(Key), Code, True);
end;

function IsSpecialCodeValid(const Key : TKey; const Code : TCode) : Boolean;
var
  Work : TCode;
begin
  Work := Code;
  MixBlock(T128bit(Key), Work, False);
  Result := (Work.CheckValue = SpecialCheckCode);
end;

function GetSpecialCodeValue(const Key : TKey; const Code : TCode) : LongInt;
var
  Work : TCode;
begin
  Work := Code;
  MixBlock(T128bit(Key), Work, False);
  if Work.CheckValue = SpecialCheckCode then
    Result := Work.Value
  else
    Result := 0;
end;

function IsSpecialCodeExpired(const Key : TKey; const Code : TCode) : Boolean;
var
  Work : TCode;
begin
  Work := Code;
  MixBlock(T128bit(Key), Work, False);
  Result := ExpandDate(Work.Expiration) < Date;
end;


{*** usage code ***}

procedure InitUsageCode(const Key : TKey; Count : Word; Expires : TDateTime; var Code : TCode;InvalidCount : Word=0);
begin
  Code.CheckValue := UsageCheckCode;
  Code.Expiration := ShrinkDate(Expires);
  Code.UsageCount := Count;
  Code.LastChange := ShrinkDate(Date);                                 {!!.02}
  Code.InvalidCount := InvalidCount;
  MixBlock(T128bit(Key), Code, True);
end;

function IsUsageCodeValid(const Key : TKey; const Code : TCode) : Boolean;
var
  Work : TCode;
begin
  Work := Code;
  MixBlock(T128bit(Key), Work, False);
  Result := (Work.CheckValue = UsageCheckCode) and                     {!!.02}
            (ExpandDate(Work.LastChange) <= Date);                     {!!.02}
end;

procedure DecUsageCode(const Key : TKey; var Code : TCode);
var                                                                    {!!.02}
  D : Word;                                                            {!!.02}
begin
  MixBlock(T128bit(Key), Code, False);
  D := ShrinkDate(Date);                                               {!!.02}
  if Code.UsageCount > 0 then                                          {!!.02}
    Code.UsageCount := Max(0, Code.UsageCount - 1);                    {!!.02}
  if (Code.LastChange < D) then                                        {!!.02}
    Code.LastChange := D;                                              {!!.02}

  MixBlock(T128bit(Key), Code, True);
end;

function GetUsageCodeValue(const Key : TKey; const Code : TCode) : LongInt;
var
  Work : TCode;
begin
  Work := Code;
  MixBlock(T128bit(Key), Work, False);
  if (Work.CheckValue = UsageCheckCode) and                            {!!.02}
     (ExpandDate(Work.LastChange) <= Date) then                        {!!.02}
    Result := Work.UsageCount                                          {!!.02}
  else
    Result := 0;
end;

function IsUsageCodeExpired(const Key : TKey; const Code : TCode) : Boolean;
var
  Work : TCode;
begin
  Work := Code;
  MixBlock(T128bit(Key), Work, False);
  Result := (Work.UsageCount = 0) or (ExpandDate(Work.Expiration) < Date);
end;
{$ENDIF}




initialization
  {record our baseline date}
  BaseDate := Trunc(EncodeDate(1996, 1, 1));

end.



