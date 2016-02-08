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
 * Andrew Haines         andrew@haines.name                        {AH.02}
 *                       added conditional define "IBO_CONSOLE"    {AH.02}
 *                       this is to allow for making console       {AH.02}
 *                       applications with Kylix and not require   {AH.02}
 *                       X11 being linked in.                      {AH.02}
 *                       January 07, 2004                          {AH.02}
 *
 * ***** END LICENSE BLOCK ***** *)
{*********************************************************}
{*                   OGUTIL.PAS 1.13                     *}
{*     Copyright (c) 1996-02 TurboPower Software Co      *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$I onguard.inc}


unit ogutil;
  {-general constants, types, and utility routines}

interface

uses
  SysUtils
{$IFDEF Unix}
  ,BaseUnix
{$ENDIF}
  ;

const
  DefAutoCheck      = True;
  DefAutoDecrease   = True;
  DefCheckSize      = True;
  DefStoreCode      = False;
  DefStoreModifier  = False;
  DefStoreRegString = False;


const
  {largest structure that can be created}
  {$IFDEF WINDOWS}
  MaxStructSize = 1024 * 2000000; {2G}
  {$ELSE}
  MaxStructSize = 1024 * 64 - 1;  {64K}
  {$ENDIF}

type

  PLongIntArray = ^TLongIntArray;
  TLongIntArray = array [0..MaxStructSize div SizeOf(LongInt) - 1] of LongInt;

  TLongIntRec = record
    case Byte of
      1: (Lo: Word;
          Hi: Word);
      2: (LoLo: Byte;
          LoHi: Byte;
          HiLo: Byte;
          HiHi: Byte);
  end;


(*
  moved these here from onguard.pas since I needed them for console work
*)
{$IFDEF IBO_CONSOLE}                                               {AH.02}
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
  TMD5Context = array [0..87] of Byte;
  TMD5Digest  = array [0..15] of Byte;

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


function GetCodeType(const Key : TKey; const Code : TCode) : TCodeType;
  {-return the type of code}
function GetExpirationDate(const Key : TKey; const Code : TCode) : TDateTime;
  {-return the date this code expires}

procedure InitDateCode(const Key : TKey; StartDate, EndDate : TDateTime; var Code : TCode);
function IsDateCodeValid(const Key : TKey; const Code : TCode) : Boolean;
function GetDateCodeValue(const Key : TKey; const Code : TCode) : TDateTime;
function IsDateCodeExpired(const Key : TKey; const Code : TCode) : Boolean;

procedure InitDaysCode(const Key : TKey; Days : Word; Expires : TDateTime; var Code : TCode);
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

procedure InitUsageCode(const Key : TKey; Count : Word; Expires : TDateTime; var Code : TCode);
function IsUsageCodeValid(const Key : TKey; const Code : TCode) : Boolean;
procedure DecUsageCode(const Key : TKey; var Code : TCode);
function GetUsageCodeValue(const Key : TKey; const Code : TCode) : LongInt;
function IsUsageCodeExpired(const Key : TKey; const Code: TCode) : Boolean;


{generate key routines}
procedure GenerateRandomKeyPrim(var Key; KeySize : Cardinal);
procedure GenerateTMDKeyPrim(var Key; KeySize : Cardinal; const Str : string);
procedure GenerateMD5KeyPrim(var Key: TKey; const Str : string);

{modifier routines}
function CreateMachineID(MachineInfo : TEsMachineInfoSet) : LongInt;   {!!.05}
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

{compressed code routines}
{
procedure CompressCodes(const Codes : Pointer; CodesSize : Integer; var Buffer : Pointer; var BufSize : Integer);
function CompressCodesStr(const CodeStr : String): String;
procedure DecompressCodes(const Codes : Pointer; var Buffer : Pointer);
function DecompressCodesStr(const CodeStr : String; const OutEst : Integer): String;
}
const
  BaseDate : LongInt = 0;

{$ENDIF}

function BufferToHex(const Buf; BufSize : Cardinal) : string;
function BufferToHexBytes(const Buf; BufSize : Cardinal) : string;
{$IFNDEF WINDOWS}
function GetDiskSerialNumber(Drive : AnsiChar) : LongInt;
function MyHashElf(const Buf;  BufSize : LongInt) : LongInt;
{$ENDIF}
function HexStringIsZero(const Hex : string) : Boolean;
function HexToBuffer(const Hex : string; var Buf; BufSize : Cardinal) : Boolean;
function Max(A, B : LongInt): LongInt;
function Min(A, B : LongInt) : LongInt;
procedure XorMem(var Mem1; const Mem2; Count : Cardinal);
function OgFormatDate(Value : TDateTime) : string;                     {!!.09}

{file related routines}
function GetFileSize(Handle : THandle) : Cardinal;

{$IFDEF LINUX}
function GetDriveType(drive:Integer): Integer;
function HiWord(I: DWORD):Word;
function CoCreateGuid(out guid: TGUID): HResult;
function timeGetTime: Cardinal;
{$ENDIF}
{$IFDEF FREEBSD}
function GetDriveType(drive:Integer): Integer;
function HiWord(I: DWORD):Word;
function CoCreateGuid(out guid: TGUID): HResult;
function timeGetTime: Cardinal;
{$ENDIF}



implementation


function BufferToHex(const Buf; BufSize : Cardinal) : string;
var
  Bytes : TByteArray absolute Buf;
  I     : LongInt;
begin
  Result := '';
  for I := 0 to BufSize - 1 do
    Result := Result + IntToHex(Bytes[I], 2);
end;

function BufferToHexBytes(const Buf;  BufSize : Cardinal) : string;
var
  Bytes  : TByteArray absolute Buf;
  I      : LongInt;
  HexStr : string;
begin
  HexStr := '$';
  Result := HexStr + IntToHex(Bytes[0], 2);
  for I := 1 to BufSize - 1 do
    Result := Result + ',' + HexStr + IntToHex(Bytes[I], 2);
end;

{%region 'I think this is Win16 leftover code' -fold}
(*
type
  PMediaIDRec = ^TMediaIDRec;
  TMediaIDRec = packed record
    InfoLevel    : Word;                      {reserved for future use}
    SerialNumber : LongInt;                   {disk serial number}
    VolumeLabel  : array[0..10] of AnsiChar;  {disk volume label}
    FileSystemID : array[0..7] of AnsiChar;   {string for internal use by the OS}
  end;

type
  DPMIRegisters = record
    DI : LongInt;
    SI : LongInt;
    BP : LongInt;
    Reserved : LongInt;
    case integer of
    1 : ( BX : LongInt;
          DX : LongInt;
          CX : LongInt;
          AX : LongInt;
          Flags : Word;
          ES : Word;
          DS : Word;
          FS : Word;
          GS : Word;
          IP : Word;
          CS : Word;
          SP : Word;
          SS : Word );
    2 : ( BL, BH : Byte; EBXH : Word;
          DL, DH : Byte; EDXH : Word;
          CL, CH : Byte; ECXH : Word;
          AL, AH : Byte; EAXH : Word );
  end;

  OS = record
    O, S : Word;
  end;

function GetCPUFlags : Byte; assembler;
asm
  lahf
  mov    al,ah
end;

function SimulateRealModeInt(IntNo : Byte; var Regs : DPMIRegisters) : Word; assembler;
asm
  xor     bx,bx
  mov     bl,IntNo
  xor     cx,cx       {StackWords = 0}
  les     di,Regs
  mov     ax,0300h
  int     31h
  jc      @@ExitPoint

  xor     ax,ax
@@ExitPoint:
end;

function GetMediaID(Drive : Byte; var MediaIDRec : TMediaIDRec) : Boolean;
type
  DoubleWord = record LoWord, HiWord : Word; end;
var
  L      : LongInt;
  RP, PP : PMediaIDRec;
  Regs   : DPMIRegisters;
begin
  Result := False;
  L := GlobalDosAlloc(SizeOf(TMediaIDRec));
  if L = 0 then
    Exit;
  try
    RP := Ptr(DoubleWord(L).HiWord, 0);
    PP := Ptr(DoubleWord(L).LoWord, 0);
    FillChar(Regs, SizeOf(Regs), 0);
    with Regs do begin
      DS := OS(RP).S;
      DX := OS(RP).O;
      AX := $440D;
      BX := Drive;
      CX := $0866;
      Flags := GetCPUFlags;
    end;
    SimulateRealModeInt($21, Regs);
    if not Odd(Regs.Flags) then begin
      MediaIDRec := PP^;
      Result := True;
    end;
  finally
    GlobalDosFree(OS(PP).S);
  end;
end;

function GetDiskSerialNumber(Drive : AnsiChar) : LongInt;
var
  MR : TMediaIDRec;
begin
  if GetMediaID(Ord(UpCase(Drive))-Ord('A')+1 ,MR) then
    Result := MR.SerialNumber
  else
    Result := -1;
end;
*)
{%endregion}

{$IFDEF LINUX}
function MyHashElf(const Buf;  BufSize : LongInt) : LongInt;
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

function GetDiskSerialNumber(Drive : AnsiChar) : LongInt;
var
   boot_partition : String;
   drive_model : String;
   iFileHandle : Integer;
   Buffer : PChar;
   iFileSize : Integer;
begin
    // read /proc/cmdline
    iFileHandle := FileOpen('/proc/cmdline', fmOpenRead or fmShareDenyNone);
    iFileSize := FileSeek(iFileHandle,0,2);
    Buffer := PChar(AllocMem(iFileSize+1));
    FileSeek(iFileHandle,0,0);
    FileRead(iFileHandle, Buffer^, iFileSize);
    boot_partition := StrPas(Buffer);
    FileClose(iFileHandle);
    FreeMem(Buffer);

    // get root=/dev/? into boot_partition
    if Pos('root=/dev/', boot_partition) > 0 then
    begin
     Delete(boot_partition, 1, Pos('root=/dev/', boot_partition)-1);
     if (Pos(' ', boot_partition) > 0) then
     begin
      boot_partition := Trim(LeftStr(boot_partition, Pos(' ', boot_partition)));
      Delete(boot_partition, 1, 10);
      boot_partition := LeftStr(boot_partition,3);
     end;
    end
    else
    begin
     boot_partition := 'hda';
    end;

    if boot_partition[1] = 'h' then boot_partition := '/ide/' + boot_partition;
    if boot_partition[1] = 's' then boot_partition := '/scsi/' + boot_partition;

    // read /proc/ide/boot_partition/model
    iFileHandle := FileOpen('/proc' + boot_partition + '/model', fmOpenRead or fmShareDenyNone);
    iFileSize := FileSeek(iFileHandle,0,2);
    Buffer := PChar(AllocMem(iFileSize+1));
    FileSeek(iFileHandle,0,0);
    FileRead(iFileHandle, Buffer^, iFileSize);
    drive_model := StrPas(Buffer);
    FileClose(iFileHandle);
    FreeMem(Buffer);

    // create a hash value of the drive_model to return an integer
    Result := MyHashElf(drive_model[1], Length(drive_model));
end;
{$ENDIF}

{$IFDEF FreeBSD}
function GetDiskSerialNumber(Drive : AnsiChar) : LongInt;
begin
  {$NOTE: Still to be implemented }
  Result := 0;
end;

function MyHashElf(const Buf;  BufSize : LongInt) : LongInt;
begin
  Result := 0;
end;
{$ENDIF FreeBSD}

function HexStringIsZero(const Hex : string) : Boolean;
var
  I   : Integer;
  Str : string;
begin
  Result := False;

  Str := '';
  for I := 1 to Length(Hex) do
    if Upcase(Hex[I]) in ['0'..'9', 'A'..'F'] then
      Str := Str + Hex[I];

  for I := 1 to Length(Str) do
    if Str[I] <> '0' then
      Exit;

  Result := True;
end;

function HexToBuffer(const Hex : string; var Buf; BufSize : Cardinal) : Boolean;
var
  Bytes : TByteArray absolute Buf;
  I, C  : Integer;
  Str   : string;
begin
  Result := False;

  Str := '';
  for I := 1 to Length(Hex) do
    if Upcase(Hex[I]) in ['0'..'9', 'A'..'F'] then
      Str := Str + Hex[I];

  if (Cardinal(Length(Str) div 2) <> BufSize) then                     {!!.07}
    Exit;

  for I := 0 to BufSize - 1 do begin
    Val('$' + Copy(Str, (I shl 1) + 1, 2), Bytes[I], C);
    if (C <> 0) then
      Exit;
  end;

  Result := True;
end;

function Max(A, B : LongInt) : LongInt;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;


function Min(A, B : LongInt) : LongInt;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

{ Functions below were replaced by
not fully compatible but easier pure pascal version
to do it multiplatform}

(*
{$IFDEF WINDOWS}
procedure XorMem(var Mem1; const Mem2; Count : Cardinal); register;
begin
asm
  push esi
  push edi

  mov  esi, eax         //esi = Mem1
  mov  edi, edx         //edi = Mem2

  push ecx              //save byte count
  shr  ecx, 2           //convert to dwords
  jz   @Continue

  cld
@Loop1:                 //xor dwords at a time
  mov  eax, [edi]
  xor  [esi], eax
  add  esi, 4
  add  edi, 4
  dec  ecx
  jnz  @Loop1

@Continue:              //handle remaining bytes (3 or less)
  pop  ecx
  and  ecx, 3
  jz   @Done

@Loop2:                 //xor remaining bytes
  mov  al, [edi]
  xor  [esi], al
  inc  esi
  inc  edi
  dec  ecx
  jnz  @Loop2

@Done:
  pop  edi
  pop  esi
end;
end;
{$ELSE}
{!!.02} {revised}
{$IFNDEF LINUX}
procedure XorMem(var Mem1; const Mem2; Count : Cardinal); assembler;
asm
  push  ds
  push  es
  lds   si, Mem2
  les   di, Mem1
  mov   cx, Count
  jz    @Done
  cld
@Loop1:
  mov  al, ds:[si]
  xor  es:[di], al
  inc  si
  inc  di
  dec  cx
  jnz  @Loop1
@Done:
  pop  es
  pop  ds
end;
{$ENDIF}
{$ENDIF}

{$IFDEF LINUX}
procedure XorMem(var Mem1; const Mem2; Count : Cardinal); register;
begin
asm
  push esi
  push edi

  mov  esi, eax         //esi = Mem1
  mov  edi, edx         //edi = Mem2

  push ecx              //save byte count
  shr  ecx, 2           //convert to dwords
  jz   @Continue

  cld
@Loop1:                 //xor dwords at a time
  mov  eax, [edi]
  xor  [esi], eax
  add  esi, 4
  add  edi, 4
  dec  ecx
  jnz  @Loop1

@Continue:              //handle remaining bytes (3 or less)
  pop  ecx
  and  ecx, 3
  jz   @Done

@Loop2:                 //xor remaining bytes
  mov  al, [edi]
  xor  [esi], al
  inc  esi
  inc  edi
  dec  ecx
  jnz  @Loop2

@Done:
  pop  edi
  pop  esi
end;
end;
{$ENDIF}
*)

procedure XorMem(var Mem1; const Mem2; Count : Cardinal);
var
 pB1,pB2 : PByte;
 B1,B2  : Byte;
 i : Cardinal;
begin
if Count = 0 then Exit;
i := 0;
pB1 := PByte(@Mem1);//Mem1
pB2 := PByte(@Mem2);//Mem2
while i < Count do
begin
 B1 := pB1^;
 B2 := pB2^;
 pB1^ := B1 xor B2;
 Inc(pB1);
 Inc(pB2);
 Inc(i);
end;
end;





{!!.09}
function OgFormatDate(Value : TDateTime) : string;
  {convert date to string with 4-digit year and 2-digit month}
var
  S : string;
begin
  FormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  {
  S := ShortDateFormat;
  if Pos('yyyy', S) = 0 then
    Insert('yy', S, Pos('yy', S));
  if Pos('MMM', S) > 0 then
    Delete(S, Pos('MMM', S), 1);
    }
  Result := DateToStr(Value);//FormatDateTime(S, Value)
end;


{file related routines}
function GetFileSize(Handle : THandle) : Cardinal;
var
  Save : LongInt;
begin
  Save := FileSeek(Handle, 0, 0);     {save current file position}
  Result := FileSeek(Handle, 0, 2);   {get file size}
  FileSeek(Handle, Save, 0);          {restore previous position}
end;

{$IFNDEF WINDOWS}
function GetLastError: Integer;
begin
  Result := FpGetErrno;
end;

procedure SetLastError(Value: Integer);
begin
  FpSetErrno(Value);
end;
{$ENDIF}


(*
{$IFDEF WINDOWS}
function RolByteX(I, C : Byte) : Byte; register;
asm
  mov  cl, dl
  rol  al, cl
end;
{$ELSE}
function RolByteX(I, C : Byte) : Byte; assembler;
asm
  mov  al, I
  mov  cl, C
  rol  al, cl
end;
{$ENDIF}
*)


{$IFDEF LINUX}
function GetDriveType(drive:Integer): Integer;
const
  DRIVE_UNKNOWN = 0;
  DRIVE_NO_ROOT_DIR = 1;
  DRIVE_REMOVABLE = 2;
  DRIVE_FIXED = 3;
  DRIVE_REMOTE = 4;
  DRIVE_CDROM = 5;
  DRIVE_RAMDISK = 6;
var
   f: TextFile;
   fn : String;
   media : String;
begin
 Result := DRIVE_UNKNOWN;
 // drive = 1-25 (A-Z)

 //assuming IDE drives
 //assuming C: = hda
 case drive of
  1: fn := '';
  2: fn := '';
  3: fn := 'hda';
  4: fn := 'hdb';
  5: fn := 'hdc';
  6: fn := 'hdd';
  7: fn := '';
  8..25: fn := '';
 end;

 if fn = '' then
 begin
  Result := DRIVE_UNKNOWN;
 end
 else
 begin
 {$I-}
  if fn[1] = 'h' then AssignFile(f, '/proc/ide/' + fn + '/media');
  Reset(f);
  media := '';
  if IoResult=0 then ReadLn(f, media)
  else
  Exit;
 {$I+}

  if media = 'disk' then Result := DRIVE_FIXED;
  if media = 'cdrom' then Result := DRIVE_CDROM;
  if media = 'floppy' then Result := DRIVE_REMOVABLE;

  CloseFile(f);
 end;
end;

function HiWord(I: DWORD):Word;
begin
 Result := I shl 16;
 Result := I and $FFFF;
end;

function CoCreateGuid(out guid: TGUID): HResult;
begin
  Result := CreateGuid(Guid);
end;

function timeGetTime: Cardinal;
begin
 Result := Cardinal(Trunc(Now * 24 * 60 * 60 * 1000));
end;
{$ENDIF Linux}

{$IFDEF FreeBSD}
function GetDriveType(drive: Integer): Integer;
begin

end;

function HiWord(I: DWORD): Word;
begin
  Result := I shl 16;
  Result := I and $FFFF;
end;

function CoCreateGuid(out guid: TGUID): HResult;
begin
  Result := CreateGuid(Guid);
end;

function timeGetTime: Cardinal;
begin
  Result := Cardinal(Trunc(Now * 24 * 60 * 60 * 1000));
end;
{$ENDIF FreeBSD}

{$IFDEF IBO_CONSOLE}
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
    AA := AA + DD; DD := DD + AA; AA := AA xor (AA shr 7);
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

{internal routines for MD5}
type
  TMD5ContextEx = record
    Count : array [0..1] of DWord;  {number of bits handled mod 2^64}
    State : array [0..3] of DWord;  {scratch buffer}
    Buf   : array [0..63] of Byte;    {input buffer}
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
{!!.05} {added}
function CreateMachineID(MachineInfo : TEsMachineInfoSet) : LongInt;
{ Obtains information from:
    - Volume sizes (NOT free space)
    - Volume serial numbers
    - Registration name and company
    - GetSystemInfo relevant info
    - Network card ID (if available)
}
const
  sCurVer   = 'Software\Microsoft\Windows\CurrentVersion';           {!!.11}
  sCurVerNT = 'Software\Microsoft\Windows NT\CurrentVersion';        {!!.11}
  sRegOwner = 'RegisteredOwner';                                     {!!.11}
  sRegOrg   = 'RegisteredOrganization';                              {!!.11}

type                                                                     {!!.11}
  TUuidCreateSequential = function (lpGUID : Pointer): HResult; stdcall; {!!.11}

var
  hRPCTR4 : THandle;                                                 {!!.11}
  UuidCreateSequential : TUuidCreateSequential;                      {!!.11}
  I       : DWord;
  RegKey  : HKEY;
  GUID1   : TGUID;
  GUID2   : TGUID;
  Drive   : AnsiChar;
  SysInfo : TSystemInfo;
  Context : TTMDContext;
  UserInfoFound : Boolean;                                           {!!.11}
  Buf     : array [0..1023] of Byte;
  iController, iDrive, maxController : Integer;
  BufStr : AnsiString;
begin
  InitTMD(Context);

  {include user specific information}
  if midUser in MachineInfo then begin
{!!.11}
    UserInfoFound := False;
    { first look for registered info in \Windows\CurrentVersion }
    if (RegOpenKeyEx(HKEY_LOCAL_MACHINE, sCurVer, 0,
        KEY_QUERY_VALUE, RegKey) = ERROR_SUCCESS) then begin
      I := SizeOf(Buf);
      if RegQueryValueEx(RegKey, sRegOwner, nil, nil, @Buf, @I) = ERROR_SUCCESS then begin
        UserInfoFound := True;
        UpdateTMD(Context, Buf, I);
        I := SizeOf(Buf);
        if RegQueryValueEx(RegKey, sRegOrg, nil, nil, @Buf, @I) = ERROR_SUCCESS then
          UpdateTMD(Context, Buf, I);
      end;
      RegCloseKey(RegKey);                                           {!!.13}
    end;

{!!.11}
    { if not found, then look in \Windows NT\CurrentVersion }
    if not UserInfoFound then
      if (RegOpenKeyEx(HKEY_LOCAL_MACHINE, sCurVerNT, 0,
          KEY_QUERY_VALUE, RegKey) = ERROR_SUCCESS) then begin
        I := SizeOf(Buf);
        if RegQueryValueEx(RegKey, sRegOwner, nil, nil, @Buf, @I) = ERROR_SUCCESS then begin
          UpdateTMD(Context, Buf, I);
          I := SizeOf(Buf);
          if RegQueryValueEx(RegKey, sRegOrg, nil, nil, @Buf, @I) = ERROR_SUCCESS then
            UpdateTMD(Context, Buf, I);
        end;
        RegCloseKey(RegKey);                                         {!!.13}
      end;
  end;

  if midSystem in MachineInfo then begin
    {include system specific information}
    GetSystemInfo(SysInfo);
    PDWord(@Buf[0])^ := SysInfo.dwOemId;
    PDWord(@Buf[4])^ := SysInfo.dwProcessorType;
    UpdateTMD(Context, Buf, 8);
  end;

  if midNetwork in MachineInfo then begin
    {include network ID}
    CreateGuid(GUID1);
    CreateGuid(GUID2);

{!!.11}
    { use UuidCreateSequential instead of CoCreateGuid if available }
        hRPCTR4 := LoadLibrary('rpcrt4.dll');
        if (hRPCTR4 <> 0) then begin
          @UuidCreateSequential := GetProcAddress(hRPCTR4, 'UuidCreateSequential');
          if Assigned(UuidCreateSequential) then begin
            UuidCreateSequential(@GUID1);
            UuidCreateSequential(@GUID2);
          end;
          FreeLibrary(hRPCTR4);                                      {!!.13}
        end;
{!!.11}

    {check to see if "network" ID is available}
    if (GUID1.D4[2] = GUID2.D4[2]) and
       (GUID1.D4[3] = GUID2.D4[3]) and
       (GUID1.D4[4] = GUID2.D4[4]) and
       (GUID1.D4[5] = GUID2.D4[5]) and
       (GUID1.D4[6] = GUID2.D4[6]) and
       (GUID1.D4[7] = GUID2.D4[7]) then
      UpdateTMD(Context, GUID1.D4[2], 6);
  end;

  if midDrives in MachineInfo then begin
    {include drive specific information}
  maxController := 15;
  if Win32Platform<>VER_PLATFORM_WIN32_NT then maxController := 0;
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
{$ENDIF WINDOWS}

{%region 'I think this is leftover Win16 code' -fold}
(*
function CreateMachineID(MachineInfo : TEsMachineInfoSet) : LongInt;
var
  I       : DWord;
  RegKey  : DWord;
  GUID1   : TGUID;
  GUID2   : TGUID;
  Drive   : Integer;
  Context : TTMDContext;
  Buf     : array [0..1023] of Byte;
begin
  InitTMD(Context);

  {no user (midUser) information under Win16}

  if midSystem in MachineInfo then begin
    {include system specific information}
    I := GetWindowsDirectory(@Buf, SizeOf(Buf));
    UpdateTMD(Context, Buf, I);
    I := GetSystemDirectory(@Buf, SizeOf(Buf));
    UpdateTMD(Context, Buf, I);

    PLongInt(@Buf[0])^ := GetWinFlags;
    PLongInt(@Buf[4])^ := WinProcs.GetVersion;
    UpdateTMD(Context, Buf, 8);
  end;

  if midNetwork in MachineInfo then begin
    {include network ID}
    CreateGuid(GUID1);
    CreateGuid(GUID2);
    {check to see if "network" ID is available}
    if (GUID1.Data4[2] = GUID2.Data4[2]) and
       (GUID1.Data4[3] = GUID2.Data4[3]) and
       (GUID1.Data4[4] = GUID2.Data4[4]) and
       (GUID1.Data4[5] = GUID2.Data4[5]) and
       (GUID1.Data4[6] = GUID2.Data4[6]) and
       (GUID1.Data4[7] = GUID2.Data4[7]) then
      UpdateTMD(Context, GUID1.Data4[2], 6);
  end;

  if midDrives in MachineInfo then begin
    {include drive specific information}
    for Drive := 2 {C} to 25 {Z} do begin
      if GetDriveType(Drive) = DRIVE_FIXED then begin
        FillChar(Buf, Sizeof(Buf), 0);
        Buf[0] := Drive;
        {!!.06} {removed cluster information}
        PLongInt(@Buf[1])^ := GetDiskSerialNumber(Chr(Drive+Ord('A')));{!!.06}
        UpdateTMD(Context, Buf, 5);
      end;
    end;
  end;

  FinalizeTMD(Context, Result, SizeOf(Result));
end;
*)
{%endregion}

{$IFDEF Linux}
function CreateMachineID(MachineInfo : TEsMachineInfoSet) : LongInt;
var
  I       : DWord;
  RegKey  : DWord;
  GUID1   : TGUID;
  GUID2   : TGUID;
  Drive   : Integer;
  Context : TTMDContext;
  Buf     : array [0..2047] of Byte;
  iFileHandle : Integer;
begin
  InitTMD(Context);

  {include user specific information}
  if midUser in MachineInfo then
  begin
   //[to do] find some organization specific info
  end;

  if midSystem in MachineInfo then
  begin
    {include system specific information}
    iFileHandle := FileOpen('/proc/cpuinfo', fmopenRead or fmShareDenyNone);
    I := FileSeek(iFileHandle,0,2);
    FileSeek(iFileHandle,0,0);
    if I < 2047 then
    begin
     FileRead(iFileHandle, Buf, I);
     UpdateTMD(Context, Buf, I);
    end;
    FileClose(iFileHandle);

    iFileHandle := FileOpen('/proc/sys/kernel/version', fmopenRead or fmShareDenyNone);
    I := FileSeek(iFileHandle,0,2);
    FileSeek(iFileHandle,0,0);
    if I < 2047 then
    begin
     FileRead(iFileHandle, Buf, I);
     UpdateTMD(Context, Buf, I);
    end;
    FileClose(iFileHandle);

    iFileHandle := FileOpen('/proc/sys/kernel/osrelease', fmopenRead or fmShareDenyNone);
    I := FileSeek(iFileHandle,0,2);
    FileSeek(iFileHandle,0,0);
    if I < 2047 then
    begin
     FileRead(iFileHandle, Buf, I);
     UpdateTMD(Context, Buf, I);
    end;
    FileClose(iFileHandle);

    iFileHandle := FileOpen('/proc/sys/kernel/hostname', fmopenRead or fmShareDenyNone);
    I := FileSeek(iFileHandle,0,2);
    FileSeek(iFileHandle,0,0);
    if I < 2047 then
    begin
     FileRead(iFileHandle, Buf, I);
     UpdateTMD(Context, Buf, I);
    end;
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

  if midDrives in MachineInfo then
  begin
    {include drive specific information}
    for Drive := 2 {C} to 25 {Z} do begin
      if GetDriveType(Drive) = 3 {DRIVE_FIXED} then begin
        FillChar(Buf, Sizeof(Buf), 0);
        Buf[0] := Drive;
        {!!.06} {removed cluster information}
        PLongInt(@Buf[1])^ := GetDiskSerialNumber(Chr(Drive+Ord('A')));{!!.06}
        UpdateTMD(Context, Buf, 5);
      end;
    end;
  end;

  FinalizeTMD(Context, Result, SizeOf(Result));
end;
{$ENDIF Linux}

{$IFDEF FreeBSD}
function CreateMachineID(MachineInfo : TEsMachineInfoSet) : LongInt;
begin
  Result := 0;
end;
{$ENDIF FreeBSD}

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

  D := HashMD5(S2[1], Length(S2));                                     {!!.06}
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

{!!.05} {revised}
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

{*** general routines ***}
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
          StartDate, EndDate : TDateTime; var Code : TCode);
begin
  Code.CheckValue := DateCheckCode;
  Code.Expiration := 0; {not used for date codes}
  Code.FirstDate := ShrinkDate(StartDate);
  Code.EndDate := ShrinkDate(EndDate);
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
                       var Code : TCode);
begin
  Code.CheckValue := DaysCheckCode;
  Code.Expiration := ShrinkDate(Expires);
  Code.Days := Days;
  Code.LastAccess := ShrinkDate(Date);
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
end;

procedure DecDaysCode(const Key : TKey; var Code : TCode);
var
  X : LongInt;
begin
  MixBlock(T128bit(Key), Code, False);
  X := ShrinkDate(Date);
  if (Code.LastAccess <> X) then begin
    if Code.Days > 0 then                                              {!!.02}
      Code.Days := Max(0, Code.Days - 1);                              {!!.02}
    Code.LastAccess := X;
  end;
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
    Result := 0;
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

procedure InitUsageCode(const Key : TKey; Count : Word; Expires : TDateTime; var Code : TCode);
begin
  Code.CheckValue := UsageCheckCode;
  Code.Expiration := ShrinkDate(Expires);
  Code.UsageCount := Count;
  Code.LastChange := ShrinkDate(Date);                                 {!!.02}
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
{$IFDEF IBO_CONSOLE}                                               {AH.02}
  {from onguard.pas}
  {record our baseline date}
  BaseDate := Trunc(EncodeDate(1996, 1, 1));
{$ENDIF}



end.
