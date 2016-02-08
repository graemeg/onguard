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
 *                       January 1, 2004                           {AH.01}
 * Boguslaw Brandys      conversion to FPC
 *                       June 14, 2006
 *
 *
 * ***** END LICENSE BLOCK ***** *)
{*********************************************************}
{*                  OGFIRST.PAS 1.13                     *}
{*     Copyright (c) 1996-02 TurboPower Software Co      *}
{*                 All rights reserved.                  *}
{*********************************************************}

{$I onguard.inc}


unit ogfirst;
  {-limit instance routines}

interface

uses
{$IFDEF WINDOWS}
  Windows,
{$ENDIF}
{$IFDEF LINUX}
  BaseUnix,
//  Libc,//only because  S_IRWXU was missing from baseunix under 0.9.16
  //fix it later
{$ENDIF}                                                           {AH.01}
  Forms, SysUtils, Dialogs,LClProc;

{detect/Activate instance routines}
function IsFirstInstance: Boolean;
procedure ActivateFirstInstance;
{!!.04} {revised Win16 version}


const
 MAGIC = 'MAGIC'; {change this code to differentiate applications}

implementation


{$IFDEF WINDOWS}
var
  FirstInstance : Boolean;
  InstanceMutex : THandle;
{$ENDIF}

{$IFDEF LINUX}
var
  FirstInstance : Boolean;
  server_name : String;
  server_lock : Integer;

{$ENDIF}

{limit instances routines}
function IsFirstInstance : Boolean;
begin
  {$IFDEF WINDOWS}
  Result := FirstInstance;
  {$ELSE}
  {$IFDEF LINUX}
  Result := FirstInstance;
  {$ELSE}
  Result := HPrevInst = 0;
  {$ENDIF}
  {$ENDIF}
end;

{$IFDEF WINDOWS}
procedure ActivateFirstInstance;
var
  ClassBuf,
  WindowBuf : array [0..255] of AnsiChar;
  Wnd,
  TopWnd    : hWnd;
  ThreadID  : DWord;                                                 {!!.07}
begin
try
  if IsFirstInstance then begin
    if IsIconic(HWND(Application.MainForm.Handle)) then
      ShowWindow(HWND(Application.MainForm.Handle), SW_RESTORE)// Application.Restore
    else
      Application.BringToFront;
  end else begin
    GetClassName(HWND(Application.MainForm.Handle), ClassBuf, SizeOf(ClassBuf));
    GetWindowText(HWND(Application.MainForm.Handle), WindowBuf, SizeOf(WindowBuf));
    Wnd := FindWindow(ClassBuf, WindowBuf);
    if (Wnd <> 0) then begin
      GetWindowThreadProcessId(Wnd, @ThreadID);
      if (ThreadID = GetCurrentProcessId) then begin
        Wnd := FindWindowEx(0, Wnd, ClassBuf, WindowBuf);
        if (Wnd <> 0) then
          if IsIconic(Wnd) then
            ShowWindow(Wnd, SW_RESTORE)
          else begin
            SetForegroundWindow(Wnd);                                {!!.09}
            TopWnd := GetLastActivePopup(Wnd);
            if (TopWnd <> 0) and (TopWnd <> Wnd) and
                IsWindowVisible(TopWnd) and IsWindowEnabled(TopWnd) then
              BringWindowToTop(TopWnd)
            else
              BringWindowToTop(Wnd);
          end;
      end;
    end;
  end;
except on E:Exception do
 DebugLn('ActivateFirstInstance exception : ' + E.Message + '.Move IsFirstInstance after CreateForm for MainForm');
end;
end;
{$ELSE}
procedure ActivateFirstInstance;
begin
 //[to do] Find and Activate the first instance of the application

 //look at the owner of the socket
 //look at the running processes
end;
{$ENDIF}




{$IFDEF WINDOWS}
function GetMutexName : string;
var
  WindowBuf : array [0..512] of AnsiChar;
begin
try
  {GetWindowText(HWND(Application.MainForm.Handle), WindowBuf, SizeOf(WindowBuf));}
  Result := 'PREVINST:' + ExtractFileName(ParamStr(0)) + MAGIC;
except on E:Exception do
 DebugLn('GetMutexName exception : '  + E.Message);
end;
end;


initialization
  InstanceMutex := CreateMutex(nil, True, PAnsiChar(GetMutexName));
  if (InstanceMutex <> 0) and (GetLastError = 0) then
    FirstInstance := True
  else
    FirstInstance := False;

finalization
  if (InstanceMutex <> 0) then
    CloseHandle(InstanceMutex);
{$ENDIF}

{$IFDEF LINUX}
initialization

 server_name := ExtractFilePath(ParamStr(0)) +  ExtractFileName(ParamStr(0)) + '.lck';
 server_lock := fpopen(PChar(server_name), O_RDWR or O_CREAT or O_TRUNC or O_NOFOLLOW or O_EXCL, S_IRWXU);
 if (server_lock = -1) then
 begin
  FirstInstance := False;
  DebugLn('Failed to create lock file. (' + IntToHex(errno,4) + ')' + #10 + server_name);
 end
 else
 begin
  FirstInstance := True;
 end;

finalization
 if (server_lock > -1) then
 begin
  FileClose(server_lock);
  unlink(PChar(server_name));
 end;

{$ENDIF}

end.
