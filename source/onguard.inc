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
 * ***** END LICENSE BLOCK ***** *)
{*********************************************************}
{*                  ONGUARD.INC 1.13                     *}
{*     Copyright (c) 1996-02 TurboPower Software Co      *}
{*                 All rights reserved.                  *}
{*********************************************************}

{Conditional defines that affect compilation}


{$B-} {Complete Boolean Evaluation}
{$I+} {Input/Output-Checking}
{$P+} {Open Parameters}
{$T-} {Typed @ Operator}



(*
Options below not needed ?
Fixme.

{$Q-} {Overflow Checking}
{$R-} {Range-Checking}
{$S-} {Stack-Overflow Checking}
{$V-} {Var-String Checking}

{$IFNDEF WINDOWS}
  {$F-} {Force Far Calls}
  {$K-} {Smart Callbacks}
{$ENDIF}
*)

{$IFDEF FPC}
 {$MODE OBJFPC}{$H+}
 {$ASMMODE INTEL}
{$ENDIF}

{====General defines=================================================}


{Activate the following define to include the TOgMakeCodes Form so it is
 compiled into the exe. }

{.$DEFINE MakeCodesSupport}


{Activate the following define to include the TOgMakeKeys Form so it is
 compiled into the exe. }

{.$DEFINE MakeKeysSupport}


