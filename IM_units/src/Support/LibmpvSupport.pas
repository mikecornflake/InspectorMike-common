Unit LibmpvSupport;

{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : LazSerialSupport.pas
  Description
    My first Class Helper

    Helper unit for LazSerial.pas

  Source
    Copyright (c) 2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    2026-06-05: Creation and upload to Githib InspectorMike-Common
                   as part of  IM_common.lpk
    2026-06-19: Added this header & refactored
    2026-06-19: Refactored into split InspectorMike package structure

  License
    This file is part of IM_units.lpk.

    This library is free software: you can redistribute it and/or modify it
    under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or (at
    your option) any later version.

    This library is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
    General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this library. If not, see <https://www.gnu.org/licenses/>.

    SPDX-License-Identifier: LGPL-3.0-or-later
-------------------------------------------------------------------------------}

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils;

Function LibmpvAvailable: Boolean;
Function LibmpvDLL: String;
Procedure SetLibmpvDLL(AValue: String);
Function FindLibmpvDLL: Boolean;

Implementation

Uses
  Forms, OSSupport, FileSupport, FileUtil;

Var
  FLibmpvDLL: String;

Function LibmpvAvailable: Boolean;
Begin
  Result := (FLibmpvDLL <> '') And FileExists(FLibmpvDLL);
End;

Function LibmpvDLL: String;
Begin
  Result := FLibmpvDLL;
End;

Procedure SetLibmpvDLL(AValue: String);
Begin
  If FileExists(AValue) Then
    FLibmpvDLL := AValue
  Else
    FLibmpvDLL := '';
End;

Function FindLibmpvDLL: Boolean;
Begin
  Result := (FLibmpvDLL <> '') And FileExists(FLibmpvDLL);

  If Result Then
    Exit;

  // TODO Linux
  FLibmpvDLL := FindDriverFileInFolders('mpv', 'libmpv-2.dll');

  Result := (FLibmpvDLL <> '') And FileExists(FLibmpvDLL);

  If Result Then
    Exit
  Else
    FLibmpvDLL := '';
End;

End.
