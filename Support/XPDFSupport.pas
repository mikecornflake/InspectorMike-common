Unit XPDFSupport;

{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : XPDFSupport.pas
  Description
    CLI support for xpdf

  Source
    Copyright (c) 2025
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    ~2008: Creation date unknown, original local SVN repository lost
    2014-07-05: Uploaded to SourceForge/Package "Shared"
    2024-01-22: Migrated to Github.  Refactored package to "IM_units"
    2025-11-29: Added this header

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

// Running the command line interface itself is so easy I see no
// need to subclass that code.
// This is really here just to find the directory

Interface

Uses
  Classes, SysUtils;

Function XPDFAvailable: Boolean;
Function XPDFPath: String;
Procedure SetXPDFPath(AValue: String);
Procedure InitializeXPDF;

Implementation

Uses
  Forms;

Var
  FXPDFPath: String;

Function XPDFAvailable: Boolean;
Begin
  Result := FXPDFPath <> '';
End;

Function XPDFPath: String;
Begin
  Result := FXPDFPath;
End;

Procedure SetXPDFPath(AValue: String);
Begin
  If DirectoryExists(AValue) Then
    FXPDFPath := AValue
  Else
    FXPDFPath := '';
End;

Procedure InitializeXPDF;
Begin
  FXPDFPath := IncludeTrailingBackslash(Application.Location) + 'XPDF\bin32';
  If DirectoryExists(FXPDFPath) Then
    Exit;

  FXPDFPath := IncludeTrailingBackslash(Application.Location) + '..\XPDF\bin32';
  If DirectoryExists(FXPDFPath) Then
    Exit;

  FXPDFPath := '';
End;

Initialization
  FXPDFPath := '';

End.
