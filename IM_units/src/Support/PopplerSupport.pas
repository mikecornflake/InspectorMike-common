Unit PopplerSupport;

{$mode objfpc}{$H+}

{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : PopplerSupport.pas
  Description
    CLI support for Poppler

  Source
    Copyright (c) 2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    2026-07-23: Created Unit

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

Interface

Uses
  Classes, SysUtils;

Function PopplerAvailable: Boolean;
Function PopplerPath: String;
Procedure SetPopplerPath(AValue: String);
Procedure InitializePoppler;

Function Poppler_PDFtoPPMExe: String;
Function Poppler_PDFInfoExe: String;
Function Poppler_PDFInfo(APDFFilename: String): String;
Function Poppler_Readme: String;
Function Poppler_License: String;

Implementation

Uses
  Forms, FileSupport, OSSupport, FileUtil;

Var
  FPopplerPath: String;

Function PopplerAvailable: Boolean;
Begin
  Result := FPopplerPath <> '';
End;

Function PopplerPath: String;
Begin
  Result := FPopplerPath;
End;

Procedure SetPopplerPath(AValue: String);
Begin
  If DirectoryExists(AValue) Then
    FPopplerPath := AValue
  Else
    FPopplerPath := '';
End;

Procedure InitializePoppler;
Var
  sFile: String;
Begin
  If FPopplerPath = '' Then
    sFile := FindSupportFileInFolders('Apps', 'Poppler\bin', 'poppler.dll');

  If sFile <> '' Then
    FPopplerPath := IncludeTrailingBackslash(ExtractFileDir(sFile));
End;

Function Poppler_PDFToPPMExe: String;
Begin
  Result := IncludeSlash(PopplerPath) + 'pdftoppm' + GetExeExt;

  If Not FileExists(Result) Then
    Result := '';
End;

Function Poppler_PDFInfoExe: String;
Begin
  Result := IncludeSlash(PopplerPath) + 'pdfinfo' + GetExeExt;

  If Not FileExists(Result) Then
    Result := '';
End;

Function Poppler_PDFInfo(APDFFilename: String): String;
Begin
  If (Poppler_PDFInfoExe <> '') And FileExists(APDFFilename) Then
    Result := RunAndCapture(Format('%s "%s"', [Poppler_PDFInfoExe, APDFFilename]));
End;

Function Poppler_Readme: String;
Begin
  Result := '';

  If FileExists(FPopplerPath + 'readme.txt') Then
    Result := FPopplerPath + 'readme.txt'
  Else If FileExists(FPopplerPath + 'readme.md') Then
    Result := FPopplerPath + 'readme.md'
  Else If FileExists(FPopplerPath + 'readme') Then
    Result := FPopplerPath + 'readme';
End;

Function Poppler_License: String;
Begin
  If FileExists(FPopplerPath + 'license.txt') Then
    Result := FPopplerPath + 'license.txt'
  Else If FileExists(FPopplerPath + 'license.md') Then
    Result := FPopplerPath + 'license.md'
  Else If FileExists(FPopplerPath + 'license') Then
    Result := FPopplerPath + 'license';
End;

Initialization
  FPopplerPath := '';

End.
