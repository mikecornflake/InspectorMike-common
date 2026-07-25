Unit XPDFSupport;

{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : XPDFSupport.pas
  Description
    CLI support for xpdf

  Source
    Copyright (c) 2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    ~2008: Creation date unknown, original local SVN repository lost
    2014-07-05: Uploaded to SourceForge/Package "Shared"
    2024-01-22: Migrated to Github.  Refactored package to "IM_units"
    2025-11-29: Added this header
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
  Classes, SysUtils, ThirdPartySupport;

Type

  { TXPDFSupport }

  TXPDFSupport = Class(TThirdParty)
  Private
  Public
    Constructor Create; Override;

    { PDFInfo.exe }

    Function Info(APDFFilename: String): String;
  End;

Function XPDF: TXPDFSupport;

Const
  THIRDPARTY_XPDF = 'XPDF';

Implementation

Uses
  Forms, FileSupport, OSSupport, FileUtil;

Var
  FXPDF: TXPDFSupport;

Function XPDF: TXPDFSupport;
Begin
  If Not Assigned(FXPDF) Then
    FXPDF := TXPDFSupport.Create;

  Result := FXPDF;
End;

{ TFFmpegSupport }

Constructor TXPDFSupport.Create;
Var
  oDef: TThirdPartyDefinition;
Begin
  oDef.Kind := tpkCommandLineTool;

  // CLI, we don't care if the exe is 32bit or 64bit
  oDef.CPUSensitive := False;

  // Preparation for default Initialise
  oDef.KeyFile := 'pdfinfo' + GetExeExt;
  oDef.KeyFolder := 'XPDF\bin32';

  // Metadata
  oDef.Name := THIRDPARTY_XPDF;
  oDef.Summary := 'The Xpdf open source project includes a PDF viewer along with a collection of command line tools which perform various functions on PDF files:';
  oDef.ProjectURL := 'https://www.xpdfreader.com';
  oDef.CodeURL := 'https://www.xpdfreader.com/download.html';

  Inherited Create(oDef);

  // This unit self registers
  FUsed := True;
End;

Function TXPDFSupport.Info(APDFFilename: String): String;
Var
  sInfo: String;
Begin
  Result := '';

  If Not FAvailable Then
    Exit;

  sInfo := IncludeSlash(FFolder) + 'pdfinfo' + GetExeExt;

  If (Not FileExists(sInfo)) Or (Not FileExists(APDFFilename)) Then
    Exit;

  Result := RunAndCapture(Format('"%s" "%s"', [sInfo, APDFFilename]));
End;

Initialization
  FXPDF := nil;

Finalization;
  // Free'd by ThirdParties owner
  //FreeAndNil(FXPDF);

End.
