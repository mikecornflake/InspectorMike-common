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
    2026-0724: Migrated to ThirdParty framework

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
  Classes, SysUtils, ThirdPartySupport;

Type

  { TPopplerSupport }

  TPopplerSupport = Class(TThirdParty)
  Public
    Procedure DefineDefaults; Override;

    Function PDFInfo(APDFFilename: String): String;
  End;

Function Poppler: TPopplerSupport;

Implementation

Uses
  Forms, FileSupport, OSSupport, FileUtil;

Var
  FPoppler: TPopplerSupport;

Function Poppler: TPopplerSupport;
Begin
  If Not Assigned(FPoppler) Then
    FPoppler := TPopplerSupport.Create;

  Result := FPoppler;
End;

{ TPopplerSupport }

Procedure TPopplerSupport.DefineDefaults;
Begin
  // CLI, we don't care if the exe is 32bit or 64bit
  FCPUSensitive := False;

  // Preparation for default Initialise
  // Have to search for the dll as Poppler shares same
  // names as XPDF
  FKeyFile := 'poppler.dll';
  FKeyFolder := 'Poppler\bin';

  // Metadata
  FName := 'Poppler';
  FSummary := 'Poppler, a PDF rendering library ' + LineEnding +
    'This is Poppler, a library for rendering PDF files, and examining or ' +
    'modifying their structure.  Poppler originally came from the XPDF ' + 'sources';
  FProjectURL := 'https://poppler.freedesktop.org/';
  FCodeURL := 'https://gitlab.freedesktop.org/poppler/poppler';
End;

Function TPopplerSupport.PDFInfo(APDFFilename: String): String;
Var
  sPDFInfo: String;
Begin
  Result := '';

  sPDFInfo := FullExe('pdfinfo');

  If FileExists(sPDFInfo) And FileExists(APDFFilename) Then
    Result := RunAndCapture(Format('"%s" "%s"', [sPDFInfo, APDFFilename]));
End;

Initialization
  FPoppler := nil;

Finalization;
  FreeAndNil(FPoppler);

End.
