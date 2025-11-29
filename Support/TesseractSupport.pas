Unit TesseractSupport;

{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : TesseractSupport.pas
  Description
    CLI support for Tesseract OCR tool

  Source
    Copyright (c) 2025
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    2022-10-06: Creation & Uploaded to Github
    2025-11-29: Added support for Options
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

Interface

Uses
  Classes, SysUtils;

Function TesseractAvailable: Boolean;
Function TesseractPath: String;
Function BuildOptionsString(AOEM: Integer = 3; APSM: Integer = -1;
  AWhitelist: String = ''): String;
Procedure SetTesseractPath(AValue: String);
Procedure InitializeTesseract;

Function OCR(AFilename: String; AOptions: String = ''): String;

Implementation

Uses
  Forms, StringSupport, FileUtil, OSSupport, FileSupport;

Var
  FTesseractPath: String;

Function TesseractAvailable: Boolean;
Begin
  Result := FTesseractPath <> '';
End;

Function TesseractPath: String;
Begin
  Result := IncludeTrailingBackslash(FTesseractPath);
End;

Function GetTesseractExe: String;
Begin
  Result := IncludeTrailingBackslash(FTesseractPath) + 'tesseract' + GetExeExt;
End;

Procedure SetTesseractPath(AValue: String);
Begin
  If DirectoryExists(AValue) Then
    FTesseractPath := AValue
  Else
    FTesseractPath := '';
End;

Function BuildOptionsString(AOEM: Integer; APSM: Integer; AWhitelist: String): String;
Begin
  Result := '';

  // For full explanation and further options see:
  //   https://github.com/tesseract-ocr/tesseract/blob/main/doc/tesseract.1.asc

  // OCR Engine Mode
  //   -1 = Don't use this setting
  //    0 = Original Tesseract only.
  //    1 = Neural nets LSTM only.
  //    2 = Tesseract + LSTM.
  //    3 = Default, based on what is available.

  If (AOEM >= 0) And (AOEM <= 3) Then
    Result += Format('--oem %d ', [AOEM]);

  // Page Segmentation Mode
  //   -1 = Don't use this setting
  //    0 = Orientation and script detection (OSD) only.
  //    1 = Automatic page segmentation with OSD.
  //    2 = Automatic page segmentation, but no OSD, or OCR. (not implemented)
  //    3 = Fully automatic page segmentation, but no OSD. (Default)
  //    4 = Assume a single column of text of variable sizes.
  //    5 = Assume a single uniform block of vertically aligned text.
  //    6 = Assume a single uniform block of text.
  //    7 = Treat the image as a single text line.
  //    8 = Treat the image as a single word.
  //    9 = Treat the image as a single word in a circle.
  //   10 = Treat the image as a single character.
  //   11 = Sparse text. Find as much text as possible in no particular order.
  //   12 = Sparse text with OSD.
  //   13 = Raw line. Treat the image as a single text line,
  //        bypassing hacks that are Tesseract-specific.

  If (APSM >= 0) And (APSM <= 13) Then
    Result += Format('--psm %d ', [APSM]);

  // Whitelist
  // Whitelist is set as a CONFIGVAR, so we use the -c parameter:
  // -c CONFIGVAR=VALUE
  //  Set value for parameter CONFIGVAR to VALUE. Multiple -c arguments are allowed.

  // For a full list of ConfigVar's, run
  // > tesseract --print-parameters

  If AWhiteList <> '' Then
    Result += Format('-c "tessedit_char_whitelist=%s" ', [AWhitelist]);

  Result := Trim(Result);
End;

Procedure InitializeTesseract;
Var
  sApplicationFolder: String;
Begin
  If FTesseractPath = '' Then
  Begin
    sApplicationFolder := IncludeTrailingBackslash(Application.Location);
    FTesseractPath := FindFolder([sApplicationFolder,
      sApplicationFolder + 'Apps', sApplicationFolder + '..',
      sApplicationFolder + '..\Apps'], 'Tesseract-OCR',
      Format('tesseract%s', [GetExeExt]));
  End;
End;

Function OCR(AFilename: String; AOptions: String): String;
Var
  sCommand: String;
  sTempDir, sOutputBase, sOutputFile: String;
  oStrings: TStringList;
Begin
  // AOptions are image specific

  // either read https://github.com/tesseract-ocr/tesseract/blob/main/doc/tesseract.1.asc
  // and supply your own options string
  // or use BuildOptionsString() - Uses only a simplified subset

  If TesseractAvailable Then
  Begin
    sTempDir := IncludeTrailingBackslash(SysUtils.GetTempDir(False)) +
      ChangeFileExt(ExtractFileName(Application.ExeName), '');

    ForceDirectories(sTempDir);

    // TODO: Investigate threadsafe locks
    sOutputFile := UniqueFilename(sTempDir, 'OCR', '.txt', True);

    // Huh.  Tesseract must be automatically adding the .txt...
    sOutputBase := ChangeFileExt(sOutputFile, '');

    sCommand := Format('"%s" "%s" "%s" %s', [GetTesseractExe, AFilename, sOutputBase, AOptions]);

    RunEx(sCommand, nil, True);

    If FileExists(sOutputFile) Then
    Begin
      oStrings := TStringList.Create;
      Try
        oStrings.LoadFromFile(sOutputFile);
        Result := oStrings.Text;
      Finally
        oStrings.Free;
      End;
      SysUtils.DeleteFile(sOutputFile);
    End
    Else
      Result := '';
  End;
End;

Initialization
  FTesseractPath := '';
End.
