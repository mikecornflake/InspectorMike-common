Unit TesseractSupport;

{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : TesseractSupport.pas
  Description
    CLI support for Tesseract OCR tool

  Source
    Copyright (c) 2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    2022-10-06: Creation & Uploaded to Github
    2025-11-29: Added support for Options
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

  { TTesseractSupport }

  TTesseractSupport = Class(TThirdParty)
  Public
    Procedure DefineDefaults; Override;
    Function BuildOptionsString(AOEM: Integer = 3; APSM: Integer = -1;
      AWhitelist: String = ''): String;

    Function OCR(AFilename: String; AOptions: String = ''): String;
  End;

Function Tesseract: TTesseractSupport;

Const
  THIRDPARTY_TESSERACT = 'Tesseract';

Implementation

Uses
  Forms, StringSupport, FileUtil, OSSupport, FileSupport;

Var
  FTesseract: TTesseractSupport;

Function Tesseract: TTesseractSupport;
Begin
  If Not Assigned(FTesseract) Then
    FTesseract := TTesseractSupport.Create;

  Result := FTesseract;
End;

{ TTesseractSupport }

Procedure TTesseractSupport.DefineDefaults;
Begin
  // This unit self registers
  FUsed := True;

  // CLI, we don't care if the exe is 32bit or 64bit
  FCPUSensitive := False;

  // Preparation for default Initialise
  FKeyFile := 'tesseract' + GetExeExt;
  FKeyFolder := 'Tesseract-OCR';

  // Metadata
  FName := THIRDPARTY_TESSERACT;
  FSummary := 'Tesseract OCR is the industry-standard free, open-source Optical Character Recognition engine. It leverages advanced LSTM neural networks to extract text from images with up to 99%+ accuracy across 100+ languages. Fully offline and secure, it is the foundation of global document analysis, text extraction, and tesseract ocr download';
  FProjectURL := 'https://tesseractocr.org/';
  FCodeURL := 'https://github.com/tesseract-ocr/tesseract';
End;

Function TTesseractSupport.BuildOptionsString(AOEM: Integer; APSM: Integer;
  AWhitelist: String): String;
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

Function TTesseractSupport.OCR(AFilename: String; AOptions: String): String;
Var
  sCommand: String;
  sTempDir, sOutputBase, sOutputFile: String;
  oStrings: TStringList;
Begin
  // AOptions are image specific

  // either read https://github.com/tesseract-ocr/tesseract/blob/main/doc/tesseract.1.asc
  // and supply your own options string
  // or use BuildOptionsString() - Uses only a simplified subset

  If FAvailable Then
  Begin
    sTempDir := IncludeTrailingBackslash(SysUtils.GetTempDir(False)) +
      ChangeFileExt(ExtractFileName(Application.ExeName), '');

    ForceDirectories(sTempDir);

    // TODO: Investigate threadsafe locks
    sOutputFile := UniqueFilename(sTempDir, 'OCR', '.txt', True);

    // Huh.  Tesseract must be automatically adding the .txt...
    sOutputBase := ChangeFileExt(sOutputFile, '');

    sCommand := Format('"%s" "%s" "%s" %s', [FullExe('tesseract'), AFilename,
      sOutputBase, AOptions]);

    RunAndCapture(sCommand, nil, True);

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
  FTesseract := nil;

Finalization;
  // Free'd by ThirdParties owner
  //FreeAndNil(FTesseract);

End.
