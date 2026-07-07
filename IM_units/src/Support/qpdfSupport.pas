Unit qpdfSupport;

{$mode objfpc}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : qpdfSupport.pas
  Description
    CLI support for qpdf

  Source
    Copyright (c) 2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    2026-07-07: Created unit

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
  Classes, SysUtils, fgl;

Type

  { TPDFAttachment }

  TPDFAttachment = Class
    Key: Integer;
    Filename: String;
    Description: String;
    CreationDate: TDateTime;
    ModificationTime: TDateTime;

    Function DisplayText: String;
  End;

  TPDFAttachments = Class(Specialize TFPGObjectList<TPDFAttachment>);


Function qpdfAvailable: Boolean;
Function qpdfExe: String;
Procedure SetqpdfExe(AValue: String);
Function Initializeqpdf: Boolean;

Function qpdfListAttachments(Const AFilename: String; APDFAttachments: TPDFAttachments): Boolean;

Function PDFDateToDateTime(APDFDate: String): TDateTime;

Implementation

Uses
  Forms, FileSupport, OSSupport, FileUtil, StringSupport;

Var
  FqpdfExe: String;

Function qpdfAvailable: Boolean;
Begin
  Result := FileExists(FqpdfExe);
End;

Function qpdfExe: String;
Begin
  Result := FqpdfExe;
End;

Procedure SetqpdfExe(AValue: String);
Begin
  If FileExists(AValue) Then
    FqpdfExe := AValue
  Else
    FqpdfExe := '';
End;

Function Initializeqpdf: Boolean;
Var
  sFile, sTemp: String;
Begin
  If (FqpdfExe = '') Or Not FileExists(FqpdfExe) Then
  Begin
    sTemp := FindSupportFileInFolders('Apps', 'qpdf\bin', Format('qpdf%s', [GetExeExt]));

    If DirectoryExists(sTemp) Then
      FqpdfExe := IncludeSlash(sTemp) + Format('qpdf%s', [GetExeExt]);
  End;

  Result := FileExists(FqpdfExe);
End;

Function qpdfListAttachments(Const AFilename: String; APDFAttachments: TPDFAttachments): Boolean;
Var
  slOutput: TStringList;
  sLine, sCommand: String;
  oAttach: TPDFAttachment;
Begin
  Result := False;

  If Not FileExists(AFilename) Then
    Exit;

  Assert(APDFAttachments <> nil, 'Must pass a TPDFAttachments');
  Assert(Not APDFAttachments.FreeObjects, 'APDFAttachments must be set to FreeObjects');

  If Not Initializeqpdf Then
    Exit;

  slOutput := TStringList.Create;
  Try
    sCommand := Format('%s --list-attachments --verbose "%s"', [FqpdfExe, AFilename]);
    slOutput.Text := RunAndCapture(sCommand, nil, True);

    oAttach := nil;

    For sLine In slOutput Do
    Begin
      If sLine = '' Then
        Continue;

      If (sLine[1] <> ' ') And sLine.Contains(' ->') Then
      Begin
        // Start of a new attachment is not indented
        oAttach := TPDFAttachment.Create;
        oAttach.Filename := TextBetween(sLine, '', ' ->');
        oAttach.Key := StrToIntDef(Trim(TextBetween(sLine, ' ->', ',')), -1);
        APDFAttachments.Add(oAttach);
      End
      Else If (oAttach.Description = '') And BeginsWith(sLine, '  description: ') Then
        oAttach.Description := TextBetween(sLine, '  description: ', '')
      Else If (oAttach.CreationDate = 0) And sLine.Contains('      creation date: ') Then
        oAttach.CreationDate := PDFDateToDateTime(TextBetween(sLine, '      creation date: ', ''))
      Else If (oAttach.ModificationTime = 0) And sLine.Contains('      modification date: ') Then
        oAttach.ModificationTime :=
          PDFDateToDateTime(TextBetween(sLine, '      modification date: ', ''));
    End;

  Finally
    slOutput.Free;
  End;
End;

Function PDFDateToDateTime(APDFDate: String): TDateTime;
Var
  sTemp, sY, sM, sD, sH, sN, sS, sYMDHns: String;
Begin
  Result := 0;

  //D:20260707065535Z
  sTemp := TextBetween(APDFDate, 'D:', 'Z');
  If Length(sTemp) <> 14 Then
    Exit;

  sY := Copy(sTemp, 1, 4);
  sM := Copy(sTemp, 5, 2);
  sD := Copy(sTemp, 7, 2);
  sH := Copy(sTemp, 9, 2);
  sN := Copy(sTemp, 11, 2);
  sS := Copy(sTemp, 13, 2);

  sYMDHns := Format('%s/%s/%s %s:%s:%s', [sY, sM, sD, sH, sN, sS]);

  Result := YYYYmmddHHmmssToDateTime(sYMDHns);
End;

{ TPDFAttachment }

Function TPDFAttachment.DisplayText: String;
Begin
  Result := '';

  If Filename <> '' Then
    Result += Format('  Filename="%s" %s', [Filename, LineEnding]);

  If Key <> 0 Then
    Result += Format('    Key="%d" %s', [Key, LineEnding]);

  If Description <> '' Then
    Result += Format('    Description="%s" %s', [Description, LineEnding]);

  If CreationDate <> 0 Then
    Result += Format('    CreationDate="%s" %s',
      [FormatDateTime('yyyy-mm-dd HH:nn:ss', CreationDate), LineEnding]);

  If ModificationTime <> 0 Then
    Result += Format('    ModificationTime="%s" %s',
      [FormatDateTime('yyyy-mm-dd HH:nn:ss', ModificationTime), LineEnding]);
End;

End.
