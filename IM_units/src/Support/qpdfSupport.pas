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
  Classes, SysUtils, fgl, PDF;

Function qpdfAvailable: Boolean;
Function qpdfExe: String;
Procedure SetqpdfExe(AValue: String);
Function Initializeqpdf: Boolean;

// Parses qpdf-normalised UTC dates.
// Expected format: D:YYYYMMDDHHNNSSZ
// Note: PDFDate is more complex when showing non-UTC dates
Function qpdfPDFDateToDateTime(Const APDFDate: String): TDateTime;
Function qpdfDateTimeToPDFDate(Const ADate: TDateTime): String;

// Escape double quotes, and wrap AValue in double quotes
Function qpdfQuoteParam(Const AValue: String): String;

// TODO This should be APDFAttachments.IndexOf()...
Function qpdfFindAttachment(APDFAttachments: TPDFAttachments;
  Const AFilename: String): TPDFAttachment;

// Produce the --add-attachment parameter using SourceFilename.
Function qpdfAddAttachmentParam(APDFAttachment: TPDFAttachment): String; Overload;

// Produce the --add-attachment parameter using an explicit source filename.
Function qpdfAddAttachmentParam(APDFAttachment: TPDFAttachment;
  Const ASourceFilename: String): String; Overload;

// Call --list-attachments, and parse the result
Function qpdfListAttachments(Const AFilename: String; APDFAttachments: TPDFAttachments): Boolean;

// call --show-attachment
Function qpdfExtractAttachment(Const APDFFilename: String; Const AAttachmentKey: String;
  Const AOutputFilename: String): Boolean;

// Generate the appropriate --add-attachment parameters list, call them
// Uses a temp file.  The only file that will exist after the operation
// is AFilename, with the attachments added
// No checking is done on whether the attachments already exist
Function qpdAddAttachments(Const AFilename: String; APDFAttachments: TPDFAttachments): Boolean;

// Replace the PDF's complete attachment collection with APDFAttachments.
Function qpdfWriteAttachments(Const AFilename: String; APDFAttachments: TPDFAttachments): Boolean;

// Read attachments from another PDF and extract their contents into
// AWorkingFolder. APDFAttachments becomes an editable import list.

// AWorkingFolder must continue to exist until the attachments have
// been written to the destination PDF.
Function qpdfLoadAttachmentsForImport(Const APDFFilename: String;
  Const AWorkingFolder: String; APDFAttachments: TPDFAttachments): Boolean;

Implementation

Uses
  Forms, DateUtils, FileSupport, OSSupport, FileUtil, StringSupport;

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

Function qpdfPDFDateToDateTime(Const APDFDate: String): TDateTime;
Var
  iYear, iMonth, iDay: Integer;
  iHour, iMinute, iSecond: Integer;
Begin
  Result := 0;

  // Expected format: D:20260707065535Z
  If Length(APDFDate) <> 17 Then
    Exit;

  If (Copy(APDFDate, 1, 2) <> 'D:') Or (APDFDate[17] <> 'Z') Then
    Exit;

  If Not TryStrToInt(Copy(APDFDate, 3, 4), iYear) Or Not
    TryStrToInt(Copy(APDFDate, 7, 2), iMonth) Or Not TryStrToInt(Copy(APDFDate, 9, 2), iDay) Or
    Not TryStrToInt(Copy(APDFDate, 11, 2), iHour) Or Not
    TryStrToInt(Copy(APDFDate, 13, 2), iMinute) Or Not
    TryStrToInt(Copy(APDFDate, 15, 2), iSecond) Then
    Exit;

  TryEncodeDateTime(iYear, iMonth, iDay, iHour, iMinute, iSecond, 0, Result);
End;

Function qpdfDateTimeToPDFDate(Const ADate: TDateTime): String;
Begin
  If ADate = 0 Then
    Exit('');

  Result := 'D:' + FormatDateTime('yyyymmddhhnnss', ADate) + 'Z';
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

  Assert(Assigned(APDFAttachments),
    'Must pass a TPDFAttachments');

  Assert(APDFAttachments.FreeObjects,
    'APDFAttachments must be set to FreeObjects');

  If Not Initializeqpdf Then
    Exit;

  slOutput := TStringList.Create;
  Try
    sCommand := Format('"%s" --list-attachments --verbose "%s"', [FqpdfExe, AFilename]);

    slOutput.Text := RunAndCapture(sCommand, nil, True);

    oAttach := nil;

    For sLine In slOutput Do
    Begin
      If sLine = '' Then
        Continue;

      If (sLine[1] <> ' ') And sLine.Contains(' ->') Then
      Begin
        // Start of a new attachment is not indented.
        oAttach := TPDFAttachment.Create;
        oAttach.Filename := TextBetween(sLine, '', ' ->');

        APDFAttachments.Add(oAttach);
      End
      Else If Not Assigned(oAttach) Then
        Continue
      Else If (oAttach.Description = '') And BeginsWith(sLine, '  description: ') Then
      Begin
        oAttach.Description :=
          TextBetween(sLine, '  description: ', '');
      End
      Else If (oAttach.CreationDate = 0) And BeginsWith(sLine, '      creation date: ') Then
      Begin
        oAttach.CreationDate := qpdfPDFDateToDateTime(
          TextBetween(sLine, '      creation date: ', ''));
      End
      Else If (oAttach.ModificationDate = 0) And BeginsWith(sLine,
        '      modification date: ') Then
      Begin
        oAttach.ModificationDate :=
          qpdfPDFDateToDateTime(TextBetween(sLine, '      modification date: ', ''));
      End;
    End;

    Result := True;
  Finally
    slOutput.Free;
  End;
End;

Function qpdfQuoteParam(Const AValue: String): String;
Begin
  Result := StringReplace(AValue, '"', '\"', [rfReplaceAll]);
  If (Result <> '') And (Result[1] <> '"') Then
    Result := '"' + Result + '"';
End;

Function qpdfAddAttachmentParam(APDFAttachment: TPDFAttachment;
  Const ASourceFilename: String): String;
Begin
  Assert(Assigned(APDFAttachment),
    'Must pass a TPDFAttachment');

  Assert(ASourceFilename <> '',
    'Attachment source filename must not be empty');

  Result := ' --add-attachment ' + qpdfQuoteParam(ASourceFilename);

  // Explicitly provide these because an extracted attachment may have
  // a generated temporary source filename.
  Result += ' --key=' + qpdfQuoteParam(APDFAttachment.Filename);

  Result += ' --filename=' + qpdfQuoteParam(APDFAttachment.Filename);

  If APDFAttachment.Description <> '' Then
    Result += ' --description=' + qpdfQuoteParam(APDFAttachment.Description);

  If APDFAttachment.CreationDate <> 0 Then
    Result += ' --creationdate=' + qpdfQuoteParam(qpdfDateTimeToPDFDate(
      APDFAttachment.CreationDate));

  If APDFAttachment.ModificationDate <> 0 Then
    Result += ' --moddate=' + qpdfQuoteParam(qpdfDateTimeToPDFDate(
      APDFAttachment.ModificationDate));

  // Terminates this --add-attachment option group.
  Result += ' --';
End;

// TODO Make this a TPDFAttachments Function, with
// case sensivity dependant on OS
Function qpdfFindAttachment(APDFAttachments: TPDFAttachments;
  Const AFilename: String): TPDFAttachment;
Var
  oAttachment: TPDFAttachment;
Begin
  Result := nil;

  For oAttachment In APDFAttachments Do
    If SameText(oAttachment.Filename, AFilename) Then
      Exit(oAttachment);
End;

Function qpdfAddAttachmentParam(APDFAttachment: TPDFAttachment): String;
Begin
  Assert(Assigned(APDFAttachment),
    'Must pass a TPDFAttachment');

  Result := qpdfAddAttachmentParam(APDFAttachment, APDFAttachment.SourceFilename);
End;

Function qpdfExtractAttachment(Const APDFFilename: String; Const AAttachmentKey: String;
  Const AOutputFilename: String): Boolean;
Var
  sCommand: String;
Begin
  sCommand := qpdfQuoteParam(FqpdfExe) + ' ' + qpdfQuoteParam(APDFFilename) +
    ' ' + '--show-attachment=' + qpdfQuoteParam(AAttachmentKey);

  Result := RunAndCaptureToFile(sCommand, AOutputFilename);
End;

Function qpdAddAttachments(Const AFilename: String; APDFAttachments: TPDFAttachments): Boolean;
Var
  sFolder, sExt, sOriginal, sParams, sCommand, sTemp: String;
  oTemp: TPDFAttachment;
Begin
  Result := False;

  If Not FileExists(AFilename) Then
    Exit;

  Assert(APDFAttachments <> nil, 'Must pass a TPDFAttachments');
  Assert(Not APDFAttachments.FreeObjects, 'APDFAttachments must be set to FreeObjects');

  If Not Initializeqpdf Then
    Exit;

  sFolder := ExtractFileDir(AFilename);
  sOriginal := ChangeFileExt(ExtractFileName(AFilename), '');
  sExt := ExtractFileExt(AFilename);
  sTemp := UniqueFilename(sFolder, sOriginal, sExt, False, 5);

  // TODO Compare Existing Attachments then update or append accordingly
  //oExistingAttachments := TPDFAttachments.Create;

  If APDFAttachments.Count > 0 Then
  Begin
    sParams := '';

    sCommand := Format('"%s" "%s"', [qpdfExe, AFilename]);

    For oTemp In APDFAttachments Do
      sParams += qpdfAddAttachmentParam(oTemp);

    If sParams = '' Then
      Exit;

    sParams := '--replace-input ' + sParams;

    RunAndCapture(sCommand + ' ' + sParams);
  End;
End;

// Takes a collection of attachments as input
// and forces the PDF file to only have these
Function qpdfWriteAttachments(Const AFilename: String; APDFAttachments: TPDFAttachments): Boolean;
Var
  oExistingAttachments: TPDFAttachments;
  oAttachment: TPDFAttachment;
  oExistingAttachment: TPDFAttachment;

  aSourceFilenames: Array Of String;

  sCommand: String;
  sTempFolder: String;
  sTempFilename, sTempRoot: String;

  i: Integer;
Begin
  Result := False;

  If Not FileExists(AFilename) Then
    Exit;

  Assert(Assigned(APDFAttachments), 'Must pass a TPDFAttachments');

  If Not Initializeqpdf Then
    Exit;

  oExistingAttachments := TPDFAttachments.Create(True);
  Try
    // Obtain the actual attachment set currently in the PDF.
    If Not qpdfListAttachments(AFilename, oExistingAttachments) Then
      Exit;

    sTempRoot := IncludeSlash(GetTempDir(False)) + 'qpdfSupport/';
    sTempFolder := sTempRoot + FormatDateTime('yyyymmdd-hhnnss-zzz', Now);

    If Not ForceDirectories(sTempFolder) Then
      Exit;

    Try
      SetLength(aSourceFilenames, APDFAttachments.Count);

      // Resolve a physical source file for every attachment in the
      // desired final collection.
      For i := 0 To APDFAttachments.Count - 1 Do
      Begin
        oAttachment := APDFAttachments[i];

        If oAttachment.Filename = '' Then
          Exit;

        If oAttachment.SourceFilename <> '' Then
        Begin
          // New attachment, including a replacement file with the
          // same embedded filename as an existing attachment.
          If Not FileExists(oAttachment.SourceFilename) Then
            Exit;

          aSourceFilenames[i] :=
            oAttachment.SourceFilename;
        End
        Else
        Begin
          // Existing attachment. Extract its binary content before
          // the original attachment collection is replaced.
          oExistingAttachment := qpdfFindAttachment(oExistingAttachments, oAttachment.Filename);

          If Not Assigned(oExistingAttachment) Then
            Exit;

          sTempFilename := IncludeSlash(sTempFolder) + oExistingAttachment.Filename;

          If Not qpdfExtractAttachment(AFilename, oExistingAttachment.Filename,
            sTempFilename) Then
            Exit;

          aSourceFilenames[i] := sTempFilename;
        End;
      End;

      sCommand := qpdfQuoteParam(FqpdfExe) + ' ' + qpdfQuoteParam(AFilename);

      // Remove every attachment currently in the PDF.
      For oAttachment In oExistingAttachments Do
        sCommand += ' --remove-attachment=' + qpdfQuoteParam(oAttachment.Filename);

      // Add the complete desired attachment collection.
      For i := 0 To APDFAttachments.Count - 1 Do
        sCommand += qpdfAddAttachmentParam(APDFAttachments[i], aSourceFilenames[i]);

      // qpdf performs the rewrite through its own temporary output.
      sCommand += ' --replace-input';

      RunAndCapture(sCommand);
    Finally
      DeleteDirectory(sTempRoot, False);
      Result := True;
    End;
  Finally
    oExistingAttachments.Free;
  End;
End;

Function qpdfLoadAttachmentsForImport(Const APDFFilename: String;
  Const AWorkingFolder: String; APDFAttachments: TPDFAttachments): Boolean;
Var
  oAttachment: TPDFAttachment;
  sExtractFilename: String;
  sSafeFilename: String;
  i: Integer;
Begin
  Result := False;

  If Not FileExists(APDFFilename) Then
    Exit;

  Assert(Assigned(APDFAttachments), 'Must pass a TPDFAttachments');

  Assert(APDFAttachments.FreeObjects, 'APDFAttachments must own its objects');

  Assert(APDFAttachments.Count = 0, 'APDFAttachments must be empty');

  If Not ForceDirectories(AWorkingFolder) Then
    Exit;

  If Not qpdfListAttachments(APDFFilename, APDFAttachments) Then
    Exit;

  Try
    For i := 0 To APDFAttachments.Count - 1 Do
    Begin
      oAttachment := APDFAttachments[i];

      // The numeric prefix avoids any possible filesystem collision.
      // ExtractFileName also prevents an embedded path being used.
      sSafeFilename := Format('%.4d_%s',
        [i, ExtractFileName(oAttachment.Filename)]);

      If ExtractFileName(oAttachment.Filename) = '' Then
        sSafeFilename := Format('%.4d_attachment.bin', [i]);

      sExtractFilename := IncludeSlash(AWorkingFolder) +
        sSafeFilename;

      If Not qpdfExtractAttachment(APDFFilename,
        oAttachment.Filename, sExtractFilename
        ) Then
        Exit;

      // This identifies the physical file that qpdfWriteAttachments
      // will use when the destination PDF is saved.
      oAttachment.SourceFilename := sExtractFilename;
    End;

    Result := True;
  Finally
    If Not Result Then
    Begin
      For oAttachment In APDFAttachments Do
        If oAttachment.SourceFilename <> '' Then
          DeleteFile(oAttachment.SourceFilename);

      APDFAttachments.Clear;
    End;
  End;
End;

End.
