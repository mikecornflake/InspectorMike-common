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
    2026-07-24: Migrated to Third Party

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
  Classes, SysUtils, fgl, fpjson, ComCtrls,
  PDF, ThirdPartySupport;

Type
  { TqpdfSupport }

  TqpdfSupport = Class(TThirdParty)
  Private
    FqpdfExe: String;

  Public
    Procedure DefineDefaults; Override;
    Procedure Initialise; Override;

    // Parses qpdf-normalised UTC dates.
    // Expected format: D:YYYYMMDDHHNNSSZ
    // Note: PDFDate is more complex when showing non-UTC dates
    Function PDFDateToDateTime(Const APDFDate: String): TDateTime;
    Function DateTimeToPDFDate(Const ADate: TDateTime): String;

    // Escape double quotes, and wrap AValue in double quotes
    // TODO: StringSupport...
    Function QuoteParam(Const AValue: String): String;

    // TODO This should be APDFAttachments.IndexOf()...
    Function FindAttachment(APDFAttachments: TPDFAttachments;
      Const AFilename: String): TPDFAttachment;

    // Produce the --add-attachment parameter using SourceFilename.
    Function AddAttachmentParam(APDFAttachment: TPDFAttachment): String; Overload;

    // Produce the --add-attachment parameter using an explicit source filename.
    Function AddAttachmentParam(APDFAttachment: TPDFAttachment;
      Const ASourceFilename: String): String; Overload;

    // Call --list-attachments, and parse the result
    Function ListAttachments(Const AFilename: String; APDFAttachments: TPDFAttachments): Boolean;

    // call --show-attachment
    Function ExtractAttachment(Const APDFFilename: String; Const AAttachmentKey: String;
      Const AOutputFilename: String): Boolean;

    // Generate the appropriate --add-attachment parameters list, call them
    // Uses a temp file.  The only file that will exist after the operation
    // is AFilename, with the attachments added
    // No checking is done on whether the attachments already exist
    Function AddAttachments(Const AFilename: String; APDFAttachments: TPDFAttachments): Boolean;

    // Replace the PDF's complete attachment collection with APDFAttachments.
    Function WriteAttachments(Const AFilename: String; APDFAttachments: TPDFAttachments): Boolean;

    // Read attachments from another PDF and extract their contents into
    // AWorkingFolder. APDFAttachments becomes an editable import list.

    // AWorkingFolder must continue to exist until the attachments have
    // been written to the destination PDF.
    Function LoadAttachmentsForImport(Const APDFFilename: String;
      Const AWorkingFolder: String; APDFAttachments: TPDFAttachments): Boolean;

    Function PopulateTOC(Const APDFFilename: String; ATreeView: TTreeView): Boolean;
    Function PageCount(Const APDFFilename: String): Integer;
  End;

Function QPDF: TqpdfSupport;


Implementation

Uses
  Forms, DateUtils, FileSupport, OSSupport, FileUtil, StringSupport,
  jsonparser;

Var
  FQPDF: TqpdfSupport;

Function QPDF: TqpdfSupport;
Begin
  If Not Assigned(FQPDF) Then
    FQPDF := TQPDFSupport.Create;

  Result := FQPDF;
End;

{ TqpdfSupport }

Procedure TqpdfSupport.DefineDefaults;
Begin
  // CLI, we don't care if the exe is 32bit or 64bit
  FCPUSensitive := False;

  // Preparation for default Initialise
  // Have to search for the dll as Poppler shares same
  // names as XPDF
  FKeyFile := 'qpdf' + GetExeExt;
  FKeyFolder := 'qpdf\bin';

  // Metadata
  FName := 'QPDF';
  FSummary := 'qpdf is a command-line tool and C++ library that performs content-preserving transformations on PDF files. It supports linearization, encryption, and numerous other features. It can also be used for splitting and merging files, creating PDF files (but you have to supply all the content yourself), and inspecting files for study or analysis. qpdf does not render PDFs or perform text extraction, and it does not contain higher-level interfaces for working with page contents. It is a low-level tool for working with the structure of PDF files and can be a valuable tool for anyone who wants to do programmatic or command-line-based manipulation of PDF files.';
  FCodeURL := 'https://github.com/qpdf/qpdf';
End;

Procedure TqpdfSupport.Initialise;
Begin
  Inherited Initialise;

  FqpdfExe := '';

  If DirectoryExists(FFolder) Then
    FqpdfExe := IncludeSlash(FFolder) + 'qpdf' + GetExeExt;

  If Not FileExists(FqpdfExe) Then
    FqpdfExe := '';
End;

Function TqpdfSupport.PDFDateToDateTime(Const APDFDate: String): TDateTime;
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

Function TqpdfSupport.DateTimeToPDFDate(Const ADate: TDateTime): String;
Begin
  If ADate = 0 Then
    Exit('');

  Result := 'D:' + FormatDateTime('yyyymmddhhnnss', ADate) + 'Z';
End;

Function TqpdfSupport.ListAttachments(Const AFilename: String;
  APDFAttachments: TPDFAttachments): Boolean;
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
        oAttach.CreationDate := PDFDateToDateTime(
          TextBetween(sLine, '      creation date: ', ''));
      End
      Else If (oAttach.ModificationDate = 0) And BeginsWith(sLine,
        '      modification date: ') Then
      Begin
        oAttach.ModificationDate :=
          PDFDateToDateTime(TextBetween(sLine, '      modification date: ', ''));
      End;
    End;

    Result := True;
  Finally
    slOutput.Free;
  End;
End;

Function TqpdfSupport.QuoteParam(Const AValue: String): String;
Begin
  Result := StringReplace(AValue, '"', '\"', [rfReplaceAll]);
  If (Result <> '') And (Result[1] <> '"') Then
    Result := '"' + Result + '"';
End;

Function TqpdfSupport.AddAttachmentParam(APDFAttachment: TPDFAttachment;
  Const ASourceFilename: String): String;
Begin
  Assert(Assigned(APDFAttachment),
    'Must pass a TPDFAttachment');

  Assert(ASourceFilename <> '',
    'Attachment source filename must not be empty');

  Result := ' --add-attachment ' + QuoteParam(ASourceFilename);

  // Explicitly provide these because an extracted attachment may have
  // a generated temporary source filename.
  Result += ' --key=' + QuoteParam(APDFAttachment.Filename);

  Result += ' --filename=' + QuoteParam(APDFAttachment.Filename);

  If APDFAttachment.Description <> '' Then
    Result += ' --description=' + QuoteParam(APDFAttachment.Description);

  If APDFAttachment.CreationDate <> 0 Then
    Result += ' --creationdate=' + QuoteParam(DateTimeToPDFDate(
      APDFAttachment.CreationDate));

  If APDFAttachment.ModificationDate <> 0 Then
    Result += ' --moddate=' + QuoteParam(DateTimeToPDFDate(APDFAttachment.ModificationDate));

  // Terminates this --add-attachment option group.
  Result += ' --';
End;

// TODO Make this a TPDFAttachments Function, with
// case sensivity dependant on OS
Function TqpdfSupport.FindAttachment(APDFAttachments: TPDFAttachments;
  Const AFilename: String): TPDFAttachment;
Var
  oAttachment: TPDFAttachment;
Begin
  Result := nil;

  For oAttachment In APDFAttachments Do
    If SameText(oAttachment.Filename, AFilename) Then
      Exit(oAttachment);
End;

Function TqpdfSupport.AddAttachmentParam(APDFAttachment: TPDFAttachment): String;
Begin
  Assert(Assigned(APDFAttachment),
    'Must pass a TPDFAttachment');

  Result := AddAttachmentParam(APDFAttachment, APDFAttachment.SourceFilename);
End;

Function TqpdfSupport.ExtractAttachment(Const APDFFilename: String;
  Const AAttachmentKey: String; Const AOutputFilename: String): Boolean;
Var
  sCommand: String;
Begin
  sCommand := QuoteParam(FqpdfExe) + ' ' + QuoteParam(APDFFilename) + ' ' +
    '--show-attachment=' + QuoteParam(AAttachmentKey);

  Result := RunAndCaptureToFile(sCommand, AOutputFilename);
End;

Function TqpdfSupport.AddAttachments(Const AFilename: String;
  APDFAttachments: TPDFAttachments): Boolean;
Var
  sFolder, sExt, sOriginal, sParams, sCommand, sTemp: String;
  oTemp: TPDFAttachment;
Begin
  Result := False;

  If Not FileExists(AFilename) Then
    Exit;

  Assert(APDFAttachments <> nil, 'Must pass a TPDFAttachments');
  Assert(Not APDFAttachments.FreeObjects, 'APDFAttachments must be set to FreeObjects');

  sFolder := ExtractFileDir(AFilename);
  sOriginal := ChangeFileExt(ExtractFileName(AFilename), '');
  sExt := ExtractFileExt(AFilename);
  sTemp := UniqueFilename(sFolder, sOriginal, sExt, False, 5);

  // TODO Compare Existing Attachments then update or append accordingly
  //oExistingAttachments := TPDFAttachments.Create;

  If APDFAttachments.Count > 0 Then
  Begin
    sParams := '';

    sCommand := Format('"%s" "%s"', [FqpdfExe, AFilename]);

    For oTemp In APDFAttachments Do
      sParams += AddAttachmentParam(oTemp);

    If sParams = '' Then
      Exit;

    sParams := '--replace-input ' + sParams;

    RunAndCapture(sCommand + ' ' + sParams);
  End;
End;

// Takes a collection of attachments as input
// and forces the PDF file to only have these
Function TqpdfSupport.WriteAttachments(Const AFilename: String;
  APDFAttachments: TPDFAttachments): Boolean;
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

  oExistingAttachments := TPDFAttachments.Create(True);
  Try
    // Obtain the actual attachment set currently in the PDF.
    If Not ListAttachments(AFilename, oExistingAttachments) Then
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
          oExistingAttachment := FindAttachment(oExistingAttachments, oAttachment.Filename);

          If Not Assigned(oExistingAttachment) Then
            Exit;

          sTempFilename := IncludeSlash(sTempFolder) + oExistingAttachment.Filename;

          If Not ExtractAttachment(AFilename, oExistingAttachment.Filename,
            sTempFilename) Then
            Exit;

          aSourceFilenames[i] := sTempFilename;
        End;
      End;

      sCommand := QuoteParam(FqpdfExe) + ' ' + QuoteParam(AFilename);

      // Remove every attachment currently in the PDF.
      For oAttachment In oExistingAttachments Do
        sCommand += ' --remove-attachment=' + QuoteParam(oAttachment.Filename);

      // Add the complete desired attachment collection.
      For i := 0 To APDFAttachments.Count - 1 Do
        sCommand += AddAttachmentParam(APDFAttachments[i], aSourceFilenames[i]);

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

Function TqpdfSupport.LoadAttachmentsForImport(Const APDFFilename: String;
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

  If Not ListAttachments(APDFFilename, APDFAttachments) Then
    Exit;

  Try
    For i := 0 To APDFAttachments.Count - 1 Do
    Begin
      oAttachment := APDFAttachments[i];

      // The numeric prefix avoids any possible filesystem collision.
      // ExtractFileName also prevents an embedded path being used.
      sSafeFilename := Format('%.4d_%s', [i, ExtractFileName(oAttachment.Filename)]);

      If ExtractFileName(oAttachment.Filename) = '' Then
        sSafeFilename := Format('%.4d_attachment.bin', [i]);

      sExtractFilename := IncludeSlash(AWorkingFolder) + sSafeFilename;

      If Not ExtractAttachment(APDFFilename, oAttachment.Filename, sExtractFilename) Then
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

Function TqpdfSupport.PopulateTOC(Const APDFFilename: String; ATreeView: TTreeView): Boolean;

  Procedure ParseNode(NodeJSON: TJSONData; Parent: TTreeNode);
  Var
    Title: String;
    Page, i: Integer;
    Kids: TJSONData;
    NewNode: TTreeNode;
  Begin
    Title := NodeJSON.FindPath('title').AsString;
    Page := NodeJSON.FindPath('destpageposfrom1').AsInteger;

    NewNode := ATreeView.Items.AddChild(Parent, Title);

    NewNode.Data := Pointer(PtrInt(Page));

    Kids := NodeJSON.FindPath('kids');
    If (Kids <> nil) And (Kids.JSONType = jtArray) Then
      For i := 0 To Kids.Count - 1 Do
        ParseNode(Kids.Items[i], NewNode);
  End;

Var
  sCommand: String;
  sJSON: String;
  RootJSON: TJSONData;
  OutlinesJSON: TJSONData;
  i: Integer;
Begin
  Result := False;

  If Not FileExists(APDFFilename) Then
    Exit;

  If Not Assigned(ATreeView) Then
    Exit;

  sCommand := Format('"%s" --json --json-key=outlines "%s"', [FqpdfExe, APDFFilename]);

  sJSON := RunAndCapture(sCommand);

  If Trim(sJSON) = '' Then
    Exit;

  RootJSON := nil;

  ATreeView.Items.BeginUpdate;
  Try
    ATreeView.Items.Clear;

    RootJSON := GetJSON(sJSON);

    OutlinesJSON := RootJSON.FindPath('outlines');

    If Not Assigned(OutlinesJSON) Then
      Exit;

    If OutlinesJSON.JSONType <> jtArray Then
      Exit;

    For i := 0 To OutlinesJSON.Count - 1 Do
      ParseNode(OutlinesJSON.Items[i], nil);

    Result := True;
  Finally
    RootJSON.Free;
    ATreeView.Items.EndUpdate;
  End;
End;

Function TqpdfSupport.PageCount(Const APDFFilename: String): Integer;
Var
  sOutput: String;
Begin
  Result := 0;

  If Not FileExists(APDFFilename) Then
    Exit;

  sOutput := Trim(RunAndCapture(Format('"%s" --show-npages "%s"', [FqpdfExe, APDFFilename])));

  If Not TryStrToInt(sOutput, Result) Then
    Result := 0;
End;

Initialization
  FQPDF := nil;

Finalization;
  FreeAndNil(FQPDF);

End.
