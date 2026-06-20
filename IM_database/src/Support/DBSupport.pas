Unit DBSupport;

{-------------------------------------------------------------------------------
  Package   : IM_database
  Unit      : DBSupport.pas
  Description
    Helper routines for SQL DB Components, and is Zeos/SQLDB/sqlite agnostic

    Signficant parts of this unit were developed with the help of Lazarus Forums
    This unit is dedicated to @BigChimp.  RIP; I still miss you :-(

    I found using a TBufDataset unintuitive, so developed TMemTable to simplify use
    FileRenamer2 uses TMemTable...

  Source
    Copyright (c) 2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    ~2008: Creation date unknown, original local SVN repository lost
    2014-07-05: Uploaded to SourceForge/Package "Shared"
    2023-12-05: Last commit in SourceForge (after 34 commits)
    2024-01-22: Migrated to Github.  Refactored package to "IM_units"
    2025-11-29: Added this header
    2026-06-19: Refactored into split InspectorMike package structure

  License
    This file is part of IM_database.lpk.

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
  Classes, Graphics, Clipbrd, LCLType, DB, DBGrids, SysUtils, Variants,
  Menus, BufDataset, StdCtrls;

Type

  { TMemTable }

  TMemTable = Class(TObject)
  Private
    FTable: TBufDataset;
    Procedure DoOnGetText(Sender: TField; Var aText: Ansistring; {%H-}DisplayText: Boolean);
    Function GetActive: Boolean;
    Function GetFieldByName(AFieldName: String): TField;
    Procedure SetActive(AValue: Boolean);
  Public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Procedure Close;
    Procedure AddField(AName: String; AType: TFieldType; ASize: Integer = -1);
    Procedure Open;

    Procedure ClearAllRecords;
    Function RecordCount: Integer;
    Property Active: Boolean Read GetActive Write SetActive;

    Function HasField(AFieldName: String): Boolean;

    Property Table: TBufDataset Read FTable;

    Property FieldByName[AFieldName: String]: TField Read GetFieldByName; Default;
  End;

// Database Routines
Function Value(oDataset: TDataset; sField: String; sDefault: String = ''): String;
Function ValueAsInteger(oDataset: TDataset; sField: String; iDefault: Integer = -1): Integer;
Function ValueAsFloat(oDataset: TDataset; sField: String; ADefault: Extended): Extended;

// DBGrid routines
Procedure InitialiseDBGrid(oGrid: TDBGrid; oDataset: TDataset; bHideIDs: Boolean = False);
Procedure ClearSortImage(oGrid: TDBGrid);

Function CountRecords(ADataset: TDataset): Integer;

// For In-memory Sorting (doesn't work with TSQLQuery)
Function SortBufDataSet(DataSet: TBufDataset; Const FieldName: String;
  Var AResultAscending: Boolean): Boolean;

// HTML routines
Procedure AppendDatasetAsHTML(oDest: TStringList; oDataset: TDataset;
  bOnlyActiveRow: Boolean = False; bIncludeHeader: Boolean = True);

// Tab Separated Text Routines
Procedure AppendDatasetAsTabSeparatedText(oDest: TStringList; oDataset: TDataset;
  bOnlyActiveRow: Boolean = False; bIncludeHeader: Boolean = True);

// Menu Routines
Procedure AddFieldNamesToMenu(oDataset: TDataset; oPopupMenu: TPopupMenu;
  TheOnClick: TNotifyEvent; iTag: Integer);
Procedure AddFieldNamesToSubMenu(oDataset: TDataset; AMenuItem: TMenuItem;
  TheOnClick: TNotifyEvent; iTag: Integer);

// Clipboard routines
Procedure PasteDatasetIntoClipboard(oDataset: TDataset; bOnlyActiveRow: Boolean = False;
  bIncludeHeader: Boolean = True);
Procedure PasteRowIntoClipboard(oDataset: TDataset; bIncludeHeader: Boolean = True);

// Populate Controls
Procedure Populate(ACombobox: TCombobox; ADataset: TDataset; AField: String;
  AInsertBlank: Boolean = False);

// Export routine
Procedure ExportDatasetToCSV(ADataset: TDataset; Const AFileName: String);

Type
  TBoolType = (btYesNo, btTrueFalse);

Const
  TBoolYesNo: Array[False..True] Of String = ('N', 'Y');
  TBoolTrueFalse: Array[False..True] Of String = ('F', 'T');

Implementation

Uses
  StringSupport, FileSupport, OSSupport, Math, sqldb, typinfo, {$IFDEF ZEOS}ZAbstractRODataset, {$ENDIF}Forms;

  { TMemTable }

Constructor TMemTable.Create;
Begin
  FTable := TBufDataset.Create(Application.MainForm);

  FTable.MaxIndexesCount := 10;
End;

Destructor TMemTable.Destroy;
Begin
  If Assigned(FTable) And (FTable.Active) Then
  Begin
    ClearAllRecords;
    Close;
  End;

  FreeAndNil(FTable);

  Inherited Destroy;
End;

Procedure TMemTable.DoOnGetText(Sender: TField; Var aText: Ansistring; DisplayText: Boolean);
Begin
  If Trim(Sender.AsString) = '' Then
    aText := ''
  Else
    aText := Format('%.4f', [Sender.AsFloat]);
End;

Function TMemTable.GetActive: Boolean;
Begin
  If Assigned(FTable) Then
    Result := FTable.Active
  Else
    Result := False;
End;

Function TMemTable.GetFieldByName(AFieldName: String): TField;
Begin
  Result := FTable.FieldByName(AFieldName);
End;

Function TMemTable.HasField(AFieldName: String): Boolean;
Var
  sField: String;
  iCount: Longint;
  i: Integer;
  oDef: TFieldDef;
Begin
  // TFields.FindField and TFieldDefs.Find both were always returning False :-(
  Result := False;
  sField := Lowercase(AFieldName);

  iCount := FTable.FieldDefs.Count;

  For i := 0 To iCount - 1 Do
  Begin
    oDef := FTable.FieldDefs[i];
    If Lowercase(oDef.Name) = sField Then
    Begin
      Result := True;
      Break;
    End;
  End;
End;

Procedure TMemTable.SetActive(AValue: Boolean);
Begin
  If Assigned(FTable) Then
    FTable.Active := AValue;
End;

Procedure TMemTable.Close;
Begin
  FTable.Filter := '';
  FTable.Filtered := False;
  FTable.OnFilterRecord := nil;

  If FTable.Active Then
    FTable.Close;

  FTable.FieldDefs.Clear;
End;

Procedure TMemTable.AddField(AName: String; AType: TFieldType; ASize: Integer);
Begin
  If ASize = -1 Then
    FTable.FieldDefs.Add(AName, AType)
  Else
    FTable.FieldDefs.Add(AName, AType, ASize);
End;

Procedure TMemTable.Open;
Var
  i: Integer;
  oField: TField;
Begin
  FTable.CreateDataset;

  // Floats are being returned to silly dp...
  For i := 0 To FTable.Fields.Count - 1 Do
  Begin
    oField := FTable.Fields[i];

    If oField.DataType In [ftWord, ftFloat, ftCurrency] Then
      oField.OnGetText := @DoOnGetText;
  End;
End;

Function TMemTable.RecordCount: Integer;
Begin
  Result := CountRecords(FTable);
End;

// care of WP @ https://forum.lazarus.freepascal.org/index.php?topic=36394.0
Procedure TMemTable.ClearAllRecords;
Begin
  FTable.DisableControls;
  FTable.Filter := '';
  FTable.Filtered := False;
  Try
    FTable.First;
    While Not FTable.EOF Do
      FTable.Delete;
  Finally
    FTable.EnableControls;
  End;
End;

Procedure Populate(ACombobox: TCombobox; ADataset: TDataset; AField: String;
  AInsertBlank: Boolean = False);
Begin
  ACombobox.Items.Clear;

  If AInsertBlank Then
    ACombobox.Items.Add('');

  ADataset.DisableControls;
  Try
    ADataset.First;

    While Not ADataset.EOF Do
    Begin
      ACombobox.Items.Add(Trim(DBSupport.Value(ADataset, AField)));

      ADataset.Next;
    End;

  Finally
    ADataset.EnableControls;
  End;
End;

Function Value(oDataset: TDataset; sField: String; sDefault: String = ''): String;
Var
  oField: TField;
Begin
  If sField = '' Then
    oField := oDataset.Fields[0]
  Else
    oField := oDataset.FindField(sField);

  If Assigned(oField) And Not VarIsNull(oField.Value) Then
    Result := oField.AsString
  Else
    Result := sDefault;
End;

Function ValueAsInteger(oDataset: TDataset; sField: String; iDefault: Integer): Integer;
Var
  oField: TField;
Begin
  If sField = '' Then
    oField := oDataset.Fields[0]
  Else
    oField := oDataset.FindField(sField);

  If Assigned(oField) And Not VarIsNull(oField.Value) Then
    Result := oField.AsInteger
  Else
    Result := iDefault;
End;

Function ValueAsFloat(oDataset: TDataset; sField: String; ADefault: Extended): Extended;
Begin
  Result := StrToFloatDef(Value(ODataset, sField, ''), ADefault);
End;

Procedure InitialiseDBGrid(oGrid: TDBGrid; oDataset: TDataset; bHideIDs: Boolean = False);
Var
  iTemp: Integer;
  oField: TField;
  oColumn: TColumn;
  iRow: Integer;
  iProcessCount: Integer;
  sCaption: String;
  bControlsDisabled: Boolean;
Begin
  If Not oDataset.Active Then
    Exit;

  bControlsDisabled := oDataset.ControlsDisabled;
  If Not bControlsDisabled Then
    oDataset.DisableControls;
  Try
    // Not the correct place, but certainly a convenient place
    If Assigned(oDataset) Then
      {$IFDEF ZEOS}
      If oDataset Is TZAbstractRODataset Then
        TZAbstractRODataset(oDataset).SortType := stIgnored
      Else
      {$ENDIF}
      If oDataset Is TSQLQuery Then
        TSQLQuery(oDataset).IndexName := '';

    oDataset.First;

    // First, get the basics right...
    oGrid.Canvas.Font.Style := [fsBold];
    For iTemp := oGrid.Columns.Count - 1 Downto 0 Do
    Begin
      oColumn := oGrid.Columns[iTemp];
      oField := oColumn.Field;
      sCaption := FindReplace(oField.FieldName, '_', ' ');

      oField.DisplayLabel := sCaption;
      oColumn.DisplayName := sCaption;

      oColumn.Width := oGrid.Canvas.TextWidth(sCaption) + 18;

      // Hide the ID columns if so requested
      If bHideIDs And (RightStr(oField.FieldName, 2) = 'ID') Then
      Begin
        oColumn.Visible := False;
        oField.Visible := False;
      End;

      // Ensure the sort arrows are not displayed by default
      oColumn.Title.ImageIndex := -1;
    End;

    // Next, plough through the visible dataset...
    oGrid.Canvas.Font.Style := [];
    oDataset.First;
    iRow := 0;
    iProcessCount := Trunc(oGrid.Height / oGrid.DefaultRowHeight);

    // This is all visible records
    // Change iProcessCount to oGrid.DataSource.DataSet.RecordCount for all records;

    While (Not oDataset.EOF) And (iRow < iProcessCount) Do
    Begin
      For iTemp := oGrid.Columns.Count - 1 Downto 0 Do
      Begin
        oColumn := oGrid.Columns[iTemp];
        oField := oColumn.Field;

        If (oColumn.Visible) And (oColumn.Width < 400) Then
          oColumn.Width := Min(400, Max(oColumn.Width,
            oGrid.Canvas.TextWidth(oField.AsString) + 12));
      End;

      oDataset.Next;
      Inc(iRow);
    End;

    oDataset.First;
  Finally
    If Not bControlsDisabled Then
      oDataset.EnableControls;
  End;
End;

Procedure ClearSortImage(oGrid: TDBGrid);
Var
  i: Integer;
Begin
  For i := 0 To oGrid.Columns.Count - 1 Do
    oGrid.Columns[i].Title.ImageIndex := -1;
End;

Procedure AppendDatasetAsTabSeparatedText(oDest: TStringList; oDataset: TDataset;
  bOnlyActiveRow: Boolean; bIncludeHeader: Boolean);

  Function Validate(sIn: String): String;
  Begin
    sIn := FindReplace(sIn, #13, '');
    sIn := FindReplace(sIn, #10, '');
    sIn := FindReplace(sIn, #09, '');

    Result := sIn;
  End;

  Procedure AddRow;
  Var
    iCol: Integer;
    oField: TField;
    sTemp: String;
  Begin
    sTemp := '';
    For iCol := 0 To oDataset.FieldCount - 1 Do
    Begin
      oField := oDataset.Fields[iCol];
      If oField.Visible Then
        sTemp := sTemp + Validate(oField.AsString) + #9;
    End;
    oDest.Add(sTemp);
  End;

Var
  iCol: Integer;
  oField: TField;
  oBookmark: TBookmark;
  sTemp: String;
Begin
  If oDataset.Active Then
  Begin
    oDataset.DisableControls;
    Try
      If bIncludeHeader Then
      Begin
        sTemp := '';

        For iCol := 0 To oDataset.FieldCount - 1 Do
        Begin
          oField := oDataset.Fields[iCol];
          If oField.Visible Then
            sTemp := sTemp + Validate(oField.DisplayName) + #9;
        End;

        oDest.Add(sTemp);
      End;

      If bOnlyActiveRow Then
        AddRow
      Else
      Begin
        // Add all rows
        oBookmark := oDataset.Bookmark;
        Try
          oDataset.First;

          While Not oDataset.EOF Do
          Begin
            AddRow;

            oDataset.Next;
          End;
        Finally
          oDataset.GotoBookmark(oBookmark);
          oDataset.FreeBookmark(oBookmark);
        End;
      End;
    Finally
      oDataset.EnableControls;
    End;
  End;
End;

Procedure AppendDatasetAsHTML(oDest: TStringList; oDataset: TDataset;
  bOnlyActiveRow: Boolean = False; bIncludeHeader: Boolean = True);

  Function Validate(Const sIn: String): String;
  Begin
    Result := FindReplace(sIn, '<', '&lt;');
    Result := FindReplace(Result, '>', '&gt;');
    //Result := FindReplace(Result, #13, '' + #13);
    Result := FindReplace(Result, #10, '<br style="mso-data-placement:same-cell;" />');
  End;

  Procedure AddRow;
  Var
    iCol: Integer;
    oField: TField;
  Begin
    oDest.Add('    <tr>');
    For iCol := 0 To oDataset.FieldCount - 1 Do
    Begin
      oField := oDataset.Fields[iCol];
      If oField.Visible Then
        If oField.DataType = ftString Then
          oDest.Add('      <td class="text">' + Validate(oField.AsString) + '</td>')
        Else If oField.DataType In [ftInteger, ftFloat, ftCurrency, ftSmallint, ftLargeint] Then
          oDest.Add('      <td class="number">' + Validate(oField.AsString) + '</td>')
        Else
          oDest.Add('      <td class="unknown">' + Validate(oField.AsString) + '</td>');
    End;
    oDest.Add('    </tr>');
  End;

Var
  iCol: Integer;
  oField: TField;
  oBookmark: TBookmark;
Begin
  If oDataset.Active Then
  Begin
    oDataset.DisableControls;
    Try
      oDest.Add('  <table class="StyleTable">');

      If bIncludeHeader Then
      Begin
        oDest.Add('    <tr valign="top">');
        For iCol := 0 To oDataset.FieldCount - 1 Do
        Begin
          oField := oDataset.Fields[iCol];
          If oField.Visible Then
            oDest.Add('      <th class="text">' + Validate(oField.DisplayName) + '</th>');
        End;
        oDest.Add('    </tr>');
      End;

      If bOnlyActiveRow Then
        AddRow
      Else
      Begin
        // Add all rows
        oBookmark := oDataset.Bookmark;
        Try
          oDataset.First;

          While Not oDataset.EOF Do
          Begin
            AddRow;

            oDataset.Next;
          End;
        Finally
          oDataset.GotoBookmark(oBookmark);
          oDataset.FreeBookmark(oBookmark);
        End;
      End;

      oDest.Add('  </table>');
    Finally
      oDataset.EnableControls;
    End;
  End;
End;

Procedure PasteDatasetIntoClipboard(oDataset: TDataset; bOnlyActiveRow: Boolean = False;
  bIncludeHeader: Boolean = True);
Var
  oTSV, oHTML: TStringList;
Begin
  If oDataset.Active Then
  Begin
    Clipboard.Clear;

    // Add a Tab Separated version to the clipboard for Notepad to use...
    oTSV := TStringList.Create;
    Try
      AppendDatasetAsTabSeparatedText(oTSV, oDataset, bOnlyActiveRow, bIncludeHeader);
      ClipBoard.AsText := oTSV.Text;
    Finally
      oTSV.Free;
    End;

    // Now append the HTML for Excel etc...
    oHTML := TStringList.Create;
    Try
      // Load the dataset into the string list
      AppendDatasetAsHTML(oHTML, oDataset, bOnlyActiveRow, bIncludeHeader);

      CopyHTMLToClipboard(oHTML);
    Finally
      oHTML.Free;
    End;
  End;
End;

Procedure PasteRowIntoClipboard(oDataset: TDataset; bIncludeHeader: Boolean);
Begin
  PasteDatasetIntoClipboard(oDataset, True, bIncludeHeader);
End;

Procedure AddFieldNamesToMenu(oDataset: TDataset; oPopupMenu: TPopupMenu;
  TheOnClick: TNotifyEvent; iTag: Integer);
Var
  i: Integer;
  oMenuItem: TMenuItem;
  oField: TField;
Begin
  If oDataset.Active Then
  Begin
    oMenuItem := NewLine;
    oMenuItem.Tag := iTag;
    oPopupMenu.Items.Add(oMenuItem);

    For i := 0 To oDataset.FieldCount - 1 Do
    Begin
      oField := oDataset.Fields[i];

      oMenuItem := TMenuItem.Create(oPopupMenu);
      oMenuItem.Caption := ':' + oField.FieldName;
      oMenuItem.Tag := iTag;
      oMenuItem.OnClick := TheOnClick;

      oPopupMenu.Items.Add(oMenuItem);
    End;
  End;
End;

Procedure AddFieldNamesToSubMenu(oDataset: TDataset; AMenuItem: TMenuItem;
  TheOnClick: TNotifyEvent; iTag: Integer);
Var
  i: Integer;
  oMenuItem: TMenuItem;
  oField: TField;
Begin
  If oDataset.Active Then
    For i := 0 To oDataset.FieldCount - 1 Do
    Begin
      oField := oDataset.Fields[i];

      oMenuItem := TMenuItem.Create(AMenuItem);
      oMenuItem.Caption := ':' + oField.FieldName;
      oMenuItem.Tag := iTag;
      oMenuItem.OnClick := TheOnClick;

      AMenuItem.Add(oMenuItem);
    End;
End;

Function CountRecords(ADataset: TDataset): Integer;
Var
  oBookmark: TBookMark;
  i: Integer;
Begin
  Result := -1;
  If ADataset.Active Then
    If Not ADataset.Filtered Then
      Result := ADataset.RecordCount
    Else
    Begin
      oBookmark := ADataset.Bookmark;

      ADataset.DisableControls;
      Try
        i := 0;
        ADataset.First;

        While Not ADataset.EOF Do
        Begin
          Inc(i);
          ADataset.Next;
        End;

        Result := i;
      Finally
        ADataset.GotoBookmark(oBookmark);
        ADataset.FreeBookmark(oBookmark);
        ADataset.EnableControls;
      End;
    End;
End;

// See http://wiki.lazarus.freepascal.org/How_to_write_in-memory_database_applications_in_Lazarus/FPC#Sorting_DBGrid_on_TitleClick_event_for_TBufDataSet
Function SortBufDataSet(DataSet: TBufDataset; Const FieldName: String;
  Var AResultAscending: Boolean): Boolean;
Var
  i: Integer;
  oField: TField;
  oIndexDefs: TIndexDefs;
  oIndexOptions: TIndexOptions;
  sIndexName: String;
Begin
  Result := False;
  oField := DataSet.Fields.FindField(FieldName);

  //If invalid ofield name, exit.
  If oField = nil Then
    Exit;

  //if invalid ofield type, exit.
  If {(oField is TObjectField) or} (oField Is TBlobField) Or
    {(oField is TAggregateField) or} (oField Is TVariantField) Or (oField Is TBinaryField) Then
    Exit;

  //Get oIndexDefs and sIndexName using RTTI
  If IsPublishedProp(DataSet, 'IndexDefs') Then
    oIndexDefs := GetObjectProp(DataSet, 'IndexDefs') As TIndexDefs
  Else
    Exit;

  If IsPublishedProp(DataSet, 'IndexName') Then
    sIndexName := GetStrProp(DataSet, 'IndexName')
  Else
    Exit;

  // Ensure oIndexDefs is up-to-date
  // This line is critical as oIndexDefs.Update will do nothing on the next sort if it's already true
  oIndexDefs.Updated := False;
  oIndexDefs.Update;

  //If an ascending index is already in use,
  //switch to a descending index
  If sIndexName = FieldName + '__IdxA' Then
  Begin
    sIndexName := FieldName + '__IdxD';
    oIndexOptions := [ixDescending];
  End
  Else
  Begin
    sIndexName := FieldName + '__IdxA';
    oIndexOptions := [];
  End;

  //Look for existing index
  For i := 0 To Pred(oIndexDefs.Count) Do
    If oIndexDefs[i].Name = sIndexName Then
    Begin
      Result := True;
      Break;
    End;

  //If existing index not found, create one
  If Not Result Then
  Begin
    If sIndexName = FieldName + '__IdxD' Then
      DataSet.AddIndex(sIndexName, FieldName, oIndexOptions, FieldName)
    Else
      DataSet.AddIndex(sIndexName, FieldName, oIndexOptions);
    Result := True;
  End;

  AResultAscending := Pos('__IdxA', sIndexName) > 0;

  //Set the index
  SetStrProp(DataSet, 'IndexName', sIndexName);
End;

// Should be RFC4180 compliant
// https://www.rfc-editor.org/info/rfc4180/
Procedure ExportDatasetToCSV(ADataset: TDataset; Const AFileName: String);
Var
  oFile: TFileStream;
  oField: TField;
  oBookmark: TBookmark;
  sRow: String;
  bFirst: Boolean;

  Function CSVCell(Const S: String): String;
  Begin
    Result := StringReplace(S, '"', '""', [rfReplaceAll]);

    If (Pos(',', S) > 0) Or (Pos('"', S) > 0) Or (Pos(#13, S) > 0) Or
      (Pos(#10, S) > 0) Then
      Result := '"' + Result + '"';
  End;

  Procedure AddCSVCell(Const S: String);
  Begin
    If Not bFirst Then
      sRow := sRow + ',';

    sRow := sRow + CSVCell(S);
    bFirst := False;
  End;

  Procedure WriteRow;
  Begin
    sRow := sRow + #13#10;
    WriteStringUTF8(oFile, sRow);
  End;

Begin
  SetBusy;
  Try
    oFile := TFileStream.Create(AFileName, fmCreate);
    Try
      oFile.WriteBuffer(UTF8BOM, SizeOf(UTF8BOM));

      sRow := '';
      bFirst := True;
      For oField In ADataset.Fields Do
        AddCSVCell(oField.FieldName);
      WriteRow;

      oBookmark := ADataset.Bookmark;
      ADataset.DisableControls;
      Try
        ADataset.First;
        While Not ADataset.EOF Do
        Begin
          sRow := '';
          bFirst := True;

          For oField In ADataset.Fields Do
            AddCSVCell(oField.AsString);

          WriteRow;
          ADataset.Next;
        End;
      Finally
        ADataset.GotoBookmark(oBookmark);
        ADataset.FreeBookmark(oBookmark);
        ADataset.EnableControls;
      End;
    Finally
      oFile.Free;
    End;
  Finally
    ClearBusy;
  End;
End;

End.
