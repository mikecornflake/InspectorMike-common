Unit DBSupport;

{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : DBSupport.pas
  Description
    Helper routines for SQL DB Components, and is Zeos/SQLDB/sqlite agnostic

    Signficant parts of this unit were developed with the help of Lazarus Forums
    This unit is dedicated to @BigChimp.  RIP; I still miss you :-(

    I found using a TBufDataset unintuitive, so developed TMemTable to simplify use
    FileRenamer2 uses TMemTable...

  Source
    Copyright (c) 2025
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    ~2008: Creation date unknown, original local SVN repository lost
    2014-07-05: Uploaded to SourceForge/Package "Shared"
    2023-12-05: Last commit in SourceForge (after 34 commits)
    2024-01-22: Migrated to Github.  Refactored package to "IM_units"
    2025-11-29: Added this header

  License
    This file is part of IM_units.lpk.

    It is free software: you can redistribute it and/or modify it under the
    terms of the GNU General Public License as published by the Free Software
    Foundation, either version 3 of the License, or (at your option) any
    later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program.  If not, see <https://www.gnu.org/licenses/>.

    SPDX-License-Identifier: GPL-3.0-or-later
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
    Property Active: Boolean read GetActive write SetActive;

    Function HasField(AFieldName: String): Boolean;

    Property Table: TBufDataset read FTable;

    Property FieldByName[AFieldName: String]: TField read GetFieldByName; Default;
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

// SQL Helper Functions
Function DeleteSQL(ATable: String; FKeyFields: TStringList; ADataset: TDataset): String;
Function InsertSQL(ATable: String; ADataset: TDataset): String;
Function InsertSQLWhereNotExists(ATable: String; ADataset: TDataset;
  AOracle: Boolean = True): String;
Function ValidateSQL(AInput: String; AQuoted: Boolean = True; AOracle: Boolean = True): String;
Function FormatSQL(ASQL: String; Const AValues: Array Of Const): String;

Type
  TBoolType = (btYesNo, btTrueFalse);

Const
  TBoolYesNo: Array[False..True] Of String = ('N', 'Y');
  TBoolTrueFalse: Array[False..True] Of String = ('F', 'T');

Implementation

Uses
  StringSupport, OSSupport, Math, sqldb, typinfo, {$IFDEF ZEOS}ZAbstractRODataset, {$ENDIF}Forms;

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

Function DeleteSQL(ATable: String; FKeyFields: TStringList; ADataset: TDataset): String;
Var
  i: Integer;
  sWhere: String;
  sField, sValue: String;
Begin
  sWhere := '';

  For i := 0 To FKeyFields.Count - 1 Do
  Begin
    sField := FKeyFields.Names[i];
    sValue := Trim(DBSupport.Value(ADataset, sField));

    sWhere := sWhere + Format('%s=''%s''', [sField, sValue]);

    If i <> FKeyFields.Count - 1 Then
      sWhere := sWhere + ' AND ';
  End;

  Result := Format('DELETE FROM %s WHERE %s; ', [ATable, sWhere]);
End;

Function InsertSQL(ATable: String; ADataset: TDataset): String;
Var
  sNames: String;
  sValues: String;
  i: Integer;
  oField: TField;
Begin
  sNames := '';
  sValues := '';
  Result := '';

  If ADataset.Active Then
  Begin
    For i := 0 To ADataset.FieldCount - 1 Do
    Begin
      oField := ADataset.Fields[i];

      If oField.Visible Then
      Begin
        sNames := sNames + oField.FieldName;

        If oField.IsNull Then
          sValues := sValues + 'NULL'
        Else
          sValues := sValues + QuotedStr(oField.AsString);

        sNames := sNames + ', ';
        sValues := sValues + ', ';
      End;
    End;

    If Length(sNames) > 2 Then
    Begin
      sNames := Copy(sNames, 1, Length(sNames) - 2);
      sValues := Copy(sValues, 1, Length(sValues) - 2);
      Result := Format('INSERT INTO %s (%s) VALUES (%s); ', [ATable, sNames, sValues]);
    End;
  End;
End;

// DANEGEROUS - Requires the ADataset fields to be in exactly the correct order
Function InsertSQLWhereNotExists(ATable: String; ADataset: TDataset; AOracle: Boolean): String;
Var
  sWhere: String;
  sValues: String;
  i: Integer;
  oField: TField;
Begin
  sWhere := '';
  sValues := '';
  Result := '';

  If ADataset.Active Then
  Begin
    For i := 0 To ADataset.FieldCount - 1 Do
    Begin
      oField := ADataset.Fields[i];

      If oField.Visible Then
      Begin

        If oField.IsNull Then
        Begin
          sWhere := sWhere + Format('%s IS NULL', [oField.FieldName]);
          sValues := sValues + 'NULL';
        End
        Else
        Begin
          sWhere := sWhere + Format('%s=%s', [oField.FieldName, QuotedStr(oField.AsString)]);
          sValues := sValues + QuotedStr(oField.AsString);
        End;

        sWhere := sWhere + ' AND ';
        sValues := sValues + ', ';
      End;
    End;

    If Length(sWhere) > 5 Then
    Begin
      sWhere := Copy(sWhere, 1, Length(sWhere) - 5);
      sValues := Copy(sValues, 1, Length(sValues) - 2);

      If AOracle Then
        Result := Format(
          'INSERT INTO %s SELECT %s FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM %s WHERE %s); ',
          [ATable, sValues, ATable, sWhere])
      Else
        Result := Format('INSERT INTO %s SELECT %s WHERE NOT EXISTS (SELECT 1 FROM %s WHERE %s); ',
          [ATable, sValues, ATable, sWhere]);
    End;
  End;
End;

Function ValidateSQL(AInput: String; AQuoted: Boolean = True; AOracle: Boolean = True): String;
Begin
  Result := TrimChars(AInput, [' ', #0, #13, #10]);

  // Don't quote known functions...
  If (Pos('TO_', UpperCase(AInput)) = 0) And (Pos('SEQ_', UpperCase(AInput)) = 0) Then
  Begin
    If (AQuoted) And (Uppercase(Result) <> 'NULL') Then
      Result := QuotedStr(Result);

    If AOracle Then
      Result := FindReplace(Result, #10, '''||CHR(10)||''');

    Result := FindReplace(Result, #13, '');
  End;
End;

Function FormatSQL(ASQL: String; Const AValues: Array Of Const): String;
Var
  arrStrings: Array Of String;
  arrArgs: Array Of TVarRec;
  i: Integer;
Begin
  // Need arrStrings.  vAnsistring is a pointer, so the processing
  // being performed doesn't increase RefCount.  Storing the result
  // in the temp array of strings ensures the result doesn't get cleared
  // until after we leave scope
  SetLength(arrStrings{%H-}, Length(AValues));
  SetLength(arrArgs{%H-}, Length(AValues));

  For i := Low(AValues) To High(AValues) Do
  Begin
    arrArgs[i] := AValues[i];

    If (AValues[i].vType = vtAnsiString) Then
    Begin
      arrStrings[i] := ValidateSQL(Ansistring(AValues[i].vAnsiString), True, True);
      arrArgs[i].vAnsiString := Pointer(arrStrings[i]);
      arrArgs[i].vType := vtAnsiString;
    End
    Else
    If (AValues[i].vType = vtChar) Then
    Begin
      // Chars will need to be converted to strings as the result will be quoted
      arrStrings[i] := ValidateSQL(AValues[i].vChar, True, True);
      arrArgs[i].vAnsiString := Pointer(arrStrings[i]);
      arrArgs[i].vType := vtAnsiString;
    End;
  End;

  Result := Format(ASQL, arrArgs);
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
  SetStrProp(DataSet, 'sIndexName', sIndexName);
End;

Procedure ExportDatasetToCSV(ADataset: TDataset; Const AFileName: String);
Var
  oFile: TFileStream;
  oField: TField;
  oRow: TStringList;
  oBookmark: TBookmark;
  sRow: String;
Begin
  // Create a TStringList to store values for each row
  oRow := TStringList.Create;

  SetBusy;
  // Create a TFileStream to write to the CSV file
  oFile := TFileStream.Create(AFilename, fmCreate);
  Try
    // Write header line with oField names
    For oField In ADataset.Fields Do
      oRow.Add(Format('%s', [oField.FieldName]));

    sRow := oRow.CommaText + LineEnding;
    oFile.WriteBuffer(Pointer(sRow)^, Length(sRow));

    oBookmark := ADataset.Bookmark;
    ADataset.DisableControls;
    Try
      // Loop through the ADataset and write each row to the CSV file
      ADataset.First;
      While Not ADataset.EOF Do
      Begin
        oRow.Clear;
        For oField In ADataset.Fields Do
          oRow.Add(Format('%s', [oField.AsString]));

        sRow := oRow.CommaText + LineEnding;
        oFile.WriteBuffer(Pointer(sRow)^, Length(sRow));

        ADataset.Next;
      End;
    Finally
      ADataset.GotoBookmark(oBookmark);
      ADataset.FreeBookmark(oBookmark);
      ADataset.EnableControls;
    End;

  Finally
    // Free resources
    ClearBusy;
    oRow.Free;
    oFile.Free;
  End;
End;

End.
