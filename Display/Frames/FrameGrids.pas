Unit FrameGrids;

{$mode objfpc}

Interface

Uses
  Classes, ComCtrls, Controls, DB, DBCtrls, DBGrids, ExtCtrls, FileUtil, Forms,
  Dialogs, Graphics, FrameBase, Inifiles, LResources, Menus, StdCtrls, SysUtils, Grids, LCLType;

Type
  THackDBGrid = Class(TDBGrid)
  Public
    Property RowHeights;
  End;

  { TFrameGrid }

  TFrameGrid = Class(TfmeBase)
    btnClearFilter: TToolButton;
    btnShowFilterDlg: TToolButton;
    btnShowFilterList: TToolButton;
    cboFields: TComboBox;
    DBMemo: TDBMemo;
    dsGrid: TDatasource;
    grdSQL: TDBGrid;
    ilGrid: TImageList;
    ilScroll: TImageList;
    lblStatus: TLabel;
    dlgSave: TSaveDialog;
    mnuColumnEditor: TMenuItem;
    Separator2: TMenuItem;
    Separator1: TMenuItem;
    mnuExportToCSV: TMenuItem;
    mnuAllowMultiline: TMenuItem;
    mnuAllowMultilineDefaults: TMenuItem;
    mnuAllowMultilineYes: TMenuItem;
    mnuAllowMultilineNo: TMenuItem;
    N1: TMenuItem;
    mnuCopyCell: TMenuItem;
    mnuCopyRow: TMenuItem;
    mnuCopyTable: TMenuItem;
    pnlToolbar: TPanel;
    Panel2: TPanel;
    pmnuFilterMRU: TPopupMenu;
    pmnuGrid: TPopupMenu;
    pnlMemo: TPanel;
    splMemo: TSplitter;
    tbGrid: TToolBar;
    ToolButton1: TToolButton;
    Procedure btnClearFilterClick(Sender: TObject);
    Procedure btnShowFilterDlgClick(Sender: TObject);
    Procedure btnShowFilterListClick(Sender: TObject);
    Procedure cboFieldsChange(Sender: TObject);
    Procedure DBMemoKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure dsGridDataChange(Sender: TObject; Field: TField);
    Procedure dsGridStateChange(Sender: TObject);
    Procedure grdSQLDblClick(Sender: TObject);
    Procedure grdSQLKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure grdSQLMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure grdSQLMouseLeave(Sender: TObject);
    Procedure grdSQLMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure grdSQLMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    Procedure grdSQLPrepareCanvas(Sender: TObject; DataCol: Integer;
      Column: TColumn; AState: TGridDrawState);
    Procedure grdSQLStartDrag(Sender: TObject; Var DragObject: TDragObject);
    Procedure grdSQLTitleClick(Column: TColumn);
    Procedure mnuAllowMultilineClick(Sender: TObject);
    Procedure mnuColumnEditorClick(Sender: TObject);
    Procedure mnuCopyCellClick(Sender: TObject);
    Procedure mnuCopyRowClick(Sender: TObject);
    Procedure mnuCopyTableClick(Sender: TObject);
    Procedure mnuExportToCSVClick(Sender: TObject);
    Procedure mnuSetFilterClick(Sender: TObject);
    Procedure pmnuGridPopup(Sender: TObject);
  Private
    FDataChanging: Boolean;
    FDataset: TDataset;
    FEditable: Boolean;
    FFilterMRU: TStringList;
    FMemoField: String;
    FMouse: TPoint;
    FMouseCol, FMouseRow: Integer;
    FOnAfterFilter: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnGridMouseDown: TMouseEvent;
    FOnGridStartDrag: TNotifyEvent;
    FParentSemaphore: Boolean;
    FReadOnlyFields: TStringList;
    FScrolling: Boolean;
    FScrollPos: Integer;
    FSortField: String;
    FGridIsMultiline: Boolean;
    FAllowMultiline: Boolean;
    FUseMultilineDefaults: Boolean;

    Function GetEditable: Boolean;
    Function GetFilterMRU: String;
    Procedure RefreshParent;
    Procedure SetAllowMultiline(AValue: Boolean);
    Procedure SetGridToSingleLine;
    Procedure SetUseMultilineDefaults(AValue: Boolean);

    Procedure SetControlsEditable(AValue: Boolean);
    Procedure SetDataset(Const AValue: TDataset);
    Procedure SetEditable(AValue: Boolean);
    Procedure SetFilterMRU(Const AValue: String);

    Procedure BuildMRUMenu;
    Procedure ClearFilter;
    Procedure SetFilter(AFilter: String; AFilterOptions: TFilterOptions);

    Procedure SetFilterHint(sHint: String);
    Procedure SetOnGridStartDrag(AValue: TNotifyEvent);
    Procedure UpdateScrollbar;
    Procedure UpdateMemo;

    // To workaround an Oracle/Zeos issue....
    Procedure DatasetOnGetText(Sender: TField; Var aText: Ansistring; DisplayText: Boolean);
    Procedure DatasetAfterEdit(DataSet: TDataSet);
  Public
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure RefreshUI; Override;

    Procedure LoadSettings(oInifile: TIniFile); Override;
    Procedure SaveSettings(oInifile: TIniFile); Override;

    Property Dataset: TDataset read FDataset write SetDataset;
    Function RecordCount: Integer;
    Property FilterMRU: String read GetFilterMRU write SetFilterMRU;

    Function ReadOnlyField(AField: String): Boolean;
    Procedure ClearReadOnlyFields;
    Procedure AddReadOnlyField(AField: String);

    Procedure ClearSort;
    Procedure InitialiseDataset;
    Procedure InitialiseDBGrid(bHideIDs: Boolean = False);
    Property SortField: String read FSortField;

    Procedure ColumnEditor;

    Property CallingRefresh: Boolean read FParentSemaphore;

    Function Status(bIncRecount: Boolean = True): String;
    Procedure SetStatus(sValue: String);

    Property Editable: Boolean read GetEditable write SetEditable;

    Property UseMultilineDefaults: Boolean read FUseMultilineDefaults
      write SetUseMultilineDefaults;
    Property AllowMultiline: Boolean read FAllowMultiline write SetAllowMultiline;

    Property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    Property OnGridMouseDown: TMouseEvent read FOnGridMouseDown write FOnGridMouseDown;
    Property OnGridStartDrag: TNotifyEvent read FOnGridStartDrag write SetOnGridStartDrag;
    Property OnAfterFilter: TNotifyEvent read FOnAfterFilter write FOnAfterFilter;
  End;

Implementation

Uses
  DialogSQLFilter, DialogDBGridColEditor, DBSupport, StringSupport, OSSupport, FormMain,
  sqldb,
  BufDataset,
  Clipbrd,
  ZAbstractRODataset, Math; // For stAscending/stDescending

  {$R *.lfm}

  { TFrameGrid }

Constructor TFrameGrid.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  FFilterMRU := TStringList.Create;
  FFilterMRU.StrictDelimiter := True;
  FFilterMRU.Delimiter := ',';

  FParentSemaphore := False;
  FDataset := nil;

  FScrollPos := -1;
  FMemoField := '';

  FScrolling := False;

  FOnDblClick := nil;
  FOnGridMouseDown := nil;
  FOnAfterFilter := nil;

  FReadOnlyFields := TStringList.Create;
  FReadOnlyFields.Duplicates := dupIgnore;
  FReadOnlyFields.Sorted := True;

  FEditable := False;
  FDataChanging := False;

  FMouse := Point(-1, -1);

  FGridIsMultiline := False;
  FUseMultilineDefaults := True;
  FAllowMultiline := True;
End;

Destructor TFrameGrid.Destroy;
Begin
  FreeAndNil(FReadOnlyFields);

  FreeAndNil(FFilterMRU);

  Inherited Destroy;
End;

Procedure TFrameGrid.UpdateScrollbar;
Begin
  // This is here for where we were drawng an external vertical scroll bar to implement
  // thumbtracking accurately
  If Not assigned(FDataset) Then
    exit;
End;

Procedure TFrameGrid.UpdateMemo;
Var
  sCurrent: String;
  oField: TField;
  i: Integer;
Begin
  If Not assigned(FDataset) Then
    exit;

  If FDataset.Active Then
  Begin
    If cboFields.ItemIndex <> -1 Then
      sCurrent := cboFields.Text
    Else
      sCurrent := '';
    cboFields.Items.Clear;

    cboFields.Items.Add('');
    For i := 0 To FDataset.FieldCount - 1 Do
    Begin
      oField := FDataset.Fields[i];
      If oField.Visible Then
        cboFields.Items.Add(oField.FieldName);
    End;

    If (sCurrent = '') And (FMemoField <> '') Then
      i := cboFields.Items.IndexOf(FMemoField)
    Else
      i := cboFields.Items.IndexOf(sCurrent);

    If i <> -1 Then
    Begin
      cboFields.ItemIndex := i;
      DBMemo.DataField := cboFields.Text;
    End
    Else
      DBMemo.DataField := '';
  End;
End;

Procedure TFrameGrid.DatasetOnGetText(Sender: TField; Var aText: Ansistring;
  DisplayText: Boolean);
Begin
  aText := Sender.AsString;
End;

Procedure TFrameGrid.RefreshUI;
Var
  bDatasetAlive, bHasRecords: Boolean;
Begin
  Inherited RefreshUI;
  // Calling FParentDock.RefreshUI will cause recursion...
  // Use FParentSemaphore to wrap vulnerable code (See RefreshParent...)
  RefreshParent;

  bDatasetAlive := Assigned(FDataset);
  If bDatasetAlive Then
    bDatasetAlive := (FDataset.Active) And (Not FDataset.ControlsDisabled);

  bHasRecords := (RecordCount > 0);

  btnShowFilterDlg.Enabled := bDatasetAlive;
  btnShowFilterList.Enabled := bDatasetAlive And (FFilterMRU.Count > 0);
  btnClearFilter.Enabled := bDatasetAlive And FDataset.Filtered;

  grdSQL.ReadOnly := bDatasetAlive And bHasRecords;
  DBMemo.Enabled := bDatasetAlive And bHasRecords;
  cboFields.Enabled := bDatasetAlive And bHasRecords;
  mnuColumnEditor.Enabled := bDatasetAlive And bHasRecords;

  If (bDatasetAlive) And (bHasRecords) Then
    SetControlsEditable(FEditable)
  Else
    SetControlsEditable(False);

  lblStatus.Left := btnClearFilter.Left + btnClearFilter.Width;
  lblStatus.Width := tbGrid.Width - lblStatus.Left;
End;

Procedure TFrameGrid.LoadSettings(oInifile: TIniFile);
Begin
  Inherited LoadSettings(oInifile);

  FilterMRU := oInifile.ReadString(SettingsKey, FullIdentKey + '.Filter MRU', '');

  FMemoField := oInifile.ReadString(SettingsKey, FullIdentKey + '.Memo Field', '');
  FUseMultilineDefaults := oInifile.ReadBool(SettingsKey, FullIdentKey +
    '.Use Multiline Defaults', FUseMultilineDefaults);
  FAllowMultiline := oInifile.ReadBool(SettingsKey, FullIdentKey + '.Multiline', FAllowMultiline);

  pnlMemo.Height := oInifile.ReadInteger(SettingsKey, FullIdentKey + '.Memo Height',
    pnlMemo.Height);

  splMemo.Top := pnlMemo.Top - splMemo.Height;
End;

Procedure TFrameGrid.SaveSettings(oInifile: TIniFile);
Var
  sTemp: String;
Begin
  Inherited SaveSettings(oInifile);

  sTemp := Trim(FilterMRU);
  If sTemp <> '' Then
    oInifile.WriteString(SettingsKey, FullIdentKey + '.Filter MRU', sTemp);

  If FMemoField <> '' Then
    oInifile.WriteString(SettingsKey, FullIdentKey + '.Memo Field', FMemoField);

  oInifile.WriteInteger(SettingsKey, FullIdentKey + '.Memo Height', pnlMemo.Height);
  oInifile.WriteBool(SettingsKey, FullIdentKey + '.Multiline', FAllowMultiline);
  oInifile.WriteBool(SettingsKey, FullIdentKey + '.Use Multiline Defaults', FUseMultilineDefaults);
End;

Function TFrameGrid.RecordCount: Integer;
Begin
  // TODO: Cache this count, and only recalc when directed
  Result := -1;

  If Not Assigned(FDataset) Then
    Exit;

  If Not (FDataset.Active) Then
    Exit;

  If (FDataset Is TZAbstractRODataset) Then
    Result := FDataset.RecordCount
  Else If (FDataset Is TSQLQuery) Then
    Result := FDataset.RecordCount
  Else
    Result := DBSupport.CountRecords(FDataset);
End;

Function TFrameGrid.ReadOnlyField(AField: String): Boolean;
Begin
  Result := FReadOnlyFields.IndexOf(AField) >= 0;
End;

Procedure TFrameGrid.ClearReadOnlyFields;
Begin
  FReadOnlyFields.Clear;
End;

Procedure TFrameGrid.AddReadOnlyField(AField: String);
Begin
  FReadOnlyFields.Add(AField);
End;

Procedure TFrameGrid.InitialiseDataset;
Var
  i: Integer;
  oField: TField;
Begin
  If Not Assigned(FDataset) Then
    Exit;

  For i := 0 To FDataset.FieldCount - 1 Do
  Begin
    oField := FDataset.Fields[i];

    If oField.DataType In [ftSmallint, ftInteger, ftWord, ftFloat, ftCurrency] Then
      oField.OnGetText := @DatasetOnGetText;
  End;
End;

Procedure TFrameGrid.DatasetAfterEdit(DataSet: TDataSet);
Begin
  // TODO Add an Event Here that users of TFrameGrid can call
End;

Procedure TFrameGrid.InitialiseDBGrid(bHideIDs: Boolean);
Var
  i: Integer;
  oColumn: TColumn;
  bTemp: Boolean;
Begin
  DBSupport.InitialiseDBGrid(grdSQL, FDataset, bHideIDs);

  For i := 0 To grdSQL.Columns.Count - 1 Do
  Begin
    oColumn := grdSQL.Columns[i];
    bTemp := Assigned(oColumn.Field) And
      (FReadOnlyFields.IndexOf(oColumn.Field.FieldName) <> -1);
    oColumn.ReadOnly := bTemp;
  End;

  FDataset.AfterEdit := @DatasetAfterEdit;

  RefreshUI;
End;

Procedure TFrameGrid.ColumnEditor;
Var
  dlgGridColumns: TdlgGridColumns;
Begin
  dlgGridColumns := TdlgGridColumns.Create(Self);
  Try
    dlgGridColumns.Grid := grdSQL;

    If dlgGridColumns.ShowModal = mrOk Then
    Begin
      dlgGridColumns.Apply;
    End;
  Finally
    FreeAndNil(dlgGridColumns);
  End;
End;

Procedure TFrameGrid.ClearSort;
Begin
  If (FDataset = nil) Then
    Exit;

  ClearSortImage(grdSQL);

  // This code is sorting for FDataset=TZQuery  (ZeosLib)
  If (FDataset Is TZAbstractRODataset) Then
    TZAbstractRODataset(FDataset).SortedFields := ''
  Else If (FDataset Is TCustomBufDataset) Then
    TCustomBufDataset(FDataset).IndexName := '';

  FSortField := '';
End;

Procedure TFrameGrid.RefreshParent;
Begin
  // use the semaphore to block recursion

  If ParentForm <> nil Then
    If Not FParentSemaphore Then
    Begin
      FParentSemaphore := True;
      Try
        ParentForm.RefreshUI
      Finally
        FParentSemaphore := False;
      End;
    End;
End;

Procedure TFrameGrid.SetUseMultilineDefaults(AValue: Boolean);
Begin
  If FUseMultilineDefaults = AValue Then Exit;
  FUseMultilineDefaults := AValue;

  If AValue Then
    AllowMultiline := MainForm.Options.MultilineGridDefaults;

  grdSQL.Invalidate;
End;

Procedure TFrameGrid.SetAllowMultiline(AValue: Boolean);
Begin
  If FAllowMultiline = AValue Then Exit;

  FAllowMultiline := AValue;

  If Not FAllowMultiline Then
    SetGridToSingleLine;

  grdSQL.Invalidate;
End;


Function TFrameGrid.Status(bIncRecount: Boolean): String;
Begin
  Result := '';
  If Assigned(FDataset) And (FDataset.Active) Then
  Begin
    If bIncRecount Then
      Result := Result + Format('%d of %d records.  ', [FDataset.RecNo, RecordCount]);

    If FDataset.Filtered Then
      Result := Result + 'Grid filtered by ' + StringReplace(FDataset.Filter +
        '.', LineEnding, ' ', [rfReplaceAll])
    Else
      Result := Result + 'No grid filter applied.';
  End;
End;

Procedure TFrameGrid.SetStatus(sValue: String);
Begin
  lblStatus.Caption := '  ' + sValue;
End;

Function TFrameGrid.GetFilterMRU: String;
Begin
  Result := FFilterMRU.DelimitedText;
  Result := FindReplace(Result, #10, ' ');
  Result := FindReplace(Result, #13, '');
End;

Function TFrameGrid.GetEditable: Boolean;
Begin
  Result := FEditable;
End;

Procedure TFrameGrid.SetFilterMRU(Const AValue: String);
Begin
  FFilterMRU.DelimitedText := AValue;
End;

Procedure TFrameGrid.SetDataset(Const AValue: TDataset);
Begin
  FDataset := AValue;
  dsGrid.DataSet := FDataset;
End;

Procedure TFrameGrid.SetEditable(AValue: Boolean);
Begin
  FEditable := AValue;

  SetControlsEditable(AValue);
End;

Procedure TFrameGrid.SetControlsEditable(AValue: Boolean);
Begin
  dsGrid.AutoEdit := AValue;

  If AValue Then
    grdSQL.Options := grdSQL.Options + [dgEditing, dgMultiselect] - [dgRowSelect]
  Else
    grdSQL.Options := grdSQL.Options - [dgEditing] + [dgRowSelect];
End;

Procedure TFrameGrid.grdSQLTitleClick(Column: TColumn);
Var
  bAscending: Boolean;
  oSQLQuery: TCustomBufDataset;
  oZQuery: TZAbstractRODataset;
  sASC_IndexName, sDESC_IndexName: String;
  sTemp: String;
Begin
  // Defensive code
  If (Not Assigned(Column)) Or (Not Assigned(Column.Field)) Or
    (Not Assigned(Column.Field.DataSet)) Then
    Exit;

  If Not Column.Field.DataSet.Active Then
    Exit;

  If (Column.Field.DataSet Is TZAbstractRODataset) Then
  Begin
    // This code is sorting for TZAbstractRODataset (ZeosLib)
    oZQuery := TZAbstractRODataset(Column.Field.DataSet);

    If oZQuery.Active And (oZQuery.RecordCount > 0) Then
    Begin
      sTemp := '"' + Column.FieldName + '"';

      If (oZQuery.SortedFields = sTemp) Then
      Begin
        If oZQuery.SortType = stAscending Then
          oZQuery.SortType := stDescending
        Else
          oZQuery.SortType := stAscending;
      End
      Else
      Begin
        ClearSortImage(grdSQL);

        oZQuery.SortedFields := sTemp;
        oZQuery.SortType := stAscending;
      End;

      Column.Title.ImageIndex := Ord(oZQuery.SortType);
      FSortField := Column.FieldName;
    End;
  End
  Else If (Column.Field.DataSet Is TCustomBufDataset) Then
  Begin
    // This code is sorting for TSQLQuery (sqldb)
    bAscending := Column.Title.ImageIndex = 0;

    ClearSortImage(grdSQL);

    sASC_IndexName := 'ASC_' + Column.FieldName;
    sDESC_IndexName := 'DESC_' + Column.FieldName;

    //indexes cant sort binary types such as ftmemo,
    If (Column.Field.DataType In [ftMemo, ftWideMemo, ftBlob, ftVarBytes, ftBytes]) Then
      Exit;

    oSQLQuery := TCustomBufDataset(Column.Field.DataSet);

    //check if a Ascending index already exists for this column, if not create one
    If oSQLQuery.IndexDefs.IndexOf(sASC_IndexName) = -1 Then
      oSQLQuery.AddIndex(sASC_IndexName, column.FieldName, []);

    //check if a Descending index already exists for this column, if not create one
    If oSQLQuery.IndexDefs.IndexOf(sDESC_IndexName) = -1 Then
      oSQLQuery.AddIndex(sDESC_IndexName, column.FieldName, [ixDescending]);

    //ensure index defs are up to date
    oSQLQuery.IndexDefs.Updated := False;  {<<<----It won't work without this line}
    oSQLQuery.IndexDefs.Update;

    //use the column image index to toggle ASC/DESC
    If Not bAscending Then
    Begin
      Column.Title.ImageIndex := 0;
      oSQLQuery.IndexName := sASC_IndexName;
    End
    Else
    Begin
      Column.Title.ImageIndex := 1;
      oSQLQuery.IndexName := sDESC_IndexName;
    End;

    FSortField := Column.FieldName;
    // TODO - Let the outside world know that SortField has changed.
  End;

  RefreshUI;
End;

Procedure TFrameGrid.mnuAllowMultilineClick(Sender: TObject);
Begin
  If Sender = mnuAllowMultilineDefaults Then
    UseMultilineDefaults := True
  Else If Sender = mnuAllowMultilineYes Then
  Begin
    UseMultilineDefaults := False;
    AllowMultiline := True;
  End
  Else
  Begin
    UseMultilineDefaults := False;
    AllowMultiline := False;
  End;

  RefreshUI;
End;

Procedure TFrameGrid.mnuColumnEditorClick(Sender: TObject);
Begin
  ColumnEditor;
End;

Procedure TFrameGrid.mnuCopyCellClick(Sender: TObject);
Var
  oField: TField;
Begin
  If Not assigned(FDataset) Then
    Exit;

  If (dsGrid.Dataset.Active) Then
  Begin
    oField := grdSQL.SelectedField;

    If Assigned(oField) Then
      Clipboard.AsText := oField.AsString
    Else
      Clipboard.AsText := 'No valid cell selected';
  End;
End;

Procedure TFrameGrid.mnuCopyRowClick(Sender: TObject);
Begin
  PasteDatasetIntoClipboard(FDataset, True, True);
End;

Procedure TFrameGrid.mnuCopyTableClick(Sender: TObject);
Begin
  PasteDatasetIntoClipboard(FDataset, False, True);
End;

Procedure TFrameGrid.mnuExportToCSVClick(Sender: TObject);
Begin
  If dlgSave.Execute Then
  Begin
    ExportDatasetToCSV(FDataset, dlgSave.FileName);

    If dlgSave.FileName <> '' Then
      LaunchFile('explorer.exe', Format('/e,/select,"%s"', [dlgSave.FileName]));
  End;
End;

Procedure TFrameGrid.SetFilterHint(sHint: String);
Var
  sTemp: String;
Begin
  If Trim(sHint) = '' Then
    sTemp := 'No grid filter applied.'
  Else
    sTemp := Trim(sHint);

  btnShowFilterList.Hint := sTemp;
  btnShowFilterDlg.Hint := sTemp;
  btnClearFilter.Hint := sTemp;
End;

Procedure TFrameGrid.SetOnGridStartDrag(AValue: TNotifyEvent);
Begin
  If FOnGridStartDrag = AValue Then
    Exit;

  FOnGridStartDrag := AValue;
End;

Procedure TFrameGrid.pmnuGridPopup(Sender: TObject);
Begin
  mnuCopyTable.Enabled := (FMouseRow <> 0) And FDataset.Active;
  mnuCopyRow.Enabled := mnuCopyTable.Enabled And (RecordCount > 0);

  mnuAllowMultilineDefaults.Checked := FUseMultilineDefaults;
  mnuAllowMultilineYes.Checked := (Not FUseMultilineDefaults) And (FAllowMultiline);
  mnuAllowMultilineNo.Checked := (Not FUseMultilineDefaults) And (Not FAllowMultiline);
End;

Procedure TFrameGrid.btnShowFilterListClick(Sender: TObject);
Var
  oPos: TPoint;
Begin
  BuildMRUMenu;

  oPos := btnShowFilterList.ClientToScreen(Point(0, btnShowFilterList.Height + 1));
  pmnuFilterMRU.Popup(oPos.X, oPos.Y);
End;

Procedure TFrameGrid.BuildMRUMenu;
Var
  i: Integer;
  oMenu: TMenuItem;
Begin
  pmnuFilterMRU.Items.Clear;

  For i := 0 To FFilterMRU.Count - 1 Do
  Begin
    oMenu := TMenuItem.Create(pmnuFilterMRU);
    oMenu.Caption := FFilterMRU[i];
    oMenu.OnClick := @mnuSetFilterClick;
    oMenu.Tag := i;

    pmnuFilterMRU.Items.Add(oMenu);
  End;
End;

Procedure TFrameGrid.mnuSetFilterClick(Sender: TObject);
Var
  sFilter: String;
Begin
  If FDataset.Active Then
    If (Sender Is TMenuItem) Then
    Begin
      //ClearFilter;

      sFilter := FFilterMRU[TMenuItem(Sender).Tag];
      SetFilter(sFilter, []);
    End;
End;

Procedure TFrameGrid.btnShowFilterDlgClick(Sender: TObject);
Var
  oDlg: TdlgSQLFilter;
Begin
  If FDataset.Active Then
  Begin
    oDlg := TdlgSQLFilter.Create(Self);
    Try
      // ClearFilter;

      oDlg.Initialise(FDataset);
      If FDataset.Filtered Then
        oDlg.Filter := FDataset.Filter;

      If oDlg.ShowModal = mrOk Then
        SetFilter(oDlg.Filter, oDlg.FilterOptions);
    Finally
      oDlg.Free;
    End;
  End;
End;

Procedure TFrameGrid.ClearFilter;
Begin
  SetFilter('', []);
End;

Procedure TFrameGrid.SetFilter(AFilter: String; AFilterOptions: TFilterOptions);
Var
  sFilter, sError, sCurr: String;
  iCurr: Integer;
Begin
  sFilter := Trim(AFilter);
  sCurr := FDataset.Filter;
  If (sCurr = sFilter) And (FDataset.FilterOptions = AFilterOptions) And
    (FDataset.Filtered = (sFilter <> '')) Then
    Exit;

  grdSQL.SelectedRows.Clear;

  FDataset.DisableControls;
  Try
    iCurr := FFilterMRU.IndexOf(sFilter);

    // Ensure the latest Filter is pushed into the top of the list
    // Also facilitates removing corrupt filters from the list
    If iCurr > 0 Then
      FFilterMRU.Delete(iCurr);

    Try
      // Try to apply the filter.
      FDataset.FilterOptions := AFilterOptions;
      FDataset.Filter := sFilter;
      FDataset.Filtered := (sFilter <> '');
    Except
      On E: Exception Do
      Begin
        sError := 'Filter: ' + AFilter + LineEnding + LineEnding;
        sError := sError + 'Error: ' + E.ClassName + LineEnding + LineEnding;
        sError := sError + E.Message;

        ShowMessage(sError);
        sFilter := '';
      End;
    End;

    If (iCurr <> 0) And (FDataset.Filtered) Then
    Begin
      FFilterMRU.Insert(0, sFilter);

      // Lets keep the MRU sane...
      If FFilterMRU.Count > 10 Then
        FFilterMRU.Delete(FFilterMRU.Count - 1);
    End;
  Finally
    FDataset.EnableControls;
  End;

  UpdateScrollbar;
  SetFilterHint(sFilter);
  RefreshUI;

  If Assigned(FOnAfterFilter) Then
    FOnAfterFilter(Self);
End;

Procedure TFrameGrid.cboFieldsChange(Sender: TObject);
Begin
  If (FDataset.Active) And (Assigned(FDataset.FindField(cboFields.Text))) Then
    DBMemo.DataField := cboFields.Text
  Else
    DBMemo.DataField := '';

  FMemoField := DBMemo.DataField;
End;

Procedure TFrameGrid.DBMemoKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Var
  oField: TField;
Begin
  If (ssCtrl In Shift) And ((Key = Ord('C')) Or (Key = Ord('c'))) Then
  Begin
    oField := DBMemo.Field;

    If Assigned(oField) Then
      DBMemo.CopyToClipboard
    Else
      Clipboard.AsText := 'Memo not active';
  End;
End;

Procedure TFrameGrid.grdSQLKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Var
  oField: TField;
Begin
  If Not assigned(dsGrid.DataSet) Then
    Exit;

  If (dsGrid.Dataset.Active) And (ssCtrl In Shift) And ((Key = Ord('C')) Or (Key = Ord('c'))) Then
  Begin
    oField := grdSQL.SelectedField;

    If Assigned(oField) Then
      Clipboard.AsText := oField.AsString
    Else
      Clipboard.AsText := 'No valid cell selected';
  End;
End;

Procedure TFrameGrid.dsGridDataChange(Sender: TObject; Field: TField);
Begin
  If FDataChanging Then
    Exit;

  FDataChanging := True;
  Try
    SetStatus(Status(True));
  Finally
    FDataChanging := False;
  End;
End;

Procedure TFrameGrid.dsGridStateChange(Sender: TObject);
Begin
  //If dsGrid.State = dsBrowse Then
  Begin
    UpdateScrollbar;
    UpdateMemo;
  End;
End;

Procedure TFrameGrid.grdSQLDblClick(Sender: TObject);
Begin
  If Assigned(FOnDblClick) And Assigned(FDataset) And (FDataset.Active) Then
    FOnDblClick(grdSQL);
End;

Procedure TFrameGrid.grdSQLMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If Not Assigned(FDataset) Then
    Exit;

  grdSQL.MouseToCell(X, Y, FMouseCol, FMouseRow);

  If (FMouseRow > 0) And (Button = mbRight) Then
  Begin
    // Get the Row right
    FDataset.MoveBy(FMouseRow - THackDBGrid(grdSQL).Row);

    // Get the column correct
    grdSQL.SelectedIndex := FMouseCol - 1;
  End;

  If Assigned(FOnGridMouseDown) Then
    FOnGridMouseDown(Sender, Button, Shift, X, Y);

  If (FMouseRow > 0) And (Button = mbLeft) Then
    FMouse := Point(X, Y);
End;

Procedure TFrameGrid.grdSQLMouseLeave(Sender: TObject);
Begin
  FMouse := Point(-1, -1);
End;

Procedure TFrameGrid.grdSQLMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Var
  iRow, iCol: Integer;
  iRec: Integer;
  oDataLink: TComponentDataLink;
Begin
  //Application.MainForm.Caption := Format('X: %d, %d.  MOUSE: %d, %d', [X, Y, FMouse.X, FMouse.Y]);
  Inherited;

  If Assigned(FOnGridStartDrag) And (FMouse.X <> -1) Then
    If (Abs(FMouse.X - X) > 9) Or (Abs(FMouse.Y - Y) > 9) Then
    Begin
      grdSQL.BeginDrag(False);
      FMouse := Point(-1, -1);
    End;

  THackDBGrid(Sender).MouseToCell(X, Y, iCol, iRow);

  If dgTitles In TDBGrid(Sender).Options Then
    Dec(iRow);
  If dgIndicator In TDBGrid(Sender).Options Then
    Dec(iCol);

  oDataLink := THackDBGrid(grdSQL).DataLink;
  If oDataLink.Active And (iRow >= 0) And (iCol >= 0) Then
  Begin
    iRec := oDataLink.ActiveRecord;
    Try
      oDataLink.ActiveRecord := iRow;
      grdSQL.Hint := grdSQL.Columns[iCol].Field.AsString;
      Application.ActivateHint(Mouse.CursorPos);
    Finally
      oDataLink.ActiveRecord := iRec;
    End;
  End;
End;

Procedure TFrameGrid.grdSQLMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  FMouse := Point(-1, -1);
End;

Procedure TFrameGrid.SetGridToSingleLine;
Var
  tsTemp: TTextStyle;
  iRowHeight: Integer;
Begin
  FGridIsMultiLine := False;

  tsTemp := grdSQL.Canvas.TextStyle;
  tsTemp.EndEllipsis := True;
  tsTemp.SingleLine := True;
  tsTemp.Wordbreak := False;
  tsTemp.Layout := tlTop;

  iRowHeight := grdSQL.Canvas.TextHeight('|');
  THackDBGrid(grdSQL).DefaultRowHeight := 6 + iRowHeight;
  THackDBGrid(grdSQL).RowHeights[0] := 6 + iRowHeight;


  grdSQL.Canvas.TextStyle := tsTemp;
  grdSQL.Invalidate;
End;

Procedure TFrameGrid.grdSQLPrepareCanvas(Sender: TObject; DataCol: Integer;
  Column: TColumn; AState: TGridDrawState);

//http://forum.lazarus.freepascal.org/index.php/topic,25004.msg151201.html
// TODO, Place in a support unit.
  Function CalcSelectionColor(c: TColor; ADelta: Byte): TColor;
  Type
    TRGBA = Record
    R, G, B, A: Byte
  End;
  Begin
    c := ColorToRGB(c);
    TRGBA(Result).A := 0;

    If TRGBA(c).R < 128 Then
      TRGBA(Result).R := TRGBA(c).R + ADelta
    Else
      TRGBA(Result).R := TRGBA(c).R - ADelta;

    If TRGBA(c).G < 128 Then
      TRGBA(Result).G := TRGBA(c).G + ADelta
    Else
      TRGBA(Result).G := TRGBA(c).G - ADelta;

    If TRGBA(c).B < 128 Then
      TRGBA(Result).B := TRGBA(c).B + ADelta
    Else
      TRGBA(Result).B := TRGBA(c).B - ADelta;
  End;

Var
  oField: TField;
  oColor: TColor;
  sColor, sTemp: String;
  tsTemp: TTextStyle;
  iLines, iRowHeight: Integer;
Begin
  tsTemp := grdSQL.Canvas.TextStyle;
  tsTemp.EndEllipsis := True;

  If FAllowMultiline Then
  Begin
    sTemp := Column.Field.AsString;

    iLines := Min(Pos(#10, sTemp), 2);
    If iLines > 0 Then
    Begin
      tsTemp.Layout := tlTop;

      iRowHeight := grdSQL.Canvas.TextHeight('|');
      THackDBGrid(grdSQL).DefaultRowHeight := 6 + ((iLines + 1) * iRowHeight);
      THackDBGrid(grdSQL).RowHeights[0] := 6 + iRowHeight;

      FGridIsMultiLine := True;
    End;

    If FGridIsMultiLine Then
    Begin
      tsTemp.SingleLine := False;
      tsTemp.Wordbreak := True;
    End;
  End;

  grdSQL.Canvas.TextStyle := tsTemp;

  If (FReadOnlyFields.Count > 0) And Column.ReadOnly Then
  Begin
    grdSQL.Canvas.Brush.Color := CalcSelectionColor(clBtnFace, 32);
    grdSQL.Canvas.Font.Color := clBtnText;
  End
  Else If Not (gdSelected In AState) Then
  Begin
    oField := FDataset.FindField('Colour_ID');
    If Not Assigned(oField) Then
      oField := FDataset.FindField('Color_ID');

    If Assigned(oField) Then
    Begin
      sColor := Lowercase(Trim(oField.Text));
      oColor := clNone;

      Case sColor Of
        'ltred': oColor := TColor($00D9D9FF);
        'red': oColor := TColor($008080FF);
        'green': oColor := TColor($0080FF80);
        'blue': oColor := TColor($00FF9F40);
        'orange': oColor := TColor($0084ACFF); // $ B9DAFF
        'yellow': oColor := TColor($0080FFFF);
        '': oColor := clNone;
        Else
          oColor := StringToColorDef(sColor, clNone);
      End;

      If (oColor <> clNone) Then
      Begin
        If gdRowHighlight In AState Then
          oColor := CalcSelectionColor(oColor, 32);

        If oColor <> 0 Then
          grdSQL.Canvas.Brush.Color := oColor;
      End;

      grdSQL.Canvas.Font.Color := clBlack;
    End;
  End;
End;

Procedure TFrameGrid.grdSQLStartDrag(Sender: TObject; Var DragObject: TDragObject);
Begin
  If assigned(FOnGridStartDrag) Then
    FOnGridStartDrag(Sender);
End;

Procedure TFrameGrid.btnClearFilterClick(Sender: TObject);
Begin
  ClearFilter;
End;

Initialization

End.
