Unit DialogSQLFilter;

{-------------------------------------------------------------------------------
  Package   : IM_application
  Unit      : DialogSQLFilter.pas
  Description
    Dialog form for composing and applying SQL dataset filters.

  Source
    Copyright (c) 2025
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    ~2008: Creation date unknown, original local SVN repository lost
    2014-07-05: Uploaded to SourceForge/Package "Shared"
    2024-01-22: Migrated to Github.  Refactored package to "IM_application"
    2025-11-29: Added LGPL-3.0-or-later license header

  License
    This file is part of IM_application.lpk.

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



{$mode objfpc}

Interface

Uses
  Classes, SysUtils, FileUtil, SynMemo, SynHighlighterSQL, LResources, Forms, Controls, Graphics,
  Dialogs, ButtonPanel, StdCtrls, ExtCtrls, ComCtrls, Buttons, DB;

Type

  { TdlgSQLFilter }

  TdlgSQLFilter = Class(TForm)
    btnAdd: TSpeedButton;
    ButtonPanel1: TButtonPanel;
    cboCompare: TComboBox;
    cboField: TComboBox;
    cboValue: TComboBox;
    cgFilterOptions: TCheckGroup;
    ilImages: TImageList;
    memFilter: TSynMemo;
    pnlText: TPanel;
    pnlFields: TPanel;
    SynSQLSyn1: TSynSQLSyn;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    btnClear: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    Procedure btnAddClick(Sender: TObject);
    Procedure btnClearClick(Sender: TObject);
    Procedure btnUseClick(Sender: TObject);
    Procedure CancelButtonClick(Sender: TObject);
    Procedure cboFieldChange(Sender: TObject);
    Procedure OKButtonClick(Sender: TObject);
    Procedure ToolButton1Click(Sender: TObject);
  Private
    FDataset: TDataset;
    FUseMemo: Boolean;
    Function GetCurrentFilter: String;
    Function GetFilter: String;
    Function GetFilterOptions: TFilterOptions;
    Procedure SetFilter(Const AValue: String);

    Function FieldIsString: Boolean;
    Function FieldIsDate: Boolean;

    Procedure HideText;
    Procedure ShowText;
  Public
    Procedure Initialise(ADataset: TDataset);

    Property Dataset: TDataset read FDataset;

    Property Filter: String read GetFilter write SetFilter;
    Property FilterOptions: TFilterOptions read GetFilterOptions;
  End;

Var
  dlgSQLFilter: TdlgSQLFilter;

Implementation

Uses
  Math, OSSupport;

{$R *.lfm}

{ TdlgSQLFilter }

Procedure TdlgSQLFilter.Initialise(ADataset: TDataset);
Var
  i: Integer;
  oField: TField;
Begin
  FDataset := ADataset;

  FUseMemo := False;
  HideText;

  If Assigned(FDataset) Then
  Begin
    FDataset.DisableControls;
    Try
      cboField.Items.Clear;

      For i := 0 To FDataset.FieldCount - 1 Do
      Begin
        oField := FDataset.Fields[i];

        If oField.Visible Then
          cboField.Items.Add(oField.FieldName);
      End;

      cboField.ItemIndex := 0;
      cboFieldChange(nil);
    Finally
      FDataset.EnableControls;
    End;
  End;
End;

Procedure TdlgSQLFilter.ShowText;
Begin
  If Not pnlText.Visible Then
  Begin
    Constraints.MinHeight := 115 + Min(105, pnlText.Height);
    Height := Constraints.MinHeight;

    pnlText.Visible := True;
    pnlText.Top := pnlFields.Top + pnlFields.Height;
  End;
End;

Procedure TdlgSQLFilter.HideText;
Begin
  pnlText.Visible := False;
  Constraints.MinHeight := 115;
  Height := Constraints.MinHeight;
End;

Procedure TdlgSQLFilter.OKButtonClick(Sender: TObject);
Begin
  ModalResult := mrOk;
End;

Procedure TdlgSQLFilter.ToolButton1Click(Sender: TObject);
Begin
  If (TToolButton(Sender).Tag = 1) Or (memFilter.Focused) Then
    memFilter.InsertTextAtCaret(' ' + TToolButton(Sender).Caption)
  Else
    memFilter.Lines.Add('  ' + TToolButton(Sender).Caption);
  FUseMemo := True;
  ShowText;
End;

Function TdlgSQLFilter.GetFilterOptions: TFilterOptions;
Begin
  Result := [];
  If cgFilterOptions.Checked[0] Then
    Result := Result + [foCaseInsensitive];

  If cgFilterOptions.Checked[0] Then
    Result := Result + [foNoPartialCompare];
End;

Function TdlgSQLFilter.GetCurrentFilter: String;
Begin
  If FieldIsString Or FieldIsDate Then
    Result := Format(' (%s %s %s)', [cboField.Text, cboCompare.Text, QuotedStr(cboValue.Text)])
  Else
    Result := Format(' (%s %s %s)', [cboField.Text, cboCompare.Text, cboValue.Text]);
End;

Function TdlgSQLFilter.GetFilter: String;
Begin
  If FUseMemo Then
    Result := Trim(memFilter.Lines.Text)
  Else
    Result := Trim(GetCurrentFilter);
End;

Procedure TdlgSQLFilter.SetFilter(Const AValue: String);
Begin
  // TODO Decode incoming filter

  // but in the meantime...
  memFilter.Lines.Text := AValue;

  FUseMemo := Trim(AValue) <> '';

  If FUseMemo Then
    ShowText;
End;

Function TdlgSQLFilter.FieldIsString: Boolean;
Var
  oField: TField;
Begin
  oField := FDataset.FieldByName(cboField.Text);

  Result := oField.DataType In [ftString, ftFixedChar, ftWideString,
    ftFixedWideChar, ftWideMemo];
End;

Function TdlgSQLFilter.FieldIsDate: Boolean;
Var
  oField: TField;
Begin
  oField := FDataset.FieldByName(cboField.Text);

  Result := oField.DataType In [ftDate, ftTime, ftDateTime];
End;

Procedure TdlgSQLFilter.CancelButtonClick(Sender: TObject);
Begin
  ModalResult := mrCancel;
End;

Procedure TdlgSQLFilter.btnAddClick(Sender: TObject);
Begin
  FUseMemo := True;
  ShowText;

  If (memFilter.Focused) Then
    memFilter.InsertTextAtCaret(GetCurrentFilter)
  Else If (Trim(memFilter.Lines.Text) = '') And (memFilter.Lines.Count = 1) Then
    memFilter.Lines.Text := GetCurrentFilter
  Else
    memFilter.Lines.Add(GetCurrentFilter);
End;

Procedure TdlgSQLFilter.btnClearClick(Sender: TObject);
Begin
  memFilter.Lines.Clear;
End;

Procedure TdlgSQLFilter.btnUseClick(Sender: TObject);
Begin
  memFilter.Text := GetCurrentFilter;
  ModalResult := mrOk;
End;

Procedure TdlgSQLFilter.cboFieldChange(Sender: TObject);
Var
  slValues: TStringList;
  oBookmark: TBookmark;
Begin
  If Assigned(FDataset) And FDataset.Active Then
  Begin
    SetBusy;
    oBookmark := FDataset.Bookmark;

    FDataset.DisableControls;
    slValues := TStringList.Create;
    slValues.Sorted := True;
    slValues.Duplicates := dupIgnore;
    Try
      cboValue.Items.Clear;
      cboValue.Style := csDropDown;
      cboValue.Text := '';

      FDataset.First;

      While Not FDataset.EOF Do
      Begin
        slValues.Add(FDataset.FieldByName(cboField.Text).DisplayText);
        FDataset.Next;
      End;

      cboValue.Items.AddStrings(slValues);

      If cboValue.Items.Count > 0 Then
        cboValue.ItemIndex := 0;
    Finally
      slValues.Free;
      FDataset.GotoBookmark(oBookmark);
      FDataset.FreeBookmark(oBookmark);

      FDataset.EnableControls;
      ClearBusy;
    End;
  End;
End;

Initialization

End.
