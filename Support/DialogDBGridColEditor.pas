Unit DialogDBGridColEditor;

{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : DialogDBGridColEditor.pas
  Description
    Dialog to allow TDBGrid columns to be shown/hidden/reordered

  Source
    Copyright (c) 2025
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    2024-04-27: Creation & uploaded to Github
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

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CheckLst, ComCtrls,
  StdCtrls, ExtCtrls, DBGrids;

Type

  { TdlgGridColumns }

  TdlgGridColumns = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    ImageList1: TImageList;
    lbColumns: TCheckListBox;
    Label1: TLabel;
    Panel1: TPanel;
    ToolBar1: TToolBar;
    btnUp: TToolButton;
    btnDown: TToolButton;
    btnAll: TToolButton;
    ToolButton2: TToolButton;
    Procedure btnAllClick(Sender: TObject);
    Procedure btnDownClick(Sender: TObject);
    Procedure btnUpClick(Sender: TObject);
    Procedure FormActivate(Sender: TObject);
    Procedure lbColumnsSelectionChange(Sender: TObject; User: Boolean);
  Private
    FGrid: TDBGrid;
    Procedure SetGrid(AValue: TDBGrid);

    Procedure RefreshUI;
  Public
    Function Apply: Boolean;

    Property Grid: TDBGrid read FGrid write SetGrid;
  End;

Implementation

{$R *.lfm}

{ TdlgGridColumns }

Procedure TdlgGridColumns.FormActivate(Sender: TObject);
Begin
  RefreshUI;
End;

Procedure TdlgGridColumns.RefreshUI;
Var
  bSelected: Boolean;
Begin
  bSelected := (lbColumns.SelCount > 0);
  btnUp.Enabled := bSelected And (lbColumns.ItemIndex > 0);
  btnDown.Enabled := bSelected And (lbColumns.ItemIndex < lbColumns.Items.Count - 1);
End;

Procedure TdlgGridColumns.SetGrid(AValue: TDBGrid);
Var
  oColumn: TColumn;
  i: Integer;
Begin
  If FGrid = AValue Then Exit;
  FGrid := AValue;

  lbColumns.Clear;

  For i := 0 To FGrid.Columns.Count - 1 Do
  Begin
    oColumn := FGrid.Columns[i];
    lbColumns.Items.Add(oColumn.FieldName);
    lbColumns.Checked[i] := oColumn.Visible;
  End;
End;

Function TdlgGridColumns.Apply: Boolean;
Var
  oColumn: TColumn;
  i: Integer;
  bVisible: Boolean;
  sField: String;
Begin
  Result := False;

  If Assigned(FGrid) Then
  Begin
    For i := 0 To lbColumns.Items.Count - 1 Do
    Begin
      sField := lbColumns.Items[i];
      oColumn := FGrid.Columns.ColumnByFieldname(sField);
      oColumn.Index := i;

      If Assigned(oColumn) Then
      Begin
        bVisible := lbColumns.Checked[i];
        oColumn.Visible := bVisible;
        oColumn.Field.Visible := bVisible;
      End;
    End;
  End;
End;

Procedure TdlgGridColumns.btnUpClick(Sender: TObject);
Var
  iSelected: Integer;
Begin
  If (lbColumns.SelCount = 1) Then
  Begin
    iSelected := lbColumns.ItemIndex;

    If (iSelected > 0) Then
    Begin
      lbColumns.Items.Move(iSelected, iSelected - 1);
      lbColumns.ItemIndex := iSelected - 1;
    End;
  End;

  RefreshUI;
End;

Procedure TdlgGridColumns.lbColumnsSelectionChange(Sender: TObject; User: Boolean);
Begin
  RefreshUI;
End;

Procedure TdlgGridColumns.btnDownClick(Sender: TObject);
Var
  iSelected: Integer;
Begin
  If (lbColumns.SelCount = 1) Then
  Begin
    iSelected := lbColumns.ItemIndex;

    If (iSelected < lbColumns.Items.Count - 1) Then
    Begin
      lbColumns.Items.Move(iSelected, iSelected + 1);
      lbColumns.ItemIndex := iSelected + 1;
    End;

    RefreshUI;
  End;
End;

Procedure TdlgGridColumns.btnAllClick(Sender: TObject);
Var
  i: Integer;
Begin
  For i := 0 To lbColumns.Items.Count - 1 Do
    lbColumns.Checked[i] := True;

  RefreshUI;
End;

End.
