Unit DialogDBGridColEditor;

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
    Procedure btnDownClick(Sender: TObject);
    Procedure btnUpClick(Sender: TObject);
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

Procedure TdlgGridColumns.RefreshUI;
Begin
  btnUp.Enabled := lbColumns.ItemIndex > 0;
  btnDown.Enabled := lbColumns.ItemIndex < lbColumns.Items.Count - 1;
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

End.
