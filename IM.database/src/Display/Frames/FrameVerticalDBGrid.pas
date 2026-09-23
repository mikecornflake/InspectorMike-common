Unit FrameVerticalDBGrid;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, ValEdit, DB, Grids, Graphics, Clipbrd, ActnList, Menus, Types;

Type

  { TFrameVerticalDBGrid }

  TFrameVerticalDBGrid = Class(TFrame)
    actCopyTableToClipboard: TAction;
    actCopyRowToClipboard: TAction;
    actVerical: TActionList;
    grdVertical: TValueListEditor;
    ilVertical: TImageList;
    mnuCopyTableToClipboard: TMenuItem;
    mnuCopyRowToClipboard: TMenuItem;
    pmVertical: TPopupMenu;

    Procedure actCopyRowToClipboardExecute(Sender: TObject);
    Procedure actCopyTableToClipboardExecute(Sender: TObject);

      procedure grdVerticalDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    Procedure grdVerticalPrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    Procedure pmVerticalPopup(Sender: TObject);
  Private
    FDataset: TDataset;
    FDataSource: TDataSource;

    Function GetDataset: TDataset;
    Procedure SetDataset(Const AValue: TDataset);

    Procedure DataChanged(Sender: TObject; Field: TField);
    Procedure StateChanged(Sender: TObject);

    Procedure RefreshGrid;
  Public
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure CopyActiveRowToClipboard;
    Procedure CopyTableToClipboard;

    Property Dataset: TDataset Read GetDataset Write SetDataset;
  End;

Implementation

Uses
  DBSupport;

{$R *.lfm}

{ TFrameVerticalDBGrid }

Constructor TFrameVerticalDBGrid.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  FDataSource := TDataSource.Create(Self);
  FDataSource.OnDataChange := @DataChanged;
  FDataSource.OnStateChange := @StateChanged;

  FDataset := nil;

  With grdVertical Do
  Begin
    Options := Options - [goEditing];

    DisplayOptions := DisplayOptions + [doColumnTitles];

    Cells[0, 0] := 'Name';
    Cells[1, 0] := 'Value';

    EditorMode := False;
  End;
End;

Destructor TFrameVerticalDBGrid.Destroy;
Begin
  FDataSource.Dataset := nil;

  FreeAndNil(FDataSource);
  Inherited Destroy;
End;

Procedure TFrameVerticalDBGrid.SetDataset(Const AValue: TDataset);
Begin
  If FDataset = AValue Then
    Exit;

  FDataset := AValue;
  FDataSource.Dataset := AValue;

  RefreshGrid;
End;

Procedure TFrameVerticalDBGrid.grdVerticalPrepareCanvas(Sender: TObject;
  aCol, aRow: Integer; aState: TGridDrawState);
Begin
  If aRow = 0 Then
    grdVertical.Canvas.Font.Style := grdVertical.Canvas.Font.Style + [fsBold];
End;

Procedure TFrameVerticalDBGrid.actCopyTableToClipboardExecute(Sender: TObject);
Begin
  CopyTableToClipboard;
End;

procedure TFrameVerticalDBGrid.grdVerticalDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
Var
  S: String;
  X, Y: Integer;
Begin
  If (ACol <> 0) Or (ARow = 0) Then
    Exit;

  S := TValueListEditor(Sender).Cells[ACol, ARow];

  With TValueListEditor(Sender).Canvas Do
  Begin
    FillRect(ARect);

    X := ARect.Right - TextWidth(S) - 4;
    Y := ARect.Top + ((ARect.Height - TextHeight(S)) Div 2);

    TextOut(X, Y, S);
  End;
end;

Procedure TFrameVerticalDBGrid.actCopyRowToClipboardExecute(Sender: TObject);
Begin
  CopyActiveRowToClipboard;
End;

Procedure TFrameVerticalDBGrid.pmVerticalPopup(Sender: TObject);
Begin
  actCopyRowToClipboard.Enabled := Assigned(FDataset) And FDataset.Active;
  actCopyTableToClipboard.Enabled := Assigned(FDataset) And FDataset.Active;
End;

Function TFrameVerticalDBGrid.GetDataset: TDataset;
Begin
  Result := FDataset;
End;

Procedure TFrameVerticalDBGrid.DataChanged(Sender: TObject; Field: TField);
Begin
  RefreshGrid;
End;

Procedure TFrameVerticalDBGrid.StateChanged(Sender: TObject);
Begin
  RefreshGrid;
End;

Procedure TFrameVerticalDBGrid.RefreshGrid;

  Function CalcNameColumnWidth(AEditor: TValueListEditor; ADataset: TDataset): Integer;
  Var
    i, iWidth: Integer;
  Begin
    Result := AEditor.Canvas.TextWidth('Name') + 16;

    For i := 0 To ADataset.FieldCount - 1 Do
      If ADataset.Fields[i].Visible Then
      Begin
        iWidth := AEditor.Canvas.TextWidth('    '+ADataset.Fields[i].DisplayLabel) + 20;

        If iWidth > Result Then
          Result := iWidth;
      End;
  End;

Var
  i: Integer;
  oField: TField;
Begin
  grdVertical.Strings.BeginUpdate;
  Try
    grdVertical.Strings.Clear;

    If Not Assigned(FDataset) Then
      Exit;

    If Not FDataset.Active Then
      Exit;

    For i := 0 To FDataset.FieldCount - 1 Do
    Begin
      oField := FDataset.Fields[i];

      If oField.Visible Then
        grdVertical.Strings.Add('    '+oField.DisplayLabel +
          grdVertical.Strings.NameValueSeparator + oField.DisplayText);
    End;
  Finally
    grdVertical.Strings.EndUpdate;
  End;

  grdVertical.ColWidths[0] := CalcNameColumnWidth(grdVertical, FDataset);
  grdVertical.ColWidths[1] := grdVertical.ClientWidth - grdVertical.ColWidths[0] - grdVertical.GridLineWidth;
End;


Procedure TFrameVerticalDBGrid.CopyActiveRowToClipboard;
Begin
  If grdVertical.Row < grdVertical.FixedRows Then
    Exit;

  Clipboard.AsText := grdVertical.Cells[0, grdVertical.Row] + #9 +
    grdVertical.Cells[1, grdVertical.Row];
End;

Procedure TFrameVerticalDBGrid.CopyTableToClipboard;
Begin
  PasteDatasetIntoClipboardVertical(FDataset);
End;


End.
