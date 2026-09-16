Unit FrameSettingsSyncedVideo;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons, Spin, ComCtrls, FrameBase,
  ControlGridLayout, Graphics;

Type

  { TFrameSettingsSyncedVideo }

  TFrameSettingsSyncedVideo = Class(TFrameBase)
    cboSequence: TComboBox;
    cbExtend: TCheckBox;
    itTools: TImageList;
    Label1: TLabel;
    lblRows: TLabel;
    lblCols: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    memChannels: TMemo;
    edtRows: TSpinEdit;
    edtCols: TSpinEdit;
    ToolBar1: TToolBar;
    btnSort: TToolButton;
    ToolButton2: TToolButton;
    btnUp: TToolButton;
    btnDown: TToolButton;
    Procedure btnUpClick(Sender: TObject);
    Procedure btnDownClick(Sender: TObject);
    Procedure btnSortClick(Sender: TObject);
    Procedure cboSequenceChange(Sender: TObject);
    Procedure memChannelsClick(Sender: TObject);
    Procedure memChannelsKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
  Private
    Function GetCLS: TControlLayoutSequence;
    Function GetCols: Integer;
    Function GetExtend: Boolean;
    Function GetRows: Integer;
    Procedure SetCLS(Const AValue: TControlLayoutSequence);
    Procedure SetCols(Const AValue: Integer);
    Procedure SetExtend(Const AValue: Boolean);
    Procedure SetRows(Const AValue: Integer);
  Protected
    Procedure DoActivateFrame; Override;
  Public
    Procedure RefreshUI; Override;

    Procedure AssignChannelOrderTo(AOutput: TStrings);
    Procedure AssignChannelOrderFrom(AInput: TStrings);

    Property LayoutRows: Integer Read GetRows Write SetRows;
    Property LayoutCols: Integer Read GetCols Write SetCols;
    Property ControlLayoutSequence: TControlLayoutSequence Read GetCLS Write SetCLS;
    Property Extend: Boolean Read GetExtend Write SetExtend;
  End;

Implementation

{$R *.lfm}

{ TFrameSettingsSyncedVideo }

Procedure TFrameSettingsSyncedVideo.AssignChannelOrderTo(AOutput: TStrings);
Begin
  Assert(Assigned(AOutput), 'TFrameSettingsSyncedVideo:  AOutput must be created');

  AOutput.Assign(memChannels.Lines);
End;

Procedure TFrameSettingsSyncedVideo.AssignChannelOrderFrom(AInput: TStrings);
Begin
  Assert(Assigned(AInput), 'TFrameSettingsSyncedVideo:  AInput must be created');

  memChannels.Lines.Assign(AInput);

  RefreshUI;
End;

Procedure TFrameSettingsSyncedVideo.RefreshUI;
Var
  I: Integer;
Begin
    If (cboSequence.ItemIndex = 0) Then
  Begin
    edtRows.Enabled := False;
    edtRows.Color := clDkGray;

    edtCols.Enabled := True;
    edtCols.Color := clWhite;
  End
  Else
  Begin
    edtRows.Enabled := True;
    edtRows.Color := clWhite;

    edtCols.Enabled := False;
    edtCols.Color := clDkGray;
  End;

  lblRows.Enabled := edtRows.Enabled;
  lblCols.Enabled := edtCols.Enabled;

  If Not memChannels.HandleAllocated Then
    Exit;

  I := memChannels.CaretPos.Y;

  btnUp.Enabled := (memChannels.Lines.Count > 0) And (I > 0);
  btnDown.Enabled := (memChannels.Lines.Count > 0) And (I < memChannels.Lines.Count - 1);
  btnSort.Enabled := memChannels.Lines.Count > 1;
End;

Procedure TFrameSettingsSyncedVideo.SetCLS(Const AValue: TControlLayoutSequence);
Begin
  cboSequence.ItemIndex := Ord(AValue);
  RefreshUI;
End;

Procedure TFrameSettingsSyncedVideo.SetCols(Const AValue: Integer);
Begin
  edtCols.Value := AValue;
End;

Procedure TFrameSettingsSyncedVideo.SetExtend(Const AValue: Boolean);
Begin
  cbExtend.Checked := AValue;
End;

Procedure TFrameSettingsSyncedVideo.SetRows(Const AValue: Integer);
Begin
  edtRows.Value := AValue;
End;

Procedure TFrameSettingsSyncedVideo.DoActivateFrame;
Begin
  Inherited DoActivateFrame;

  RefreshUI;
End;

Procedure TFrameSettingsSyncedVideo.btnSortClick(Sender: TObject);
Var
  sl: TStringList;
Begin
  sl := TStringList.Create;
  Try
    sl.Assign(memChannels.Lines);
    sl.Sort;
    memChannels.Lines.Assign(sl);
  Finally
    sl.Free;
  End;

  RefreshUI;
End;

Procedure TFrameSettingsSyncedVideo.cboSequenceChange(Sender: TObject);
Begin
  RefreshUI;
End;

Procedure TFrameSettingsSyncedVideo.memChannelsClick(Sender: TObject);
Begin
  RefreshUI;
End;

Procedure TFrameSettingsSyncedVideo.memChannelsKeyUp(Sender: TObject;
  Var Key: Word; Shift: TShiftState);
Begin
  RefreshUI;
End;

Function TFrameSettingsSyncedVideo.GetCLS: TControlLayoutSequence;
Begin
  Result := TControlLayoutSequence(cboSequence.ItemIndex);
End;

Function TFrameSettingsSyncedVideo.GetCols: Integer;
Begin
  Result := edtCols.Value;
End;

Function TFrameSettingsSyncedVideo.GetExtend: Boolean;
Begin
  Result := cbExtend.Checked;
End;

Function TFrameSettingsSyncedVideo.GetRows: Integer;
Begin
  Result := edtRows.Value;
End;

Procedure TFrameSettingsSyncedVideo.btnUpClick(Sender: TObject);
Var
  I: Integer;
  S: String;
Begin
  I := memChannels.CaretPos.Y;

  If I <= 0 Then
    Exit;

  S := memChannels.Lines[I - 1];
  memChannels.Lines[I - 1] := memChannels.Lines[I];
  memChannels.Lines[I] := S;

  memChannels.CaretPos := Point(0, I - 1);

  RefreshUI;
End;

Procedure TFrameSettingsSyncedVideo.btnDownClick(Sender: TObject);
Var
  I: Integer;
  S: String;
Begin
  I := memChannels.CaretPos.Y;

  If I < 0 Then
    Exit;

  S := memChannels.Lines[I];
  memChannels.Lines[I] := memChannels.Lines[I + 1];
  memChannels.Lines[I + 1] := S;

  memChannels.CaretPos := Point(0, I + 1);

  RefreshUI;
End;

End.
