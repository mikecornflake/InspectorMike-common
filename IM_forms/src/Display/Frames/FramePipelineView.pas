Unit FramePipelineView;

{$mode objfpc}{$H+}

{-------------------------------------------------------------------------------
  Package   : IM_forms
  Unit      : FramePipelineView.pas
  Description
    Frame for Visual Component to display KP based data on a Pipeline

  Source
    Copyright (c) 2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    2019: Original Frame developed for C3PU Pipeline Module.
    2026-06-20: Migrated to IM_forms as part of package re-org

  License
    This file is part of IM_forms.lpk.

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
  Classes, SysUtils, Forms, Controls, ComCtrls, ExtCtrls, StdCtrls,
  ActnList, Menus, FrameBase, PipelineEventMap, DB;

Type
  { TFramePipelineView }

  TFramePipelineView = Class(TFrameBase)
    acPipelineDisplay: TActionList;
    acCopyToClipboard: TAction;
    acZoomExtents: TAction;
    acZoomIn: TAction;
    acZoomOut: TAction;
    ilPipelineDisplay: TImageList;
    pmnuCopyToClipboard: TMenuItem;
    Separator1: TMenuItem;
    mnuZoomExtents2: TMenuItem;
    mnu2km: TMenuItem;
    mnu1km: TMenuItem;
    mnu50m: TMenuItem;
    mnu500m: TMenuItem;
    mnu100m: TMenuItem;
    pmnuZoomExtents: TMenuItem;
    pmnuZoomIn: TMenuItem;
    pmnuZoomOut: TMenuItem;
    pmPipelineDisplay: TPopupMenu;
    pnlPipelineDisplay: TPanel;
    mnuRange: TPopupMenu;
    sbPipelineDisplay: TStatusBar;
    ScrollBar: TScrollBar;
    tbPipelineDisplay: TToolBar;
    tmrScroll: TTimer;
    btnZoomOut: TToolButton;
    btnZoomIn: TToolButton;
    btnZoomExtents: TToolButton;
    ToolButton1: TToolButton;
    btnRange: TToolButton;

    procedure acCopyToClipboardExecute(Sender: TObject);
    Procedure acZoomExtentsExecute(Sender: TObject);
    Procedure acZoomInExecute(Sender: TObject);
    Procedure acZoomOutExecute(Sender: TObject);
    Procedure mnuRangeClick(Sender: TObject);
    Procedure pmnuZoomInClick(Sender: TObject);
    Procedure pmnuZoomOutClick(Sender: TObject);
    Procedure ScrollBarChange(Sender: TObject);
    Procedure tmrScrollTimer(Sender: TObject);
    Procedure PipelineDisplayMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

  Private
    FCount: Integer;
    FLoaded: Boolean;
    FMinKP, FMaxKP: Extended;
    FOnAfterScroll: TNotifyEvent;
    FPipelineDisplay: TPipelineEventMap;
    FScrolling: Boolean;

    Function GetGraphMode: TPipelineDisplayType;
    Function GetKP: Extended;
    Function HasData: Boolean;
    Function VisibleRange: Extended;
    Function FullRange: Extended;

    Procedure UpdateData;
    Procedure RecalcScrollbar;
    Procedure SetGraphMode(AValue: TPipelineDisplayType);
    Procedure SetKP(AValue: Extended);
    Procedure SetDisplayRange(AStartKP, AEndKP: Extended);

  Public
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure BeginUpdate;
    Procedure AddData(ATitle: String; AStart, AEnd: Extended);
    Procedure EndUpdate;
    Function Count: Integer;
    Procedure Clear;

    Procedure RefreshUI; Override;

    Property GraphMode: TPipelineDisplayType Read GetGraphMode Write SetGraphMode;
    Property Loaded: Boolean Read FLoaded;
    Property KP: Extended Read GetKP Write SetKP;
    Property OnAfterScroll: TNotifyEvent Read FOnAfterScroll Write FOnAfterScroll;
  End;

Implementation

Uses
  Math;

  {$R *.lfm}

Constructor TFramePipelineView.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  FPipelineDisplay := TPipelineEventMap.Create(pnlPipelineDisplay);
  FPipelineDisplay.Parent := pnlPipelineDisplay;
  FPipelineDisplay.Align := alClient;

  FPipelineDisplay.ShowScale := True;
  FPipelineDisplay.ShowTitles := True;
  FPipelineDisplay.ColLines := True;
  FPipelineDisplay.RowLines := True;

  FPipelineDisplay.OnMouseMove := @PipelineDisplayMouseMove;
  FPipelineDisplay.PopupMenu := pmPipelineDisplay;

  FCount := -1;
  FMinKP := 0;
  FMaxKP := 0;
  FLoaded := False;
  FScrolling := False;

  ScrollBar.Visible := False;
  ScrollBar.Enabled := False;
End;

Destructor TFramePipelineView.Destroy;
Begin
  FreeAndNil(FPipelineDisplay);
  Inherited Destroy;
End;

Function TFramePipelineView.HasData: Boolean;
Begin
  Result := FCount > 0;
End;

Function TFramePipelineView.VisibleRange: Extended;
Begin
  Result := FPipelineDisplay.EndValue - FPipelineDisplay.StartValue;
End;

Function TFramePipelineView.FullRange: Extended;
Begin
  Result := FMaxKP - FMinKP;
End;

Procedure TFramePipelineView.BeginUpdate;
Begin
  FPipelineDisplay.BeginUpdate;
End;

Procedure TFramePipelineView.AddData(ATitle: String; AStart, AEnd: Extended);
Begin
  FPipelineDisplay.AddData(ATitle, AStart, AEnd);
End;

Procedure TFramePipelineView.EndUpdate;
Begin
  FPipelineDisplay.EndUpdate;

  FCount := FPipelineDisplay.Count;
  FMinKP := FPipelineDisplay.DataMinValue;
  FMaxKP := FPipelineDisplay.DataMaxValue;
  FLoaded := FCount > 0;

  UpdateData;
  RecalcScrollbar;
End;

Function TFramePipelineView.Count: Integer;
Begin
  Result := FPipelineDisplay.Count;
End;

Procedure TFramePipelineView.Clear;
Begin
  FPipelineDisplay.BeginUpdate;
  Try
    FPipelineDisplay.Clear;
    FCount := -1;
    FMinKP := 0;
    FMaxKP := 0;
    FLoaded := False;
  Finally
    FPipelineDisplay.EndUpdate;
  End;

  UpdateData;
  RecalcScrollbar;
End;

Procedure TFramePipelineView.SetDisplayRange(AStartKP, AEndKP: Extended);
Begin
  If AEndKP <= AStartKP Then
    Exit;

  FPipelineDisplay.StartValue := AStartKP;
  FPipelineDisplay.EndValue := AEndKP;

  RecalcScrollbar;
End;

Procedure TFramePipelineView.acZoomInExecute(Sender: TObject);
Begin
  If Not HasData Then
    Exit;

  FPipelineDisplay.ZoomIn(FPipelineDisplay.Width Div 2, FPipelineDisplay.Height Div 2, 50);
  RecalcScrollbar;
End;

Procedure TFramePipelineView.acZoomOutExecute(Sender: TObject);
Begin
  If Not HasData Then
    Exit;

  FPipelineDisplay.ZoomOut(FPipelineDisplay.Width Div 2, FPipelineDisplay.Height Div 2, 50);
  RecalcScrollbar;
End;

Procedure TFramePipelineView.pmnuZoomInClick(Sender: TObject);
Var
  pMouse: TPoint;
Begin
  If Not HasData Then
    Exit;

  pMouse := FPipelineDisplay.ScreenToClient(Mouse.CursorPos);
  FPipelineDisplay.ZoomIn(pMouse.X, pMouse.Y, 50);
  RecalcScrollbar;
End;

Procedure TFramePipelineView.pmnuZoomOutClick(Sender: TObject);
Var
  pMouse: TPoint;
Begin
  If Not HasData Then
    Exit;

  pMouse := FPipelineDisplay.ScreenToClient(Mouse.CursorPos);
  FPipelineDisplay.ZoomOut(pMouse.X, pMouse.Y, 50);
  RecalcScrollbar;
End;

Procedure TFramePipelineView.acZoomExtentsExecute(Sender: TObject);
Begin
  If Not HasData Then
    Exit;

  SetDisplayRange(FMinKP, FMaxKP);
End;

procedure TFramePipelineView.acCopyToClipboardExecute(Sender: TObject);
begin
  FPipelineDisplay.CopyToClipboard;
end;

Procedure TFramePipelineView.mnuRangeClick(Sender: TObject);
Var
  dRange: Extended;
  dKP: Extended;
Begin
  If (Not HasData) Or Not (Sender Is TMenuItem) Then
    Exit;

  dRange := TMenuItem(Sender).Tag / 1000;
  If dRange <= 0 Then
    Exit;

  dKP := FPipelineDisplay.MidValue;
  SetDisplayRange(dKP - (dRange / 2), dKP + (dRange / 2));
End;

Procedure TFramePipelineView.UpdateData;
Begin
  If Not HasData Then
  Begin
    sbPipelineDisplay.Panels[0].Text := 'Not loaded';
    sbPipelineDisplay.Panels[1].Text := 'Min KP:';
    sbPipelineDisplay.Panels[2].Text := 'Max KP:';
  End
  Else
  Begin
    sbPipelineDisplay.Panels[0].Text := Format('Records: %d', [FCount]);
    sbPipelineDisplay.Panels[1].Text := Format('Min KP: %0.3f', [FMinKP]);
    sbPipelineDisplay.Panels[2].Text := Format('Max KP: %0.3f', [FMaxKP]);
  End;

  RefreshUI;
End;

Procedure TFramePipelineView.PipelineDisplayMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
Var
  dKP: Extended;
Begin
  dKP := FPipelineDisplay.GetKP(X, Y);

  If dKP = -1 Then
    FPipelineDisplay.Hint := ''
  Else
  Begin
    FPipelineDisplay.Hint := Format('KP%0.3f', [dKP]);
    FPipelineDisplay.ShowHint := True;
  End;
End;

Procedure TFramePipelineView.RefreshUI;
Var
  bEnabled: Boolean;
Begin
  bEnabled := HasData;

  acZoomIn.Enabled := bEnabled;
  acZoomOut.Enabled := bEnabled;
  acZoomExtents.Enabled := bEnabled;
  acCopyToClipboard.Enabled := bEnabled;
  pmnuCopyToClipboard.Enabled := bEnabled;

  //btnZoomIn.Enabled := bEnabled;
  //btnZoomOut.Enabled := bEnabled;
  //btnZoomExtents.Enabled := bEnabled;
  //pmnuZoomIn.Enabled := bEnabled;
  //pmnuZoomOut.Enabled := bEnabled;
  //pmnuZoomExtents.Enabled := bEnabled;
  btnRange.Enabled := bEnabled;

  RecalcScrollbar;
End;

Procedure TFramePipelineView.RecalcScrollbar;
Var
  iStart, iEnd, iMin, iMax: Integer;
  iPosition, iPageSize: Integer;
  NeedScrollbar: Boolean;
Begin
  If Not HasData Then
  Begin
    ScrollBar.Tag := 1;
    Try
      ScrollBar.Visible := False;
      ScrollBar.Enabled := False;
    Finally
      ScrollBar.Tag := 0;
    End;
    Exit;
  End;

  NeedScrollbar := (FullRange > 0) And (VisibleRange < FullRange - 0.000001);

  ScrollBar.Tag := 1;
  Try
    ScrollBar.Visible := NeedScrollbar;
    ScrollBar.Enabled := NeedScrollbar;

    If NeedScrollbar Then
    Begin
      iStart := Trunc(1000 * FPipelineDisplay.StartValue);
      iEnd := Trunc(1000 * FPipelineDisplay.EndValue);
      iMin := Trunc(1000 * FMinKP);
      iMax := Trunc(1000 * FMaxKP);
      iPageSize := Max(1, iEnd - iStart);
      iPosition := iStart;

      ScrollBar.SetParams(iPosition, iMin, iMax, iPageSize);
    End;
  Finally
    ScrollBar.Tag := 0;
  End;

  If (Not FScrolling) And FLoaded Then
    tmrScroll.Enabled := True;
End;

Procedure TFramePipelineView.ScrollBarChange(Sender: TObject);
Var
  dRange: Extended;
  dStart: Extended;
Begin
  If (ScrollBar.Tag <> 0) Or (Not ScrollBar.Enabled) Or (Not HasData) Then
    Exit;

  dRange := VisibleRange;
  dStart := ScrollBar.Position / 1000;

  FPipelineDisplay.StartValue := dStart;
  FPipelineDisplay.EndValue := dStart + dRange;

  If Not FScrolling Then
    tmrScroll.Enabled := True;
End;

Procedure TFramePipelineView.tmrScrollTimer(Sender: TObject);
Begin
  tmrScroll.Enabled := False;

  If FLoaded And Assigned(FOnAfterScroll) And (Not FScrolling) Then
  Begin
    FScrolling := True;
    Try
      FOnAfterScroll(Self);
    Finally
      FScrolling := False;
    End;
  End;
End;

Procedure TFramePipelineView.SetGraphMode(AValue: TPipelineDisplayType);
Begin
  FPipelineDisplay.GraphMode := AValue;
End;

Function TFramePipelineView.GetGraphMode: TPipelineDisplayType;
Begin
  Result := FPipelineDisplay.GraphMode;
End;

Function TFramePipelineView.GetKP: Extended;
Begin
  Result := FPipelineDisplay.MidValue;
End;

Procedure TFramePipelineView.SetKP(AValue: Extended);
Begin
  If Not HasData Then
    Exit;

  FPipelineDisplay.MidValue := AValue;
  RecalcScrollbar;
End;

End.
