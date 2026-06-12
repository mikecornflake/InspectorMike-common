Unit VideoGridLayout;

{-------------------------------------------------------------------------------
  Package   : IM_application
  Unit      : VideoGridLayout.pas
  Description
    Layout manager for arranging multiple video playback frames within
    a parent control.

    Supports configurable row and column counts, spacing, margins and
    alternative placement sequences for multi-channel video review.

  Source
    Copyright (c) 2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    2026-06-12: Initial implementation.
                Extracted from FrameSyncedVideo to separate playback
                logic from presentation layout responsibilities.
                Initial implementation generated with assistance from
                OpenAI ChatGPT GPT-5.5 and reviewed by Mike Thompson.

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

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Controls, ExtCtrls,
  FrameVideoBase;

Type
  TVideoLayoutSequence = (
    vlsLeftToRightThenDown,
    vlsTopToBottomThenRight
    );

  { TVideoGridLayout }

  TVideoGridLayout = Class
  Private
    FParent: TWinControl;
    FRowCount: Integer;
    FColCount: Integer;
    FSequence: TVideoLayoutSequence;
    FPanelMargin: Integer;
    FCellSpacing: Integer;

    Procedure SetRowCount(AValue: Integer);
    Procedure SetColCount(AValue: Integer);

    Procedure GetCellPosition(AIndex: Integer; Out ARow, ACol: Integer);

  Public
    Constructor Create(AParent: TWinControl);

    Procedure LayoutVideos(AVideos: TfmeVideoBaseList);
    Procedure ClearParent;

    Property Parent: TWinControl Read FParent Write FParent;

    Property RowCount: Integer Read FRowCount Write SetRowCount;
    Property ColCount: Integer Read FColCount Write SetColCount;

    Property Sequence: TVideoLayoutSequence Read FSequence Write FSequence;

    Property PanelMargin: Integer Read FPanelMargin Write FPanelMargin;
    Property CellSpacing: Integer Read FCellSpacing Write FCellSpacing;
  End;

Implementation

{ TVideoGridLayout }

Constructor TVideoGridLayout.Create(AParent: TWinControl);
Begin
  Inherited Create;

  FParent := AParent;
  FRowCount := 1;
  FColCount := 1;
  FSequence := vlsLeftToRightThenDown;
  FPanelMargin := 0;
  FCellSpacing := 4;
End;

Procedure TVideoGridLayout.SetRowCount(AValue: Integer);
Begin
  If AValue < 1 Then
    AValue := 1;

  FRowCount := AValue;
End;

Procedure TVideoGridLayout.SetColCount(AValue: Integer);
Begin
  If AValue < 1 Then
    AValue := 1;

  FColCount := AValue;
End;

Procedure TVideoGridLayout.GetCellPosition(AIndex: Integer; Out ARow, ACol: Integer);
Begin
  Case FSequence Of
    vlsLeftToRightThenDown:
    Begin
      ARow := AIndex Div FColCount;
      ACol := AIndex Mod FColCount;
    End;

    vlsTopToBottomThenRight:
    Begin
      ARow := AIndex Mod FRowCount;
      ACol := AIndex Div FRowCount;
    End;
    Else
      ARow := AIndex Div FColCount;
      ACol := AIndex Mod FColCount;
  End;
End;

Procedure TVideoGridLayout.LayoutVideos(AVideos: TfmeVideoBaseList);
Var
  i: Integer;
  Row: Integer;
  Col: Integer;
  CellW: Integer;
  CellH: Integer;
  LeftPos: Integer;
  TopPos: Integer;
  WorkW: Integer;
  WorkH: Integer;
  Video: TfmeVideoBase;
Begin
  If Not Assigned(FParent) Then
    Exit;

  If Not Assigned(AVideos) Then
    Exit;

  If (FRowCount < 1) Or (FColCount < 1) Then
    Exit;

  WorkW :=
    FParent.ClientWidth - (FPanelMargin * 2) - (FCellSpacing * (FColCount - 1));

  WorkH :=
    FParent.ClientHeight - (FPanelMargin * 2) - (FCellSpacing * (FRowCount - 1));

  If (WorkW <= 0) Or (WorkH <= 0) Then
    Exit;

  CellW := WorkW Div FColCount;
  CellH := WorkH Div FRowCount;

  For i := 0 To AVideos.Count - 1 Do
  Begin
    If i >= FRowCount * FColCount Then
      Break;

    Video := AVideos[i];

    If Not Assigned(Video) Then
      Continue;

    GetCellPosition(i, Row, Col);

    LeftPos := FPanelMargin + (Col * (CellW + FCellSpacing));
    TopPos := FPanelMargin + (Row * (CellH + FCellSpacing));

    Video.Parent := FParent;
    Video.Align := alNone;
    Video.SetBounds(LeftPos, TopPos, CellW, CellH);
    Video.Visible := True;
  End;
End;

Procedure TVideoGridLayout.ClearParent;
Var
  i: Integer;
Begin
  If Not Assigned(FParent) Then
    Exit;

  For i := FParent.ControlCount - 1 Downto 0 Do
    FParent.Controls[i].Parent := nil;
End;

End.
