Unit ControlGridLayout;

{-------------------------------------------------------------------------------
  Package   : IM.forms
  Unit      : ControlGridLayout.pas
  Description
    Layout manager developed for arranging multiple video playback frames within
    a parent control, but which can work with any controls.

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
    2026-06-19: Refactored into split InspectorMike package structure
    2026-06-28: Added Extend (extend into unused cells in final row/col)

  License
    This file is part of IM.forms.lpk.

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
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
Interface

Uses
  Classes, SysUtils, Controls, ExtCtrls, ControlsSupport;

Type
  TControlLayoutSequence = (
    clsLeftToRightThenDown,
    clsTopToBottomThenRight
    );

  { TControlGridLayout }

  TControlGridLayout = Class
  Private
    FParent: TWinControl;
    FRowCount: Integer;
    FColCount: Integer;
    FSequence: TControlLayoutSequence;
    FPanelMargin: Integer;
    FCellSpacing: Integer;
    FExtend: Boolean;

    Procedure SetRowCount(AValue: Integer);
    Procedure SetColCount(AValue: Integer);

    Procedure GetCellPosition(AIndex: Integer; Out ARow, ACol: Integer);

  Public
    Constructor Create(AParent: TWinControl);

    Procedure LayoutControls(AControls: TControlList; AControlCountLimit: Integer = -1);

    Property Parent: TWinControl Read FParent Write FParent;

    Property RowCount: Integer Read FRowCount Write SetRowCount;
    Property ColCount: Integer Read FColCount Write SetColCount;

    Property Sequence: TControlLayoutSequence Read FSequence Write FSequence;

    Property PanelMargin: Integer Read FPanelMargin Write FPanelMargin;
    Property CellSpacing: Integer Read FCellSpacing Write FCellSpacing;
    Property Extend: Boolean Read FExtend Write FExtend;
  End;

Implementation

Uses
  Math;

{ TControlGridLayout }

Constructor TControlGridLayout.Create(AParent: TWinControl);
Begin
  Inherited Create;

  FParent := AParent;
  FRowCount := 2;
  FColCount := 2;
  FExtend := True;

  FSequence := clsLeftToRightThenDown;

  FPanelMargin := 0;
  FCellSpacing := 4;
End;

Procedure TControlGridLayout.SetRowCount(AValue: Integer);
Begin
  If AValue < 1 Then
    AValue := 1;

  FRowCount := AValue;
End;

Procedure TControlGridLayout.SetColCount(AValue: Integer);
Begin
  If AValue < 1 Then
    AValue := 1;

  FColCount := AValue;
End;

Procedure TControlGridLayout.GetCellPosition(AIndex: Integer; Out ARow, ACol: Integer);
Begin
  Case FSequence Of
    clsLeftToRightThenDown:
    Begin
      ARow := AIndex Div FColCount;
      ACol := AIndex Mod FColCount;
    End;

    clsTopToBottomThenRight:
    Begin
      ARow := AIndex Mod FRowCount;
      ACol := AIndex Div FRowCount;
    End;
    Else
      ARow := AIndex Div FColCount;
      ACol := AIndex Mod FColCount;
  End;
End;

Procedure TControlGridLayout.LayoutControls(AControls: TControlList;
  AControlCountLimit: Integer = -1);
Var
  i: Integer;
  Row: Integer;
  Col: Integer;

  CellW: Integer;
  CellH: Integer;

  GridColCount: Integer;
  GridRowCount: Integer;

  LayoutCellW: Integer;
  LayoutCellH: Integer;
  LayoutColCount: Integer;
  LayoutRowCount: Integer;

  LastRow: Integer;
  LastCol: Integer;
  ItemsInLastRow: Integer;
  ItemsInLastCol: Integer;

  LeftPos: Integer;
  TopPos: Integer;
  WorkW: Integer;
  WorkH: Integer;

  oControl: TControl;
Begin
  If Not Assigned(FParent) Then
    Exit;

  If Not Assigned(AControls) Then
    Exit;

  If (FRowCount < 1) Or (FColCount < 1) Then
    Exit;

  If (AControlCountLimit < 0) Or (AControlCountLimit > AControls.Count) Then
    AControlCountLimit := AControls.Count;

  If AControlCountLimit = 0 Then
    Exit;

  { -----------------------------------------------------------
    Determine the actual grid required.

    FRowCount/FColCount remain the user's preferred settings.
    ----------------------------------------------------------- }

  Case FSequence Of

    clsLeftToRightThenDown:
    Begin
      { Preserve preferred column count, but don't create
        columns which cannot possibly be used. }
      GridColCount := Min(FColCount, AControlCountLimit);

      { Ceiling division }
      GridRowCount := (AControlCountLimit + GridColCount - 1) Div GridColCount;
    End;


    clsTopToBottomThenRight:
    Begin
      { Preserve preferred row count, but don't create
        rows which cannot possibly be used. }
      GridRowCount := Min(FRowCount, AControlCountLimit);

      { Ceiling division }
      GridColCount := (AControlCountLimit + GridRowCount - 1) Div GridRowCount;
    End;
    Else
      Exit;
  End;

  WorkW := FParent.ClientWidth - (FPanelMargin * 2) - (FCellSpacing * (GridColCount - 1));

  WorkH := FParent.ClientHeight - (FPanelMargin * 2) - (FCellSpacing * (GridRowCount - 1));

  If (WorkW <= 0) Or (WorkH <= 0) Then
    Exit;

  CellW := WorkW Div GridColCount;
  CellH := WorkH Div GridRowCount;


  For i := 0 To AControlCountLimit - 1 Do
  Begin
    If AControls[i] = nil Then
      Raise Exception.CreateFmt('Item %d is nil', [i]);

    If Not (TObject(AControls[i]) Is TControl) Then
      Raise Exception.CreateFmt('Item %d is %s, not TControl',
        [i, TObject(AControls[i]).ClassName]);

    oControl := TControl(AControls[i]);

    { Calculate position using the effective grid rather
      than FRowCount/FColCount. }

    Case FSequence Of

      clsLeftToRightThenDown:
      Begin
        Row := i Div GridColCount;
        Col := i Mod GridColCount;
      End;

      clsTopToBottomThenRight:
      Begin
        Col := i Div GridRowCount;
        Row := i Mod GridRowCount;
      End;

    End;

    LayoutCellW := CellW;
    LayoutCellH := CellH;
    LayoutColCount := GridColCount;
    LayoutRowCount := GridRowCount;

    If FExtend Then
    Begin
      Case FSequence Of

        clsLeftToRightThenDown:
        Begin
          ItemsInLastRow := AControlCountLimit Mod GridColCount;

          If ItemsInLastRow <> 0 Then
          Begin
            LastRow := AControlCountLimit Div GridColCount;

            If Row = LastRow Then
            Begin
              LayoutColCount := ItemsInLastRow;

              LayoutCellW := (FParent.ClientWidth - (FPanelMargin * 2) -
                (FCellSpacing * (LayoutColCount - 1))) Div LayoutColCount;

              { Position within the extended row }
              Col := i Mod GridColCount;
            End;
          End;
        End;

        clsTopToBottomThenRight:
        Begin
          ItemsInLastCol := AControlCountLimit Mod GridRowCount;

          If ItemsInLastCol <> 0 Then
          Begin
            LastCol := AControlCountLimit Div GridRowCount;

            If Col = LastCol Then
            Begin
              LayoutRowCount := ItemsInLastCol;

              LayoutCellH := (FParent.ClientHeight - (FPanelMargin * 2) -
                (FCellSpacing * (LayoutRowCount - 1))) Div LayoutRowCount;

              { Position within the extended column }
              Row := i Mod GridRowCount;
            End;
          End;
        End;

      End;
    End;

    LeftPos := FPanelMargin + (Col * (LayoutCellW + FCellSpacing));

    TopPos := FPanelMargin + (Row * (LayoutCellH + FCellSpacing));

    oControl.Parent := FParent;
    oControl.Align := alNone;

    oControl.SetBounds(LeftPos, TopPos, LayoutCellW, LayoutCellH);

    oControl.Visible := True;
  End;
End;

End.
