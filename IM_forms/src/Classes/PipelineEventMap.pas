Unit PipelineEventMap;

{$mode objfpc}{$H+}

{-------------------------------------------------------------------------------
  Package   : IM_forms
  Unit      : PipelineEventMap.pas
  Description
    Visual Component to display KP based data on a Pipeline

    This is a clean room implementation by ChatGPT 5.5 based on
    sections of an existing Interface only.

  Source
    Copyright (c) 2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    ~1997: Original unit - developed at home
    2026-06-19: Clean room implementation added to InspectorMike package structure

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
  Classes, SysUtils, Controls, Graphics, Types, Math;

Type
  TPipelineDisplayType = (pdStartEnd, pdStartLength);

  TPipelineEventRange = Class
  Public
    StartKP: Extended;
    EndKP: Extended;
  End;

  TPipelineEventRow = Class
  Public
    Title: String;
    Ranges: TFPList;

    Constructor Create(Const ATitle: String);
    Destructor Destroy; Override;
  End;

  { TPipelineEventMap }

  TPipelineEventMap = Class(TCustomControl)
  Private
    FRows: TFPList;
    FUpdateCount: Integer;
    FBackBuffer: TBitmap;

    FGraphMode: TPipelineDisplayType;

    FStartValue: Extended;
    FEndValue: Extended;
    FDataMinValue: Extended;
    FDataMaxValue: Extended;

    FRowLines: Boolean;
    FColLines: Boolean;
    FRowStyle: TPenStyle;
    FColStyle: TPenStyle;

    FShowScale: Boolean;
    FShowTitles: Boolean;
    FShowMidpoint: Boolean;

    FTitleWidth: Integer;
    FEventCount: Integer;

    Procedure SetStartValue(AValue: Extended);
    Procedure SetEndValue(AValue: Extended);
    Function GetMidValue: Extended;
    Procedure SetMidValue(AValue: Extended);
    Procedure SetShowTitles(AValue: Boolean);
    Procedure SetShowMidpoint(AValue: Boolean);

    Procedure FreeRows;
    Procedure RebuildBuffer;
    Procedure CalculateRange;

    Function FindOrCreateRow(Const ATitle: String): TPipelineEventRow;
    Function GraphRect: TRect;
    Function ScaleHeight: Integer;
    Function RowHeight: Integer;
    Function ValueToX(AValue: Extended): Integer;
    Function XToValue(AX: Integer): Extended;
    Function RowAtY(AY: Integer): TPipelineEventRow;

  Protected
    Procedure Paint; Override;
    Procedure Resize; Override;

  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure BeginUpdate;
    Procedure AddData(ATitle: String; AStart, AEnd: Extended);
    Procedure EndUpdate;
    Procedure Clear;

    Function Count: Integer;
    Function RowCount: Integer;

    Function XYToTitle(AX, AY: Integer): String;
    Function GetKP(AX, AY: Integer): Extended;

    Procedure ZoomIn(AX, AY, APercent: Integer);
    Procedure ZoomOut(AX, AY, APercent: Integer);

    Property DataMinValue: Extended Read FDataMinValue;
    Property DataMaxValue: Extended Read FDataMaxValue;

    Procedure CopyToClipboard;
    Procedure SaveToFile(Const AFilename: String);
  Published
    Property Align;
    Property Anchors;
    Property Color;
    Property Font;
    Property ParentColor;
    Property ParentFont;
    Property Visible;

    Property GraphMode: TPipelineDisplayType Read FGraphMode Write FGraphMode Default pdStartEnd;

    Property RowLines: Boolean Read FRowLines Write FRowLines Default True;
    Property ColLines: Boolean Read FColLines Write FColLines Default True;

    Property StartValue: Extended Read FStartValue Write SetStartValue;
    Property EndValue: Extended Read FEndValue Write SetEndValue;
    Property MidValue: Extended Read GetMidValue Write SetMidValue;

    Property RowStyle: TPenStyle Read FRowStyle Write FRowStyle Default psSolid;
    Property ColStyle: TPenStyle Read FColStyle Write FColStyle Default psSolid;

    Property ShowScale: Boolean Read FShowScale Write FShowScale Default True;
    Property ShowTitles: Boolean Read FShowTitles Write SetShowTitles Default True;
    Property ShowMidpoint: Boolean Read FShowMidpoint Write SetShowMidpoint Default True;

    Property OnClick;
    Property OnDblClick;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;
  End;

Implementation

Uses
  Clipbrd;

Const
  TITLE_PADDING = 12;
  MIN_ROW_HEIGHT = 18;
  SCALE_HEIGHT_PIXELS = 22;
  MIN_VISIBLE_RANGE = 0.000001;

Constructor TPipelineEventRow.Create(Const ATitle: String);
Begin
  Inherited Create;
  Title := ATitle;
  Ranges := TFPList.Create;
End;

Destructor TPipelineEventRow.Destroy;
Var
  i: Integer;
Begin
  For i := 0 To Ranges.Count - 1 Do
    TObject(Ranges[i]).Free;

  Ranges.Free;
  Inherited Destroy;
End;

constructor TPipelineEventMap.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];

  FRows := TFPList.Create;
  FBackBuffer := TBitmap.Create;

  Width := 600;
  Height := 300;
  Color := clWindow;

  FGraphMode := pdStartEnd;
  FStartValue := 0;
  FEndValue := 100;
  FDataMinValue := 0;
  FDataMaxValue := 0;

  FRowLines := True;
  FColLines := True;
  FRowStyle := psSolid;
  FColStyle := psSolid;

  FShowScale := True;
  FShowTitles := True;
  FShowMidpoint := True;

  FTitleWidth := 0;
  FEventCount := 0;
End;

destructor TPipelineEventMap.Destroy;
Begin
  FreeRows;
  FRows.Free;
  FBackBuffer.Free;
  Inherited Destroy;
End;

procedure TPipelineEventMap.FreeRows;
Var
  i: Integer;
Begin
  For i := 0 To FRows.Count - 1 Do
    TObject(FRows[i]).Free;

  FRows.Clear;
  FEventCount := 0;
End;

procedure TPipelineEventMap.BeginUpdate;
Begin
  Inc(FUpdateCount);
End;

procedure TPipelineEventMap.EndUpdate;
Begin
  If FUpdateCount > 0 Then
    Dec(FUpdateCount);

  If FUpdateCount = 0 Then
  Begin
    CalculateRange;
    RebuildBuffer;
    Invalidate;
  End;
End;

procedure TPipelineEventMap.Clear;
Begin
  FreeRows;
  FDataMinValue := 0;
  FDataMaxValue := 0;
  FStartValue := 0;
  FEndValue := 100;

  If FUpdateCount = 0 Then
  Begin
    RebuildBuffer;
    Invalidate;
  End;
End;

function TPipelineEventMap.FindOrCreateRow(const ATitle: String
  ): TPipelineEventRow;
Var
  i: Integer;
Begin
  For i := 0 To FRows.Count - 1 Do
  Begin
    Result := TPipelineEventRow(FRows[i]);
    If SameText(Result.Title, ATitle) Then
      Exit;
  End;

  Result := TPipelineEventRow.Create(ATitle);
  FRows.Add(Result);
End;

procedure TPipelineEventMap.AddData(ATitle: String; AStart, AEnd: Extended);
Var
  Row: TPipelineEventRow;
  Range: TPipelineEventRange;
  Temp: Extended;
Begin
  Row := FindOrCreateRow(ATitle);

  Range := TPipelineEventRange.Create;
  Range.StartKP := AStart;

  Case FGraphMode Of
    pdStartEnd:
      Range.EndKP := AEnd;

    pdStartLength:
      Range.EndKP := AStart + AEnd;
  End;

  If Range.EndKP < Range.StartKP Then
  Begin
    Temp := Range.StartKP;
    Range.StartKP := Range.EndKP;
    Range.EndKP := Temp;
  End;

  Row.Ranges.Add(Range);
  Inc(FEventCount);

  If FUpdateCount = 0 Then
  Begin
    CalculateRange;
    RebuildBuffer;
    Invalidate;
  End;
End;

function TPipelineEventMap.Count: Integer;
Begin
  Result := FEventCount;
End;

function TPipelineEventMap.RowCount: Integer;
Begin
  Result := FRows.Count;
End;

procedure TPipelineEventMap.CalculateRange;
Var
  i, j: Integer;
  Row: TPipelineEventRow;
  Range: TPipelineEventRange;
  MinKP, MaxKP: Extended;
  First: Boolean;
Begin
  First := True;
  MinKP := 0;
  MaxKP := 0;

  For i := 0 To FRows.Count - 1 Do
  Begin
    Row := TPipelineEventRow(FRows[i]);

    For j := 0 To Row.Ranges.Count - 1 Do
    Begin
      Range := TPipelineEventRange(Row.Ranges[j]);

      If First Then
      Begin
        MinKP := Range.StartKP;
        MaxKP := Range.EndKP;
        First := False;
      End
      Else
      Begin
        If Range.StartKP < MinKP Then
          MinKP := Range.StartKP;
        If Range.EndKP > MaxKP Then
          MaxKP := Range.EndKP;
      End;
    End;
  End;

  If First Then
    Exit;

  If SameValue(MinKP, MaxKP) Then
    MaxKP := MinKP + MIN_VISIBLE_RANGE;

  FDataMinValue := MinKP;
  FDataMaxValue := MaxKP;
  FStartValue := MinKP;
  FEndValue := MaxKP;
End;

procedure TPipelineEventMap.SetStartValue(AValue: Extended);
Begin
  If SameValue(FStartValue, AValue) Then
    Exit;

  FStartValue := AValue;

  If FEndValue <= FStartValue Then
    FEndValue := FStartValue + MIN_VISIBLE_RANGE;

  If FUpdateCount = 0 Then
  Begin
    RebuildBuffer;
    Invalidate;
  End;
End;

procedure TPipelineEventMap.SetEndValue(AValue: Extended);
Begin
  If SameValue(FEndValue, AValue) Then
    Exit;

  FEndValue := AValue;

  If FEndValue <= FStartValue Then
    FStartValue := FEndValue - MIN_VISIBLE_RANGE;

  If FUpdateCount = 0 Then
  Begin
    RebuildBuffer;
    Invalidate;
  End;
End;

function TPipelineEventMap.GetMidValue: Extended;
Begin
  Result := (FStartValue + FEndValue) / 2;
End;

procedure TPipelineEventMap.SetMidValue(AValue: Extended);
Var
  HalfRange: Extended;
Begin
  HalfRange := (FEndValue - FStartValue) / 2;
  FStartValue := AValue - HalfRange;
  FEndValue := AValue + HalfRange;

  If FUpdateCount = 0 Then
  Begin
    RebuildBuffer;
    Invalidate;
  End;
End;

procedure TPipelineEventMap.SetShowTitles(AValue: Boolean);
Begin
  If FShowTitles = AValue Then
    Exit;

  FShowTitles := AValue;
  If FUpdateCount = 0 Then
  Begin
    RebuildBuffer;
    Invalidate;
  End;
End;

procedure TPipelineEventMap.SetShowMidpoint(AValue: Boolean);
Begin
  If FShowMidpoint = AValue Then
    Exit;

  FShowMidpoint := AValue;
  If FUpdateCount = 0 Then
  Begin
    RebuildBuffer;
    Invalidate;
  End;
End;

function TPipelineEventMap.ScaleHeight: Integer;
Begin
  If FShowScale Then
    Result := SCALE_HEIGHT_PIXELS
  Else
    Result := 0;
End;

function TPipelineEventMap.RowHeight: Integer;
Var
  AvailableHeight: Integer;
Begin
  AvailableHeight := ClientHeight - ScaleHeight;

  If FRows.Count = 0 Then
    Result := MIN_ROW_HEIGHT
  Else
    Result := Max(MIN_ROW_HEIGHT, AvailableHeight Div FRows.Count);
End;

function TPipelineEventMap.GraphRect: TRect;
Begin
  Result := Rect(FTitleWidth, ScaleHeight, ClientWidth, ClientHeight);
End;

function TPipelineEventMap.ValueToX(AValue: Extended): Integer;
Var
  R: TRect;
  Fraction: Extended;
Begin
  R := GraphRect;

  If SameValue(FEndValue, FStartValue) Then
    Exit(R.Left);

  Fraction := (AValue - FStartValue) / (FEndValue - FStartValue);
  Result := R.Left + Round(Fraction * (R.Right - R.Left));
End;

function TPipelineEventMap.XToValue(AX: Integer): Extended;
Var
  R: TRect;
  Fraction: Extended;
Begin
  R := GraphRect;

  If R.Right = R.Left Then
    Exit(FStartValue);

  Fraction := (AX - R.Left) / (R.Right - R.Left);
  Result := FStartValue + Fraction * (FEndValue - FStartValue);
End;

function TPipelineEventMap.RowAtY(AY: Integer): TPipelineEventRow;
Var
  Index: Integer;
Begin
  Result := nil;

  If AY < ScaleHeight Then
    Exit;

  Index := (AY - ScaleHeight) Div RowHeight;

  If (Index >= 0) And (Index < FRows.Count) Then
    Result := TPipelineEventRow(FRows[Index]);
End;

function TPipelineEventMap.XYToTitle(AX, AY: Integer): String;
Var
  Row: TPipelineEventRow;
Begin
  Result := '';

  Row := RowAtY(AY);
  If Assigned(Row) Then
    Result := Row.Title;
End;

function TPipelineEventMap.GetKP(AX, AY: Integer): Extended;
Var
  GR: TRect;
Begin
  GR := GraphRect;

  If (AX < GR.Left) Or (AX > GR.Right) Or (AY < GR.Top) Or (AY > GR.Bottom) Then
    Exit(-1);

  Result := XToValue(AX);
End;

procedure TPipelineEventMap.ZoomIn(AX, AY, APercent: Integer);
Var
  Centre: Extended;
  Range: Extended;
  NewRange: Extended;
Begin
  If APercent <= 0 Then
    Exit;

  Centre := XToValue(AX);
  Range := FEndValue - FStartValue;
  NewRange := Range * (100 - APercent) / 100;

  If NewRange < MIN_VISIBLE_RANGE Then
    NewRange := MIN_VISIBLE_RANGE;

  FStartValue := Centre - NewRange / 2;
  FEndValue := Centre + NewRange / 2;

  RebuildBuffer;
  Invalidate;
End;

procedure TPipelineEventMap.ZoomOut(AX, AY, APercent: Integer);
Var
  Centre: Extended;
  Range: Extended;
  NewRange: Extended;
Begin
  If APercent <= 0 Then
    Exit;

  Centre := XToValue(AX);
  Range := FEndValue - FStartValue;
  NewRange := Range * (100 + APercent) / 100;

  FStartValue := Centre - NewRange / 2;
  FEndValue := Centre + NewRange / 2;

  RebuildBuffer;
  Invalidate;
End;

procedure TPipelineEventMap.CopyToClipboard;
begin
  RebuildBuffer;
  Clipboard.Assign(FBackBuffer);
end;

procedure TPipelineEventMap.SaveToFile(const AFilename: String);
var
  PNG: TPortableNetworkGraphic;
begin
  RebuildBuffer;

  PNG := TPortableNetworkGraphic.Create;
  try
    PNG.Assign(FBackBuffer);
    PNG.SaveToFile(AFilename);
  finally
    PNG.Free;
  end;
end;

procedure TPipelineEventMap.RebuildBuffer;
Var
  i, j: Integer;
  Row: TPipelineEventRow;
  Range: TPipelineEventRange;
  R, GR: TRect;
  Y1, Y2: Integer;
  X1, X2: Integer;
  TextY: Integer;
  MaxTitleWidth: Integer;
  Tick: Integer;
  TickValue: Extended;
  S: String;
  TextW: Integer;
  TempX: Integer;
Begin
  If (ClientWidth <= 0) Or (ClientHeight <= 0) Then
    Exit;

  FBackBuffer.SetSize(ClientWidth, ClientHeight);

  FBackBuffer.Canvas.Brush.Style := bsSolid;
  FBackBuffer.Canvas.Brush.Color := Color;
  FBackBuffer.Canvas.FillRect(Rect(0, 0, ClientWidth, ClientHeight));
  FBackBuffer.Canvas.Font.Assign(Font);

  If FShowTitles Then
  Begin
    MaxTitleWidth := 0;

    For i := 0 To FRows.Count - 1 Do
    Begin
      Row := TPipelineEventRow(FRows[i]);
      MaxTitleWidth := Max(MaxTitleWidth, FBackBuffer.Canvas.TextWidth(Row.Title) +
        TITLE_PADDING);
    End;

    FTitleWidth := MaxTitleWidth;
  End
  Else
    FTitleWidth := 0;

  GR := GraphRect;

  If FShowScale Then
  Begin
    FBackBuffer.Canvas.Pen.Style := psSolid;
    FBackBuffer.Canvas.Pen.Color := clBtnShadow;
    FBackBuffer.Canvas.Line(0, ScaleHeight - 1, ClientWidth, ScaleHeight - 1);

    FBackBuffer.Canvas.Brush.Style := bsClear;
    FBackBuffer.Canvas.Font.Color := clWindowText;

    For Tick := 0 To 10 Do
    Begin
      TickValue := FStartValue + ((FEndValue - FStartValue) * Tick / 10);
      X1 := ValueToX(TickValue);

      FBackBuffer.Canvas.Pen.Style := psSolid;
      FBackBuffer.Canvas.Pen.Color := clBtnShadow;
      FBackBuffer.Canvas.Line(X1, ScaleHeight - 5, X1, ScaleHeight);

      S := FormatFloat('0.###', TickValue);
      TextW := FBackBuffer.Canvas.TextWidth(S);

      If Tick = 0 Then
        FBackBuffer.Canvas.TextOut(GR.Left + 2, 2, S)
      Else If Tick = 10 Then
        FBackBuffer.Canvas.TextOut(GR.Right - TextW - 2, 2, S)
      Else
        FBackBuffer.Canvas.TextOut(X1 - (TextW Div 2), 2, S);
    End;

    FBackBuffer.Canvas.Brush.Style := bsSolid;
  End;

  If FShowTitles Then
  Begin
    FBackBuffer.Canvas.Pen.Color := clBtnShadow;
    FBackBuffer.Canvas.Pen.Style := psSolid;
    FBackBuffer.Canvas.Line(FTitleWidth - 1, ScaleHeight,
      FTitleWidth - 1, ClientHeight);
  End;

  For i := 0 To FRows.Count - 1 Do
  Begin
    Row := TPipelineEventRow(FRows[i]);

    Y1 := ScaleHeight + i * RowHeight;
    Y2 := Y1 + RowHeight;

    If Y1 > ClientHeight Then
      Break;

    If FShowTitles Then
    Begin
      TextY := Y1 + ((RowHeight - FBackBuffer.Canvas.TextHeight(Row.Title)) Div 2);

      FBackBuffer.Canvas.Brush.Style := bsClear;
      FBackBuffer.Canvas.Font.Color := clWindowText;
      FBackBuffer.Canvas.TextOut(4, TextY, Row.Title);
      FBackBuffer.Canvas.Brush.Style := bsSolid;
    End;

    For j := 0 To Row.Ranges.Count - 1 Do
    Begin
      Range := TPipelineEventRange(Row.Ranges[j]);

      X1 := ValueToX(Range.StartKP);
      X2 := ValueToX(Range.EndKP);

      X1 := EnsureRange(X1, GR.Left, GR.Right);
      X2 := EnsureRange(X2, GR.Left, GR.Right);

      If X2 < X1 Then
      Begin
        TempX := X1;
        X1 := X2;
        X2 := TempX;
      End;

      R := Rect(X1, Y1 + 3, X2, Y2 - 3);

      If R.Right <= R.Left Then
        R.Right := R.Left + 1;

      FBackBuffer.Canvas.Brush.Style := bsSolid;
      FBackBuffer.Canvas.Brush.Color := clHighlight;
      FBackBuffer.Canvas.FillRect(R);
    End;

    If FRowLines Then
    Begin
      FBackBuffer.Canvas.Pen.Color := clBtnShadow;
      FBackBuffer.Canvas.Pen.Style := FRowStyle;
      FBackBuffer.Canvas.Line(0, Y2, ClientWidth, Y2);
    End;
  End;

  If FColLines Then
  Begin
    FBackBuffer.Canvas.Pen.Color := clBtnShadow;
    FBackBuffer.Canvas.Pen.Style := FColStyle;

    For Tick := 0 To 10 Do
    Begin
      TickValue := FStartValue + ((FEndValue - FStartValue) * Tick / 10);
      X1 := ValueToX(TickValue);
      FBackBuffer.Canvas.Line(X1, ScaleHeight, X1, ClientHeight);
    End;
  End;

  If FShowMidpoint Then
  Begin
    X1 := ValueToX(MidValue);
    FBackBuffer.Canvas.Pen.Width := 2;
    FBackBuffer.Canvas.Pen.Color := clRed;
    FBackBuffer.Canvas.Pen.Style := psSolid;
    FBackBuffer.Canvas.Line(X1, ScaleHeight, X1, ClientHeight);
    FBackBuffer.Canvas.Pen.Width := 1;
  End;
End;

procedure TPipelineEventMap.Paint;
Begin
  If (FBackBuffer.Width <> ClientWidth) Or (FBackBuffer.Height <> ClientHeight) Then
    RebuildBuffer;

  Canvas.Draw(0, 0, FBackBuffer);
End;

procedure TPipelineEventMap.Resize;
Begin
  Inherited Resize;
  RebuildBuffer;
End;

End.
