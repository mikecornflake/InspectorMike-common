Unit PipelineDisplay;

{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : PipelineDisplay.pas
  Description
    Visual Component to display KP based data on a Pipeline
    Based on code from Netlink

  Source
    Copyright (c) 2025
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    ~1997: Original unit - developed at home
    2019-04-05: Uploaded to SourceForge/Package "Shared"
                - Part of InspectionSQLReports Pipeline Mode
    2019-12-03: Last commit in SourceForge
    2024-01-22: Migrated to Github.  Refactored package to "IM_units"
    2025-11-29: Added this header

  License
    This file is part of IM_units.lpk.

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

Uses Windows, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Menus, ComCtrls, fgl;

Type
  TPipelineDisplayType = (pdStartEnd, pdStartLength);

  TPipelineDisplayDataItem = Class
    StartPos, EndPos: Extended;
    Title: String;
    Color: TColor;
  End;

  TPipelineDisplayData = Specialize TFPGObjectList<TPipelineDisplayDataItem>;
  TColorList = Specialize TFPGList<TColor>;

  { TPipelineDisplay }

  TPipelineDisplay = Class(TCustomControl)
  Private
    FChart: TPaintBox;
    FShowMidpoint: Boolean;
    FTitleBox: TPaintBox;

    FUpdating: Integer;
    FGraphMode: TPipelineDisplayType;

    FData: TPipelineDisplayData;

    FMaxVal: Extended;

    FMaxSet: Integer;
    FMaxLen: Integer;
    FTopH: Integer;
    FXS, FYS: Extended;

    FTitles: TStringList;
    FColors: TColorList;
    FLightColors: TColorList;

    FRowLines: Boolean;
    FRowStyle: TPenStyle;
    FColLines: Boolean;
    FColStyle: TPenStyle;
    FColScale: Single;

    FShowScale: Boolean;
    FShowTitles: Boolean;

    FStartValue: Extended;
    FEndValue: Extended;

    FColorDialog: TColorDialog;

    FAutoSizing: Boolean;

    Function GetMidValue: Extended;
    Procedure MyMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    Procedure MyMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure SetAutoSizing(Value: Boolean);
    Procedure SetMidValue(AValue: Extended);
    Procedure SetStartValue(Value: Extended);
    Procedure SetEndValue(Value: Extended);
    Procedure SetColScale(Value: Single);
    Procedure SetShowTitles(Value: Boolean);

    Function ColorLighten(cColor: TColor): TColor;
  Protected
    Procedure UpdateData; Virtual;

    Procedure FlushGraph(Sender: TObject);
    Procedure DrawCanvas(ACanvas: TCanvas; AWidth, AHeight: Integer);

    Procedure Paint; Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Procedure Clear;

    Procedure BeginUpdate;
    Procedure AddData(ATitle: String; AStart, AEnd: Extended);
    Procedure EndUpdate;

    Procedure DrawBitmap(Bitmap: TBitmap);

    Function XYToTitle(AX, AY: Integer): String;
    Function GetKP(AX, AY: Integer): Extended;
    Procedure ZoomIn(AX, AY, APercent: Integer);
    Procedure ZoomOut(AX, AY, APercent: Integer);

    Procedure ChangeColor(AX, AY: Integer);
  Published
    Property Align Default alClient;

    Property RowLines: Boolean read FRowLines write FRowLines Default True;
    Property ColLines: Boolean read FColLines write FColLines Default True;
    Property ColScale: Single read FColScale write SetColScale;

    Property StartValue: Extended read FStartValue write SetStartValue;
    Property EndValue: Extended read FEndValue write SetEndValue;

    Property MidValue: Extended read GetMidValue write SetMidValue;

    Property RowStyle: TPenStyle read FRowStyle write FRowStyle Default psSolid;
    Property ColStyle: TPenStyle read FColStyle write FColStyle Default psSolid;

    Property ShowScale: Boolean read FShowScale write FShowScale Default True;
    Property ShowTitles: Boolean read FShowTitles write SetShowTitles Default True;
    Property ShowMidpoint: Boolean read FShowMidpoint write FShowMidPoint Default True;

    Property GraphMode: TPipelineDisplayType read FGraphMode write FGraphMode;

    Property AutoSizing: Boolean read FAutoSizing write SetAutoSizing;

    Property PopupMenu;
    Property OnMouseDown;
    Property OnMouseMove;
  End;

Implementation

Uses
  Math;

Const
  StandardCount = 10;
  StandardColors: Array[0..StandardCount - 1] Of TColor =
    (clNavy, clPurple, clGreen, clMaroon, clOlive, clTeal, $000065CA, $005F9700,
    $00824100, $0082004E);

Procedure SwapExtended(Var One, Two: Extended);
Var
  Temp: Extended;
Begin
  Temp := One;
  One := Two;
  Two := Temp;
End;

Constructor TPipelineDisplay.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);

  FData := TPipelineDisplayData.Create;
  FData.FreeObjects := True;

  FTitles := TStringList.Create;
  FTitles.Sorted := True;
  FTitles.Duplicates := dupIgnore;

  FColors := TColorList.Create;
  FLightColors := TColorList.Create;
  Color := clWhite;

  FTitleBox := TPaintBox.Create(Self);
  FTitleBox.Parent := Self;
  FTitleBox.Align := alLeft;
  FTitleBox.OnPaint := nil;

  FChart := TPaintBox.Create(Self);
  FChart.Parent := Self;
  FChart.Align := alClient;

  FChart.OnMouseDown := @MyMouseDown;
  FChart.OnMouseMove := @MyMouseMove;

  Align := alClient;
  RowLines := True;
  ColLines := True;

  RowStyle := psSolid;
  ColStyle := psDot;

  FShowScale := True;
  FShowTitles := True;
  FShowMidPoint := True;

  FUpdating := 0;
End;

Procedure TPipelineDisplay.FlushGraph(Sender: TObject);
Begin
  If Not (csDestroying In ComponentState) Then
  Begin
    FData.Clear;
    FTitles.Clear;
    If FUpdating = 0 Then
      Refresh;
  End;
End;

Destructor TPipelineDisplay.Destroy;
Begin
  Destroying;

  FreeAndNil(FTitles);
  FreeAndNil(FColorDialog);
  FreeAndNil(FData);
  FreeAndNil(FChart);
  FreeAndNil(FColors);
  FreeAndNil(FLightColors);
  FreeAndNil(FTitleBox);

  Inherited Destroy;
End;

Procedure TPipelineDisplay.SetAutoSizing(Value: Boolean);
Begin
  If Value <> FAutoSizing Then
  Begin
    FAutoSizing := Value;
    UpdateData;
  End;
End;

Function TPipelineDisplay.GetKP(AX, AY: Integer): Extended;
Var
  KP: Extended;
Begin
  If (AX > FTitleBox.Width) And (AX < Width) Then
  Begin
    KP := StartValue + (EndValue - StartValue) * (AX - FTitleBox.Width) / FChart.Width;
    Result := KP;
  End
  Else
    Result := -1;
End;

Procedure TPipelineDisplay.SetStartValue(Value: Extended);
Begin
  If FStartValue <> Value Then
  Begin
    FStartValue := Value;

    If FStartValue > FEndValue Then
      SwapExtended(FStartValue, FEndValue);

    If (ColScale <= 0) Or ((FEndValue - FStartValue) / ColScale > 10) Then
      ColScale := (EndValue - StartValue) / 10;

    If FUpdating = 0 Then
      Refresh;
  End;
End;

Procedure TPipelineDisplay.SetEndValue(Value: Extended);
Begin
  If FEndValue <> Value Then
  Begin
    FEndValue := Value;

    If FStartValue > FEndValue Then
      SwapExtended(FStartValue, FEndValue);

    If (ColScale <= 0) Or ((FEndValue - FStartValue) / ColScale > 10) Then
      ColScale := (FEndValue - FStartValue) / 10;

    If FUpdating = 0 Then
      Refresh;
  End;
End;

Procedure TPipelineDisplay.SetMidValue(AValue: Extended);
Var
  dCurrRange: Extended;
Begin
  dCurrRange := FEndValue - FStartValue;

  FStartValue := AValue - (dCurrRange / 2);
  FEndValue := AValue + (dCurrRange / 2);

  ColScale := (FEndValue - FStartValue) / 10;

  If FUpdating = 0 Then
    Refresh;
End;

Function TPipelineDisplay.GetMidValue: Extended;
Begin
  Result := (FStartValue + FEndValue) / 2;
End;

Procedure TPipelineDisplay.UpdateData;
Var
  i: Integer;
  dMinVal: Extended;
  dExtra: Extended;
  oTemp: TPipelineDisplayDataItem;

Begin
  dMinVal := MaxInt;
  FMaxVal := -MaxInt;

  If AutoSizing Then
  Begin
    For i := 0 To FData.Count - 1 Do
    Begin
      oTemp := FData[i];
      dMinVal := Min(dMinVal, oTemp.StartPos);
      FMaxVal := Max(FMaxVal, oTemp.EndPos);
    End;

    dExtra := (FMaxVal - dMinVal) * 0.05;
    FStartValue := dMinVal - dExtra / 2;
    FEndValue := FMaxVal + dExtra / 2;
  End;

  ColScale := (EndValue - StartValue) / 10;

  If FUpdating = 0 Then
    Refresh;
End;

Procedure TPipelineDisplay.SetColScale(Value: Single);
Begin
  If Value <= 0 Then
    Value := 1;

  If FColScale <> Value Then
  Begin
      { We need to ensure there is an upper limit of only 20 divisions }
    If (Value <= 0) Or ((FEndValue - FStartValue) / Value > 20) Then
      FColScale := (FEndValue - FStartValue) / 20
    Else
      FColScale := Value;

    If FUpdating = 0 Then
      Refresh;
  End;
End;

Procedure TPipelineDisplay.MyMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Begin
  If Assigned(OnMouseMove) Then
    OnMouseMove(Sender, Shift, X + FTitlebox.Width + 4, Y + Top);
End;

Procedure TPipelineDisplay.MyMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If Assigned(OnMouseDown) Then
    OnMouseDown(Sender, Button, Shift, X + FTitleBox.Width + 4, Y + Top);
End;

Procedure TPipelineDisplay.SetShowTitles(Value: Boolean);
Begin
  If FShowTitles <> Value Then
  Begin
    FShowTitles := Value;
    FTitleBox.Visible := Value;
  End;
End;

Procedure TPipelineDisplay.Clear;
Begin
  FData.Clear;
  FTitles.Clear;
  FMaxVal := 0;
  Refresh;
End;

Function TPipelineDisplay.XYToTitle(AX, AY: Integer): String;
Var
  i: Integer;
Begin
  If (AY > FTopH) And (AY < FChart.Height) Then
  Begin
    i := Trunc((AY - FTopH) / FYS);

    If (i < FTitles.Count) Then
      Result := FTitles[i]
    Else
      Result := IntToStr(i);
  End
  Else
    Result := '';
End;

Procedure TPipelineDisplay.ChangeColor(AX, AY: Integer);
Var
  Index: Integer;
Begin
  FColorDialog := TColorDialog.Create(nil);
  If FColorDialog.Execute Then
  Begin
    Index := Trunc((AY - FTopH) / FYS) + 1;
    If (Index - 1) >= (FColors.Count - 1) Then
      FColors.Add(FColorDialog.Color)
    Else
      FColors[Index] := FColorDialog.Color;
  End;

  FColorDialog.Free;
  FColorDialog := nil;
  FChart.Refresh;
End;

Procedure TPipelineDisplay.ZoomOut(AX, AY, APercent: Integer);
Var
  KP, LeftBoundary, RightBoundary, ZoomWidth: Extended;
Begin
  If (AX > FTitleBox.Width) And (AX < Width) Then
  Begin
    BeginUpdate;
    KP := StartValue + (EndValue - StartValue) * (AX - FTitleBox.Width) / FChart.Width;
    ZoomWidth := (EndValue - StartValue) * 100 / APercent;

      { Find out which boundaries AX is in }
    LeftBoundary := KP - ZoomWidth / 2;
    RightBoundary := LeftBoundary + ZoomWidth;

    StartValue := LeftBoundary;
    EndValue := RightBoundary;
    ColScale := (EndValue - StartValue) / 10;
    EndUpdate;
  End;
End;

Procedure TPipelineDisplay.ZoomIn(AX, AY, APercent: Integer);
Var
  KP, LeftBoundary, RightBoundary, ZoomWidth: Extended;
Begin
  If (AX > FTitleBox.Width) And (AX < Width) And (FColScale > 0.001) Then
  Begin
    BeginUpdate;
    KP := StartValue + (EndValue - StartValue) * (AX - FTitleBox.Width) / FChart.Width;
    ZoomWidth := (EndValue - StartValue) * APercent / 100;

      { Find out which boundaries AX is in }
    LeftBoundary := KP - ZoomWidth / 2;
    RightBoundary := LeftBoundary + ZoomWidth;

    StartValue := LeftBoundary;
    EndValue := RightBoundary;
    ColScale := (EndValue - StartValue) / 10;
    EndUpdate;
  End;
End;


Procedure TPipelineDisplay.DrawBitmap(Bitmap: TBitmap);
Begin
  DrawCanvas(Bitmap.Canvas, Bitmap.Width, Bitmap.Height);
End;

Procedure TPipelineDisplay.DrawCanvas(ACanvas: TCanvas; AWidth, AHeight: Integer);
Var
  i, iRow: Integer;
  dStart, dEnd: Extended;

  iRWidth, iRHeight, j: Integer;
  sTemp: String;

  rBounds, rItem: TRect;
  oItem: TPipelineDisplayDataItem;

  oColor, oLight: TColor;
  iMax, iLeft: Int64;

  Procedure DrawPipe(ARect: TRect; AColor, ALight: TColor);
  Var
    rTop, rBottom: TRect;
  Begin
    rTop := Rect(ARect.Left, ARect.Top, ARect.Right, ARect.Top +
      ((ARect.Bottom - ARect.Top) Div 2));
    rBottom := Rect(ARect.Left, rTop.Bottom, ARect.Right, ARect.Bottom);

    ACanvas.GradientFill(rTop, AColor, ALight, gdVertical);
    ACanvas.GradientFill(rBottom, ALight, AColor, gdVertical);
  End;

Begin
  rBounds := Rect(0, 0, AWidth{-32}, AHeight);

  If (FData.Count = 0) Or (FStartValue >= FEndValue) Then
    Exit;

  iRWidth := rBounds.Right - rBounds.Left;
  iRHeight := rBounds.Bottom - rBounds.Top;

  FMaxSet := FTitles.Count;
  FMaxLen := 0;

  ACanvas.Font.Name := 'Tahoma';
  ACanvas.Font.Size := 10;
  ACanvas.Pen.Color := clBlue;
//  ACanvas.Pen.Color := clBlack;

  If FTitles.Count > 0 Then
    For i := 0 To FTitles.Count - 1 Do
      If ACanvas.TextWidth(FTitles[i]) > FMaxLen Then
        FMaxLen := ACanvas.TextWidth(FTitles[i]);

  FTopH := Round(ACanvas.TextHeight('M') * 1.2);
  FMaxLen := Round(FMaxLen * 1.2);

  If FMaxVal > FEndValue Then
    FMaxVal := FEndValue;

  FColors.Clear;
  FLightColors.Clear;
  For i := 0 To FMaxSet - 1 Do
  Begin
    oColor := StandardColors[i Mod StandardCount];
    FColors.Add(oColor);
    FLightColors.Add(ColorLighten(oColor));
  End;

  FXS := (iRWidth - FMaxLen) / (FEndValue - FStartValue);
  FYS := (iRHeight - (FTopH)) / FMaxSet;

  { Clear ACanvas }
  ACanvas.Pen.Color := Color;
  ACanvas.Brush.Color := Color;
  ACanvas.Rectangle(rBounds.Left, rBounds.Top, rBounds.Right, rBounds.Bottom);

  ACanvas.MoveTo(rBounds.Left + FMaxLen, rBounds.Top + FTopH);
  ACanvas.LineTo(rBounds.Left + FMaxLen, rBounds.Top + iRHeight);

  ACanvas.MoveTo(rBounds.Left + FMaxLen, rBounds.Top + iRHeight);
  ACanvas.LineTo(rBounds.Left + iRWidth, rBounds.Top + iRHeight);

  // Draw the Events
  For i := 0 To FData.Count - 1 Do
  Begin
    oItem := FData[i];
    dStart := oItem.StartPos;
    dEnd := oItem.EndPos;

      { Modify dStart if Incident is off left of FChart }
    If (dStart < FStartValue) And (dEnd > FStartValue) Then
      dStart := FStartValue;

      {  Modify dEnd if Incident is off Right of FChart }
    If (dEnd > FEndValue) And (dStart < FEndValue) Then
      dEnd := FEndValue;

    iRow := FTitles.IndexOf(oItem.Title) + 1;
    oColor := FColors[iRow Mod FColors.Count];
    oLight := ColorLighten(oColor);

    ACanvas.Pen.Color := oColor;
    ACanvas.Brush.Color := oColor;

    If (dStart >= FStartValue) And (dEnd <= FEndValue) Then
    Begin
      rItem.Left := Round((dStart - FStartValue) * FXS) + FMaxLen + rBounds.Left;
      rItem.Top := Round((iRow - 1) * FYS) + FTopH + rBounds.Top;
      rItem.Bottom := Round((iRow) * FYS) + FTopH + rBounds.Top;

      If (dEnd - dStart) > 0 Then
        rItem.Right := Round((dEnd - FStartValue) * FXS) + FMaxLen + rBounds.Left
      Else
        rItem.Right := Round((dStart + 0.01 - FStartValue) * FXS) + FMaxLen + rBounds.Left;

      If rItem.Right <= rItem.Left Then
        rItem.Right := rItem.Left + 1;

      DrawPipe(rItem, oColor, oLight);
    End;
  End;

  ACanvas.Pen.Color := clBlack;
  ACanvas.Brush.Color := Color;
  ACanvas.Pen.Style := ColStyle;

  dStart := FXS * ColScale;
  iMax := Round((iRWidth - FMaxLen) / dStart);

  // Draw the Columns and Horizontal Scale
  ACanvas.Font.Size := 8;
  For i := 0 To iMax Do
  Begin
    If FShowScale Then
    Begin
      sTemp := Format('%.3f', [(i * ColScale) + FStartValue]);

      ACanvas.TextOut(Round((i * dStart) - (ACanvas.TextWidth(sTemp) Div 2)) +
        FMaxLen + rBounds.Left, rBounds.Top, sTemp);
    End;

    iLeft := Round(i * dStart) + FMaxLen + rBounds.Left;

    // Draw the indicator
    If FShowMidpoint And (i = iMax Div 2) Then
    Begin
      ACanvas.Pen.Style := psSolid;
      ACanvas.Pen.Color := clBlue;
      For j := 1 To 5 Do
      Begin
        ACanvas.MoveTo(iLeft - j, rBounds.Top + FTopH - 1 - j);
        ACanvas.LineTo(iLeft + j + 1, rBounds.Top + FTopH - 1 - j);
      End;
      ACanvas.Pixels[iLeft, rBounds.Top + FTopH - 1] := clBlue;
    End
    Else
    Begin
      ACanvas.Pen.Style := ColStyle;
      ACanvas.Pen.Color := clBlack;
    End;

    If ColLines Then
    Begin
      ACanvas.MoveTo(iLeft, rBounds.Top + FTopH);
      ACanvas.LineTo(iLeft, rBounds.Top + iRHeight);
    End;
  End;

  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Style := RowStyle;

  // Draw the Rows and Titles
  ACanvas.Font.Size := 10;
  For i := 0 To FMaxSet - 1 Do
  Begin
    If ShowTitles Then
      ACanvas.TextOut(((FMaxLen - ACanvas.TextWidth(FTitles[i])) Div 2) + rBounds.Left,
        Round((i * FYS) + (FYS / 2) + FTopH / 2) + rBounds.Top, FTitles[i]);

    If RowLines Then
    Begin
      ACanvas.MoveTo(rBounds.Left, Round(i * FYS) + FTopH + rBounds.Top);
      ACanvas.LineTo(rBounds.Left + iRWidth, Round(i * FYS) + FTopH + rBounds.Top);
    End;
  End;

  ACanvas.Brush.Style := bsClear;
  ACanvas.Rectangle(rBounds);
End;

Procedure TPipelineDisplay.Paint;
Begin
  //Inherited Paint;

  DrawCanvas(Canvas, Width, Height);
End;

Procedure TPipelineDisplay.BeginUpdate;
Begin
  Inc(FUpdating);
End;

Procedure TPipelineDisplay.AddData(ATitle: String; AStart, AEnd: Extended);
Var
  oTemp: TPipelineDisplayDataItem;
Begin
  oTemp := TPipelineDisplayDataItem.Create;

  oTemp.Title := ATitle;
  oTemp.StartPos := AStart;
  oTemp.EndPos := AEnd;

  FData.Add(oTemp);
  FTitles.Add(ATitle);

  If FUpdating = 0 Then
    UpdateData;
End;

Procedure TPipelineDisplay.EndUpdate;
Begin
  Dec(FUpdating);

  If FUpdating < 0 Then
    FUpdating := 0;

  If FUpdating = 0 Then
    UpdateData;
End;

Function TPipelineDisplay.ColorLighten(cColor: TColor): TColor;
Var
  R, G, B: Byte;

  Procedure IncreaseCol(Var Val: Byte);
  Begin
    If (Val <= 127) Then
      Inc(Val, 128)
    Else
      Val := 255;
  End;

Begin
  R := GetRValue(cColor);
  G := GetGValue(cColor);
  B := GetBValue(cColor);

  IncreaseCol(R);
  IncreaseCol(G);
  IncreaseCol(B);

  Result := RGB(R, G, B);
End;

End.
