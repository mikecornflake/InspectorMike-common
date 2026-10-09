Unit TimeTrackbar;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Controls, Graphics;

Type

  { TRenderer }

  TRenderer = Class
  Public
    Procedure Paint(ACanvas: TCanvas; Const ARect: TRect); Virtual; Abstract;
  End;

  { TTimeTrackbar }

  TTimeTrackbar = Class(TCustomControl)
  Private
    FDecorator: TRenderer;
    FEndDateTime: TDateTime;
    FOnChange: TNotifyEvent;
    FPosition: TDateTime;
    FStartDateTime: TDateTime;
    FBackBuffer: TBitmap;
    FDragging: Boolean;

    Function GetPositionPercent: Single;
    Procedure SeekToFraction(AFraction: Double);
    Procedure SeekToTime(AValue: TDateTime);
    Procedure MouseSeek(X: Integer);

    Procedure SetDecorator(Const AValue: TRenderer);
    Procedure SetEndDateTime(Const AValue: TDateTime);
    Procedure SetPosition(Const AValue: TDateTime);
    Procedure RebuildBuffer;
    Procedure SetPositionAsPercent(Const AValue: Single);
    Procedure SetStartDateTime(Const AValue: TDateTime);
  Protected
    Procedure DoEnter; Override;
    Procedure DoExit; Override;
    Procedure Paint; Override;
    Procedure Resize; Override;

    Procedure KeyDown(Var Key: Word; Shift: TShiftState); Override;
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); Override;
    Procedure MouseMove(Shift: TShiftState; X, Y: Integer); Override;
    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;

    Property StartDateTime: TDateTime Read FStartDateTime Write SetStartDateTime;
    Property EndDateTime: TDateTime Read FEndDateTime Write SetEndDateTime;
    Property Position: TDateTime Read FPosition Write SetPosition;
    Property PositionPercent: Single Read GetPositionPercent Write SetPositionAsPercent;

    Property Decorator: TRenderer Read FDecorator Write SetDecorator;

    Property OnChange: TNotifyEvent Read FOnChange Write FOnChange;
  End;

Implementation

Uses
  LCLType, Math, Types;

Const
  GAUGE_HEIGHT_DECORATOR = 6;
  GAUGE_HEIGHT = 12;
  THUMB_WIDTH = 10;
  THUMB_HEIGHT = 20;

  { TTimeTrackbar }

Constructor TTimeTrackbar.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);

  FBackBuffer := TBitmap.Create;
  FDecorator := nil;

  Width := 600;
  Height := 30;
  Color := clBtnFace;
  //ParentColor := True;
  Align := alBottom;

  ControlStyle := ControlStyle + [csOpaque];
  TabStop := True;

  FDragging := False;
End;

Destructor TTimeTrackbar.Destroy;
Begin
  FreeAndNil(FBackBuffer);

  Inherited Destroy;
End;

Procedure TTimeTrackbar.SetPosition(Const AValue: TDateTime);
Begin
  If FPosition = AValue Then Exit;
  FPosition := AValue;

  FPosition := EnsureRange(FPosition, FStartDateTime, FEndDateTime);

  RebuildBuffer;
  Invalidate;
End;

Procedure TTimeTrackbar.SetDecorator(Const AValue: TRenderer);
Begin
  If FDecorator = AValue Then Exit;
  FDecorator := AValue;

  RebuildBuffer;
  Invalidate;
End;

Procedure TTimeTrackbar.SetEndDateTime(Const AValue: TDateTime);
Begin
  If FEndDateTime = AValue Then Exit;
  FEndDateTime := AValue;

  RebuildBuffer;
  Invalidate;
End;

Procedure TTimeTrackbar.SetStartDateTime(Const AValue: TDateTime);
Begin
  If FStartDateTime = AValue Then Exit;
  FStartDateTime := AValue;

  RebuildBuffer;
  Invalidate;
End;

Procedure TTimeTrackbar.RebuildBuffer;
Var
  iHalfPosn, iGaugeWidth: Integer;
  dPosition: Extended;
  iThumbMid: Int64;
  R: TRect;
  iGaugeLeft: Integer;
  iGaugeRight, iGaugeHeight: Integer;
Begin
  If (ClientWidth <= 0) Or (ClientHeight <= 0) Then
    Exit;

  FBackBuffer.SetSize(ClientWidth, ClientHeight);

  // Background
  FBackBuffer.Canvas.Brush.Style := bsSolid;
  FBackBuffer.Canvas.Brush.Color := Color;
  FBackBuffer.Canvas.FillRect(Rect(0, 0, ClientWidth, ClientHeight));
  FBackBuffer.Canvas.Font.Assign(Font);

  // Safety Checks
  If (FStartDateTime = 0) Or (FEndDateTime = 0) Or (FPosition = 0) Or
    (FEndDateTime <= FStartDateTime) Then
    Exit;

  // Helpers
  iHalfPosn := ClientHeight Div 2;
  iGaugeWidth := ClientWidth - THUMB_WIDTH;
  iGaugeLeft := THUMB_WIDTH Div 2;
  iGaugeRight := ClientWidth - (THUMB_WIDTH Div 2);
  If Assigned(FDecorator) Then
    iGaugeHeight := GAUGE_HEIGHT_DECORATOR
  Else
    iGaugeHeight := GAUGE_HEIGHT;

  // Optional Decorator
  If Assigned(FDecorator) Then
    FDecorator.Paint(FBackBuffer.Canvas, Rect(iGaugeLeft, 0, iGaugeRight, ClientHeight));

  // Unfilled portion of the gauge (right)
  R := Rect(iGaugeLeft, iHalfPosn - (iGaugeHeight Div 2), iGaugeRight, iHalfPosn +
    (iGaugeHeight Div 2));

  FBackBuffer.Canvas.Brush.Color := clBtnShadow;
  FBackBuffer.Canvas.Pen.Color := clBtnShadow;
  FBackBuffer.Canvas.RoundRect(R, 4, 4);

  // Filled portion of the gauge (left)
  dPosition := (FPosition - FStartDateTime) / (FEndDateTime - FStartDateTime);
  dPosition := EnsureRange(dPosition, 0, 1);
  iThumbMid := (THUMB_WIDTH Div 2) + Trunc(iGaugeWidth * dPosition);

  R := Rect(iGaugeLeft, iHalfPosn - (iGaugeHeight Div 2), iThumbMid, iHalfPosn +
    (iGaugeHeight Div 2));

  FBackBuffer.Canvas.Brush.Color := clLime;
  FBackBuffer.Canvas.Pen.Color := clLime;
  FBackBuffer.Canvas.RoundRect(R, 4, 4);

  // Thumb
  R := Rect(iThumbMid - (THUMB_WIDTH Div 2), iHalfPosn - (THUMB_HEIGHT Div 2),
    iThumbMid + (THUMB_WIDTH Div 2) - 1, iHalfPosn + (THUMB_HEIGHT Div 2) - 1);

  FBackBuffer.Canvas.Brush.Color := clBtnFace;

  If Focused Then
  Begin
    FBackBuffer.Canvas.Pen.Color := RGBToColor(255, 165, 0);
    FBackBuffer.Canvas.Pen.Width := 2;
  End
  Else
  Begin
    FBackBuffer.Canvas.Pen.Color := clBtnShadow;
    FBackBuffer.Canvas.Pen.Width := 1;
  End;

  FBackBuffer.Canvas.RoundRect(R, 5, 5);

  FBackBuffer.Canvas.Pen.Width := 1;
End;

Procedure TTimeTrackbar.Paint;
Begin
  If (FBackBuffer.Width <> ClientWidth) Or (FBackBuffer.Height <> ClientHeight) Then
    RebuildBuffer;

  Canvas.Draw(0, 0, FBackBuffer);
End;

Procedure TTimeTrackbar.Resize;
Begin
  Inherited Resize;
  RebuildBuffer;
  Invalidate;
End;

Procedure TTimeTrackbar.DoEnter;
Begin
  Inherited DoEnter;
  RebuildBuffer;
  Invalidate;
End;

Procedure TTimeTrackbar.DoExit;
Begin
  Inherited DoExit;
  RebuildBuffer;
  Invalidate;
End;

Procedure TTimeTrackbar.SeekToFraction(AFraction: Double);
Var
  dtNewPosition: TDateTime;
Begin
  If AFraction < 0 Then
    AFraction := 0
  Else If AFraction > 1 Then
    AFraction := 1;

  dtNewPosition := FStartDateTime + ((FEndDateTime - FStartDateTime) * AFraction);

  SetPosition(dtNewPosition);

  If Assigned(FOnChange) Then
    FOnChange(Self);
End;

Function TTimeTrackbar.GetPositionPercent: Single;
Var
  dtDuration: TDateTime;
Begin
  dtDuration := FEndDateTime - FStartDateTime;

  If dtDuration > 0 Then
    Result := 100 * (FPosition - FStartDateTime) / (dtDuration)
  Else
    Result := 0;
End;

Procedure TTimeTrackbar.SetPositionAsPercent(Const AValue: Single);
Var
  dtDuration: TDateTime;
Begin
  If InRange(AValue, 0, 100) Then
  Begin
    dtDuration := FEndDateTime - FStartDateTime;
    If dtDuration > 0 Then
      FPosition := FStartDateTime + AValue / 100 * dtDuration
    Else
      FPosition := 0;
  End
  Else If AValue <= 0 Then
    FPosition := FStartDateTime
  Else
    FPosition := FEndDateTime;

  RebuildBuffer;
  Invalidate;
End;

Procedure TTimeTrackbar.SeekToTime(AValue: TDateTime);
Begin
  If AValue < FStartDateTime Then
    AValue := FStartDateTime
  Else If AValue > FEndDateTime Then
    AValue := FEndDateTime;

  SetPosition(AValue);

  If Assigned(FOnChange) Then
    FOnChange(Self);
End;

Procedure TTimeTrackbar.MouseSeek(X: Integer);
Var
  iTrackLeft: Integer;
  iTrackWidth: Integer;
  dFraction: Double;
Begin
  iTrackLeft := THUMB_WIDTH Div 2;
  iTrackWidth := ClientWidth - THUMB_WIDTH;

  If iTrackWidth <= 0 Then
    Exit;

  dFraction := (X - iTrackLeft) / iTrackWidth;

  SeekToFraction(dFraction);
End;

Procedure TTimeTrackbar.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  Inherited MouseDown(Button, Shift, X, Y);

  If Button <> mbLeft Then
    Exit;

  SetFocus;

  FDragging := True;
  MouseSeek(X);
End;

Procedure TTimeTrackbar.MouseMove(Shift: TShiftState; X, Y: Integer);
Begin
  Inherited MouseMove(Shift, X, Y);

  If FDragging Then
    MouseSeek(X);
End;

Procedure TTimeTrackbar.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  Inherited MouseUp(Button, Shift, X, Y);

  If Button = mbLeft Then
  Begin
    MouseSeek(X);
    FDragging := False;
  End;
End;

Procedure TTimeTrackbar.KeyDown(Var Key: Word; Shift: TShiftState);
Var
  dSeconds: Double;
Begin
  Inherited KeyDown(Key, Shift);

  Case Key Of

    VK_LEFT,
    VK_RIGHT:
    Begin
      If ssCtrl In Shift Then
        dSeconds := 5
      Else If ssShift In Shift Then
        dSeconds := 0.5
      Else
        dSeconds := 1;

      If Key = VK_LEFT Then
        dSeconds := -dSeconds;

      SeekToTime(FPosition + (dSeconds / SecsPerDay));
      Key := 0;
    End;

    VK_HOME:
    Begin
      SeekToTime(FStartDateTime);
      Key := 0;
    End;

    VK_END:
    Begin
      SeekToTime(FEndDateTime);
      Key := 0;
    End;

  End;
End;

End.
