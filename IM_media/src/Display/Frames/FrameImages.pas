Unit FrameImages;

{-------------------------------------------------------------------------------
  Package   : IM_application
  Unit      : FrameImages.pas
  Description
    Image viewer frame with navigation and clipboard actions.

  Source
    Copyright (c) 2025
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    ~2008: Creation date unknown, original local SVN repository lost
    2014-07-05: Uploaded to SourceForge/Package "Shared"
    2024-01-22: Migrated to Github.  Refactored package to "IM_application"
    2025-11-29: Added LGPL-3.0-or-later license header

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
  Classes, Controls, Dialogs, ExtCtrls, FileUtil, Forms, FrameBase,
  Graphics, LResources, Menus, StdCtrls, SysUtils, types;

Type
  TScrollToNext = Procedure(Sender: TObject; AScrollUp: Boolean) Of Object;

  { TFrameImage }

  TFrameImage = Class(TfmeBase)
    imgImage: TImage;
    lblStatus: TLabel;
    MenuItem1: TMenuItem;
    mnuCopyToClipboard: TMenuItem;
    mnuImage: TPopupMenu;
    mnuOpenInExplorer2: TMenuItem;
    mnuToggleStretch: TMenuItem;
    sboxImage: TScrollBox;
    Procedure FrameResize(Sender: TObject);
    Procedure mnuCopyToClipboardClick(Sender: TObject);
    Procedure mnuImagePopup(Sender: TObject);
    Procedure mnuOpenInExplorer2Click(Sender: TObject);
    Procedure mnuToggleStretchClick(Sender: TObject);
    Procedure sboxImageMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; Var Handled: Boolean);
  Protected
    FCenterBeforeZoom: TPoint;
    FFilename: String;
    FPDF: Boolean;
    FScale: Double;
    FScrollToNext: TScrollToNext;
    FStretch: Boolean;
    FZoomToWidth: Boolean;
    Procedure CenterImage;
    Function GetScale: Double;
    Procedure ResizeImage;
    Procedure ClearImage;
    Procedure SetFilename(Const AValue: String);
    Procedure SetScale(AValue: Double);
    Procedure SetStretch(AValue: Boolean);
    Procedure SetZoomToWidth(AValue: Boolean);
  Public
    Procedure ZoomToPage;  // Sets Stretch to true, all else reset Stretch to false
    Procedure Zoom100;
    Procedure ZoomIn;
    Procedure ZoomOut;

    Procedure ScrollToTop;
    Procedure ScrollToBottom;

    Constructor Create(TheOwner: TComponent); Override;

    Property Filename: String read FFilename write SetFilename;
    Property PDF: Boolean read FPDF write FPDF;

    Property Scale: Double read GetScale write SetScale;
    Property OnScrollToNext: TScrollToNext read FScrollToNext write FScrollToNext;
    Property Stretch: Boolean read FStretch write SetStretch;
    Property ZoomToWidth: Boolean read FZoomToWidth write SetZoomToWidth;
  End;

Implementation

Uses
  Clipbrd, OSSupport, Math;

{$R *.lfm}

{ TFrameImage }

Constructor TFrameImage.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);
  FPDF := False;
  FScale := 1.0;
  FStretch := True;
  FZoomToWidth := False;

  FCenterBeforeZoom := Point(-1, -1);
  FScrollToNext := nil;
End;

Procedure TFrameImage.mnuCopyToClipboardClick(Sender: TObject);
Var
  oBmp: TBitmap;
Begin
  If Assigned(imgImage.Picture.Graphic) Then
  Begin
    oBmp := TBitmap.Create;
    Try
      oBmp.Assign(imgImage.Picture.Graphic);
      Clipboard.Assign(oBmp);
    Finally
      oBmp.Free;
    End;
  End;
End;

Procedure TFrameImage.FrameResize(Sender: TObject);
Begin
  Stretch := FStretch;

  ResizeImage;
End;

Procedure TFrameImage.mnuImagePopup(Sender: TObject);
Begin
  If FStretch Then
    mnuToggleStretch.Caption := 'Show Full Size'
  Else
    mnuToggleStretch.Caption := 'Zoom to fit';

  mnuCopyToClipboard.Enabled := Assigned(imgImage.Picture.Graphic);
  mnuOpenInExplorer2.Enabled := mnuCopyToClipboard.Enabled;
  mnuToggleStretch.Enabled := mnuCopyToClipboard.Enabled;
End;

Procedure TFrameImage.mnuOpenInExplorer2Click(Sender: TObject);
Begin
  If Assigned(imgImage.Picture.Graphic) Then
    LaunchFile('explorer.exe', Format('/e,/select,"%s"', [FFilename]))
  Else
    ShowMessage('No image loaded...');
End;

Procedure TFrameImage.mnuToggleStretchClick(Sender: TObject);
Begin
  Stretch := Not FStretch;
  CenterImage;
End;

Procedure TFrameImage.sboxImageMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; Var Handled: Boolean);
Var
  iDirection, iPosition: Integer;
Begin
  If Shift = [] Then
  Begin
    If WheelDelta < 0 Then
      iDirection := 1
    Else
      iDirection := -1;

    iPosition := sboxImage.VertScrollBar.Position;

    If sboxImage.VertScrollBar.Visible Then
    Begin
      If assigned(FScrollToNext) Then
      Begin
        If (iDirection = -1) And (iPosition = 0) Then
          FScrollToNext(Self, True)
        Else If (iDirection = 1) And (iPosition >= (sboxImage.VertScrollBar.Range -
          sboxImage.VertScrollBar.Page)) Then
          FScrollToNext(Self, False)
        Else
          With sboxImage.VertScrollBar Do
            Position := Position + (4 * Increment * iDirection);
      End
      Else
        With sboxImage.VertScrollBar Do
          Position := Position + (30 * iDirection);
    End
    Else If assigned(FScrollToNext) Then
      FScrollToNext(Self, (iDirection = -1));
  End
  Else If ssCtrl In Shift Then
  Begin
    FCenterBeforeZoom := TControl(Sender).ClientToScreen(MousePos);
    FCenterBeforeZoom := sboxImage.ScreenToClient(FCenterBeforeZoom);
    If WheelDelta < 0 Then
      ZoomOut
    Else
      ZoomIn;
  End;
End;

Procedure TFrameImage.SetFilename(Const AValue: String);
Begin
  If FFilename = AValue Then
    exit;

  FFilename := AValue;
  If FileExists(FFilename) Then
  Begin
    imgImage.Picture.LoadFromFile(FFilename);

    If Assigned(imgImage.Picture.Graphic) Then
    Begin
      Stretch := FStretch;

      ResizeImage;
      CenterImage;
    End
    Else
      ClearImage;
  End
  Else
    ClearImage;
End;

Procedure TFrameImage.SetScale(AValue: Double);
Begin
  FZoomToWidth := False;

  FScale := AValue;

  ResizeImage;
End;

Procedure TFrameImage.SetStretch(AValue: Boolean);
Begin
  FStretch := AValue;

  If FStretch Then
    ZoomToPage
  Else If FZoomToWidth Then
    ZoomToWidth := True
  Else
    Zoom100;
End;

Procedure TFrameImage.ZoomToPage;
Var
  dScaleX, dScaleY: Extended;
Begin
  If Assigned(imgImage.Picture) And (imgImage.Picture.Width > 0) Then
  Begin
    FStretch := True;

    dScaleX := sboxImage.ClientWidth / imgImage.Picture.Width;
    dScaleY := sboxImage.ClientHeight / imgImage.Picture.Height;

    Scale := Min(dScaleX - 0.02, dScaleY - 0.02);

    ResizeImage;
    CenterImage;

    RefreshUI;
  End;
End;

Procedure TFrameImage.SetZoomToWidth(AValue: Boolean);
Begin
  FStretch := False;

  Scale := sboxImage.ClientWidth / imgImage.Picture.Width;
  FZoomToWidth := AValue;

  ResizeImage;
  CenterImage;

  RefreshUI;
End;

Procedure TFrameImage.Zoom100;
Begin
  FStretch := False;
  Scale := 1;
  CenterImage;
  RefreshUI;
End;

Procedure TFrameImage.ZoomIn;
Begin
  FStretch := False;
  Scale := Scale * 1.1;
  RefreshUI;
End;

Procedure TFrameImage.ZoomOut;
Begin
  FStretch := False;
  Scale := Scale / 1.1;
  RefreshUI;
End;

Procedure TFrameImage.ScrollToTop;
Begin
  sboxImage.VertScrollBar.Position := 0;
End;

Procedure TFrameImage.ScrollToBottom;
Begin
  sboxImage.VertScrollBar.Position := sboxImage.VertScrollBar.Page + sboxImage.VertScrollBar.Range;
End;

Procedure TFrameImage.ClearImage;
Begin
  FFilename := '';
  imgImage.Picture.Clear;
  lblStatus.Caption := '';
End;

Procedure TFrameImage.CenterImage;
Begin
  With sboxImage.HorzScrollBar Do
    Position := (Range - Page) Div 2;

  If FPDF Then
    sboxImage.VertScrollBar.Position := 0
  Else
    With sboxImage.VertScrollBar Do
      Position := (Range - Page) Div 2;
End;

Function TFrameImage.GetScale: Double;
Begin
  Result := FScale;
End;

Procedure TFrameImage.ResizeImage;

  Function ScaleInt(aValue: Double): Integer;
  Begin
    Result := Trunc(FScale * AValue);
  End;

Var
  iLeft, iTop, iNewWidth, iNewHeight: Integer;
  iOldWidth, iOldHeight: Integer;
  dOldXPercent, dOldYPercent: Extended;
  iNewXPosition, iNewYPosition: Integer;
  iOldHoriz, iOldVert: Integer;
Begin
  If Assigned(imgImage.Picture.Graphic) Then
  Begin
    iOldWidth := imgImage.Width;
    iOldHeight := imgImage.Height;

    iOldHoriz := sboxImage.HorzScrollBar.Position;
    iOldVert := sboxImage.VertScrollBar.Position;

    iNewWidth := ScaleInt(imgImage.Picture.Width);
    iNewHeight := ScaleInt(imgImage.Picture.Height);

    iLeft := Max(0, (sboxImage.VertScrollBar.ClientSizeWithoutBar - iNewWidth) Div 2);
    iTop := Max(0, (sboxImage.HorzScrollBar.ClientSizeWithoutBar - iNewHeight) Div 2);

    imgImage.SetBounds(iLeft, iTop, iNewWidth, iNewHeight);

    If (FCenterBeforeZoom.X <> -1) And (FCenterBeforeZoom.X <> -1) Then
    Begin
      If sboxImage.HorzScrollBar.Visible Then
      Begin
        dOldXPercent := (FCenterBeforeZoom.X + iOldHoriz) / iOldWidth;
        iNewXPosition := Trunc(iNewWidth * dOldXPercent) - FCenterBeforeZoom.X;
        sboxImage.HorzScrollBar.Position := iNewXPosition;
      End;

      If sboxImage.VertScrollBar.Visible Then
      Begin
        dOldYPercent := (FCenterBeforeZoom.Y + iOldVert) / iOldHeight;
        iNewYPosition := Trunc(iNewHeight * dOldYPercent) - FCenterBeforeZoom.Y;
        sboxImage.VertScrollBar.Position := iNewYPosition;
      End;
    End;

    lblStatus.Caption := Format('(%d x %d)    Filename: %s    Zoom: %d%%',
      [imgImage.Picture.Width, imgImage.Picture.Height, ExtractFilename(FFilename),
      ScaleInt(100)]);

      // Reset this, we might not use it again
    FCenterBeforeZoom := Point(-1, -1);
  End;
End;

Initialization

End.
