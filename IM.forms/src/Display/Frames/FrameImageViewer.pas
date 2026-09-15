Unit FrameImageViewer;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
Interface

Uses
  Classes, SysUtils, Forms, Controls, Grids, ComCtrls, Menus, ActnList, fgl, BGRABitmap, Types,
  Inifiles, LCLType, LCLIntf, Graphics;

Type

  TImageSelectionMode = (ismNone, ismSingle, ismMultiple);

  { TViewerImage }

  TViewerImage = Class
  Private
    FFileName: String;
    FCaption: String;
    FSelected: Boolean;
    FThumbnail: TBGRABitmap;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure InvalidateThumbnail;
    Function GetThumbnail(Const AWidth, AHeight: Integer): TBGRABitmap;

    Property FileName: String Read FFileName Write FFileName;
    Property Caption: String Read FCaption Write FCaption;
    Property Selected: Boolean Read FSelected Write FSelected;
  End;

  { TViewerImageList }

  TViewerImageList = Class(Specialize TFPGObjectList<TViewerImage>)
  Public
    Procedure InvalidateThumbnails;
  End;

  { TFrameImageViewer }

  TFrameImageViewer = Class(TFrame)
    actImages: TActionList;
    actOpenFolder: TAction;
    actViewImage: TAction;
    grdImages: TDrawGrid;
    mnuOpenFolder: TMenuItem;
    mnuViewImage: TMenuItem;
    pmImages: TPopupMenu;

    Procedure actOpenFolderExecute(Sender: TObject);
    Procedure FrameResize(Sender: TObject);
    Procedure actViewImageClick(Sender: TObject);
    Procedure grdImagesClick(Sender: TObject);
    Procedure grdImagesDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);

    Procedure grdImagesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure pmImagesPopup(Sender: TObject);
  Private
    FCaptionNotSelected: String;
    FCaptionSelected: String;
    FImages: TViewerImageList;
    FSelectionMode: TImageSelectionMode;
    FThumbnailBorder: Integer;
    FThumbnailHeight: Integer;
    FThumbnailWidth: Integer;
    Function GetImage(AIndex: Integer): TViewerImage;
    Procedure SetThumbnailBorder(Const AValue: Integer);
    Procedure UpdateGridLayout;

  Public
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure SetThumbnailSize(Const AWidth, AHeight: Integer);

    Function AddImage(Const AFilename, ACaption: String): TViewerImage;
    Procedure ClearImages;

    Function ImageCount: Integer;
    Property Image[AIndex: Integer]: TViewerImage Read GetImage;

    Procedure LoadSettings(AInifile: TIniFile);
    Procedure SaveSettings(AInifile: TIniFile);

    Property ThumbnailWidth: Integer Read FThumbnailWidth;
    Property ThumbnailHeight: Integer Read FThumbnailHeight;
    Property ThumbnailBorder: Integer Read FThumbnailBorder Write SetThumbnailBorder;

    Property CaptionSelected: String Read FCaptionSelected Write FCaptionSelected;
    Property CaptionNotSelected: String Read FCaptionNotSelected Write FCaptionNotSelected;

    // Checkbox on each image
    Property SelectionMode: TImageSelectionMode Read FSelectionMode Write FSelectionMode;
  End;

Implementation

Uses
  BGRAThumbnail, BGRABitmapTypes, OSSupport;

  {$R *.lfm}

  { TViewerImage }

Constructor TViewerImage.Create;
Begin
  FThumbnail := nil;

  FSelected := True;
End;

Destructor TViewerImage.Destroy;
Begin
  FreeAndNil(FThumbnail);

  Inherited Destroy;
End;

Procedure TViewerImage.InvalidateThumbnail;
Begin
  FreeAndNil(FThumbnail);
End;

Function TViewerImage.GetThumbnail(Const AWidth, AHeight: Integer): TBGRABitmap;
Begin
  If Not Assigned(FThumbnail) Then
    FThumbnail := GetFileThumbnail(FFileName, AWidth, AHeight, BGRAWhite, True);

  Result := FThumbnail;
End;

{ TViewerImageList }

Procedure TViewerImageList.InvalidateThumbnails;
Var
  oImage: TViewerImage;
Begin
  For oImage In Self Do
    oImage.InvalidateThumbnail;
End;

{ TFrameImageViewer }

Constructor TFrameImageViewer.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  FThumbnailWidth := 320;
  FThumbnailHeight := 180;
  FThumbnailBorder := 4;

  FSelectionMode := ismNone;
  FCaptionSelected := 'Save';
  FCaptionNotSelected := 'Do not save';

  FImages := TViewerImageList.Create(True);
End;

Destructor TFrameImageViewer.Destroy;
Begin
  FreeAndNil(FImages);

  Inherited Destroy;
End;

Procedure TFrameImageViewer.UpdateGridLayout;
Var
  iCellWidth, iCellHeight: Integer;
  iCols, iRows: Integer;
  iCaptionHeight: Integer;
Begin
  // Calculations
  iCaptionHeight := grdImages.Canvas.TextHeight('Ag') + 4;

  iCellWidth := FThumbnailWidth + (FThumbnailBorder * 2);
  iCellHeight := FThumbnailHeight + (FThumbnailBorder * 2) + iCaptionHeight;

  iCols := grdImages.ClientWidth Div iCellWidth;
  If iCols < 1 Then
    iCols := 1;

  If FImages.Count = 0 Then
  Begin
    iRows := 1;
    iCols := 1;
  End
  Else
    iRows := (FImages.Count + iCols - 1) Div iCols;

  // UI
  grdImages.DefaultColWidth := iCellWidth;
  grdImages.DefaultRowHeight := iCellHeight;
  grdImages.ColCount := iCols;
  grdImages.RowCount := iRows;

  grdImages.Invalidate;
End;

Procedure TFrameImageViewer.grdImagesDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
Var
  iIndex: Integer;
  iLeft, iTop, Flags: Integer;
  oImage: TViewerImage;
  oThumbnail: TBGRABitmap;
  R: TRect;
  sSelectionCaption: String;
Begin
  { Clear the complete cell }
  grdImages.Canvas.FillRect(aRect);

  iIndex := (aRow * grdImages.ColCount) + aCol;
  If iIndex >= FImages.Count Then
    Exit;

  oImage := FImages[iIndex];
  oThumbnail := oImage.GetThumbnail(FThumbnailWidth, FThumbnailHeight);

  If (FSelectionMode <> ismNone) And (Not oImage.Selected) Then
  Begin
    oThumbnail := oThumbnail.FilterGrayscale;

    oThumbnail.FillRect(oThumbnail.ClipRect, BGRA(0, 0, 0, 150),
      dmDrawWithTransparency);
  End;

  If Assigned(oThumbnail) Then
  Begin
    iLeft := aRect.Left + ((aRect.Width - oThumbnail.Width) Div 2);
    iTop := aRect.Top + FThumbnailBorder;

    oThumbnail.Draw(grdImages.Canvas, iLeft, iTop, True);

    If FSelectionMode = ismMultiple Then
    Begin
      // Checkbox in top-left corner of image cell
      R := Rect(aRect.Left + 8, aRect.Top + 8, aRect.Left + 26, aRect.Top + 26);

      Flags := DFCS_BUTTONCHECK;

      If oImage.Selected Then
        Flags := Flags Or DFCS_CHECKED;

      DrawFrameControl(grdImages.Canvas.Handle, R, DFC_BUTTON, Flags);

      // Optional selection caption
      If oImage.Selected Then
        sSelectionCaption := FCaptionSelected
      Else
        sSelectionCaption := FCaptionNotSelected;

      If sSelectionCaption <> '' Then
      Begin
        grdImages.Canvas.Brush.Color := clWhite;
        grdImages.Canvas.Font.Color := clBlack;

        grdImages.Canvas.TextOut(R.Right + 2, R.Top + 2, '  ' + sSelectionCaption + ' ');
      End;
    End;
  End;

  { Caption }
  If oImage.Selected Then
  Begin
    grdImages.Canvas.Font.Color := clBlack
  end
  Else
  Begin
    grdImages.Canvas.Font.Color := clGrayText;
  end;

  // Center the caption in the cell
  iLeft := aRect.Left + ((aRect.Width - grdImages.Canvas.TextWidth(oImage.Caption)) Div 2);
  grdImages.Canvas.TextOut(iLeft, aRect.Top + FThumbnailBorder + FThumbnailHeight +
    2, oImage.Caption);
End;

Procedure TFrameImageViewer.grdImagesMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  aCol, aRow: Integer;
Begin
  If Button <> mbRight Then
    Exit;

  grdImages.MouseToCell(X, Y, aCol, aRow);

  If (aCol >= grdImages.FixedCols) And (aRow >= grdImages.FixedRows) Then
  Begin
    grdImages.Col := aCol;
    grdImages.Row := aRow;
  End;
End;

Procedure TFrameImageViewer.pmImagesPopup(Sender: TObject);
Var
  iIndex: Integer;
  oImage: TViewerImage;
Begin
  iIndex := (grdImages.Row * grdImages.ColCount) + grdImages.Col;

  actViewImage.Enabled := (iIndex < FImages.Count);
  actOpenFolder.Enabled := (iIndex < FImages.Count);
End;

Procedure TFrameImageViewer.FrameResize(Sender: TObject);
Begin
  UpdateGridLayout;
End;

Procedure TFrameImageViewer.actOpenFolderExecute(Sender: TObject);
Var
  iIndex: Integer;
  oImage: TViewerImage;
Begin
  iIndex := (grdImages.Row * grdImages.ColCount) + grdImages.Col;

  If iIndex >= FImages.Count Then
    Exit;

  oImage := FImages[iIndex];

  If Assigned(oImage) Then
    LaunchFile('explorer.exe', Format('/e,/select,"%s"', [oImage.FileName]));
End;

Procedure TFrameImageViewer.actViewImageClick(Sender: TObject);
Var
  iIndex: Integer;
  oImage: TViewerImage;
Begin
  iIndex := (grdImages.Row * grdImages.ColCount) + grdImages.Col;

  If iIndex >= FImages.Count Then
    Exit;

  oImage := FImages[iIndex];

  If Assigned(oImage) Then
    LaunchDocument(oImage.FileName);
End;

Procedure TFrameImageViewer.grdImagesClick(Sender: TObject);
Var
  iIndex: Integer;
  oImage: TViewerImage;
Begin
  If FSelectionMode <> ismNone Then
  Begin
    iIndex := (grdImages.Row * grdImages.ColCount) + grdImages.Col;

    If iIndex >= FImages.Count Then
      Exit;

    oImage := FImages[iIndex];

    oImage.Selected := Not oImage.Selected;

    grdImages.InvalidateCell(grdImages.Col, grdImages.Row);
  End;
End;

Procedure TFrameImageViewer.SetThumbnailBorder(Const AValue: Integer);
Begin
  If FThumbnailBorder = AValue Then Exit;

  FThumbnailBorder := AValue;

  UpdateGridLayout;
End;

Function TFrameImageViewer.GetImage(AIndex: Integer): TViewerImage;
Begin
  If (AIndex >= 0) And (AIndex < FImages.Count) Then
    Result := FImages[AIndex]
  Else
    Result := nil;
End;

Procedure TFrameImageViewer.SetThumbnailSize(Const AWidth, AHeight: Integer);
Begin
  If (FThumbnailWidth = AWidth) And (FThumbnailHeight = AHeight) Then
    Exit;

  FThumbnailWidth := AWidth;
  FThumbnailHeight := AHeight;

  FImages.InvalidateThumbnails;

  UpdateGridLayout;
End;

Function TFrameImageViewer.AddImage(Const AFilename, ACaption: String): TViewerImage;
Var
  oImage: TViewerImage;
Begin
  oImage := TViewerImage.Create;
  oImage.FileName := AFilename;
  oImage.Caption := ACaption;

  FImages.Add(oImage);

  UpdateGridLayout;

  Result := oImage;
End;

Procedure TFrameImageViewer.ClearImages;
Begin
  FImages.Clear;

  UpdateGridLayout;
End;

Function TFrameImageViewer.ImageCount: Integer;
Begin
  Result := FImages.Count;
End;

Procedure TFrameImageViewer.LoadSettings(AInifile: TIniFile);
Begin

End;

Procedure TFrameImageViewer.SaveSettings(AInifile: TIniFile);
Begin

End;

End.
