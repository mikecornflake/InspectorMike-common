Unit FrameImageGrid;

{$mode ObjFPC}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Grids, ComCtrls, Menus, ActnList, fgl, BGRABitmap, Types,
  Inifiles, LCLType, LCLIntf, Graphics, LazLogger, SyncObjs;

Type

  TImageSelectionMode = (ismNone, ismSingle, ismMultiple);

  { TViewerImage }

  TViewerImage = Class
  Private
    FFileName: String;
    FCaption: String;
    FSelected: Boolean;
    FThumbnail: TBGRABitmap;
    FThumbnailPending: Boolean;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure InvalidateThumbnail;
    Procedure SetThumbnail(ABitmap: TBGRABitmap);
    Property ThumbnailPending: Boolean Read FThumbnailPending Write FThumbnailPending;
    Property CachedThumbnail: TBGRABitmap Read FThumbnail;
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

  // A request owns only immutable information. The worker must NEVER touch
  // TViewerImage, TDrawGrid or any other LCL object.
  TThumbnailRequest = Class
    FileName: String;
    Index, Width, Height: Integer;
    Generation: QWord;
    Bitmap: TBGRABitmap; // Owned by the request until transferred to an image.
    Destructor Destroy; Override;
  End;

  TFrameImageGrid = Class;

  // One persistent worker; a LIFO queue favours the images most recently
  // requested during scrolling. Results are delivered on the main thread.
  TThumbnailWorker = Class(TThread)
  Private
    FOwner: TFrameImageGrid;
    FLock: TRTLCriticalSection;
    FWake: TEvent;
    FPending: Specialize TFPGObjectList<TThumbnailRequest>;
    FCompleted: Specialize TFPGObjectList<TThumbnailRequest>;
    Procedure DeliverResults;
  Protected
    Procedure Execute; Override;
  Public
    Constructor Create(AOwner: TFrameImageGrid);
    Destructor Destroy; Override;
    Procedure Request(ARequest: TThumbnailRequest);
    Procedure CancelPending;
    Procedure Stop;
  End;

  { TFrameImageGrid }

  TFrameImageGrid = Class(TFrame)
    actImages: TActionList;
    actAddimage: TAction;
    actRefreshList: TAction;
    actRenameImage: TAction;
    actDeleteImage: TAction;
    actOpenFolder: TAction;
    actViewImage: TAction;
    grdImages: TDrawGrid;
    ImageList: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    Separator1: TMenuItem;
    mnuOpenFolder: TMenuItem;
    mnuViewImage: TMenuItem;
    pmImages: TPopupMenu;
    ToolBar: TToolBar;
    btnRefresh: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;

    Procedure actAddimageExecute(Sender: TObject);
    Procedure actDeleteImageExecute(Sender: TObject);
    Procedure actOpenFolderExecute(Sender: TObject);
    Procedure actRefreshListExecute(Sender: TObject);
    Procedure actRenameImageExecute(Sender: TObject);
    Procedure FrameResize(Sender: TObject);
    Procedure actViewImageClick(Sender: TObject);
    Procedure grdImagesClick(Sender: TObject);
    Procedure grdImagesDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    Procedure grdImagesGetCellHint(Sender: TObject; aCol, aRow: Integer; Var HintText: String);

    Procedure grdImagesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure pmImagesPopup(Sender: TObject);
  Private
    FCaptionNotSelected: String;
    FCaptionSelected: String;
    FImages: TViewerImageList;
    FOnRequestAddImage: TNotifyEvent;
    FOnRequestRefreshImages: TNotifyEvent;
    FSelectionMode: TImageSelectionMode;
    FThumbnailBorder: Integer;
    FThumbnailHeight: Integer;
    FThumbnailWidth: Integer;
    FUpdateCount: Integer;
    FWorker: TThumbnailWorker;
    FGeneration: QWord;
    FDestroying: Boolean;

    Procedure ThumbnailReady(ARequest: TThumbnailRequest);
    Procedure QueueThumbnail(AIndex: Integer);
    Procedure CancelThumbnails;

    Function GetImage(AIndex: Integer): TViewerImage;
    Function GetShowToolbar: Boolean;
    Procedure RefreshUI;
    Procedure SetShowToolbar(Const AValue: Boolean);
    Procedure SetThumbnailBorder(Const AValue: Integer);
    Procedure UpdateGridLayout;
  Public
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure SetThumbnailSize(Const AWidth, AHeight: Integer);

    Procedure BeginUpdate;
    Procedure EndUpdate;

    Function AddImage(Const AFilename, ACaption: String): TViewerImage;
    Procedure ClearImages;

    Procedure RefreshImages;

    Function ImageCount: Integer;
    Property Image[AIndex: Integer]: TViewerImage Read GetImage;

    Procedure LoadSettings(AInifile: TIniFile);
    Procedure SaveSettings(AInifile: TIniFile);

    Property ThumbnailWidth: Integer Read FThumbnailWidth;
    Property ThumbnailHeight: Integer Read FThumbnailHeight;
    Property ThumbnailBorder: Integer Read FThumbnailBorder Write SetThumbnailBorder;

    Property CaptionSelected: String Read FCaptionSelected Write FCaptionSelected;
    Property CaptionNotSelected: String Read FCaptionNotSelected Write FCaptionNotSelected;

    Property ShowToolbar: Boolean Read GetShowToolbar Write SetShowToolbar;

    // Checkbox on each image
    Property SelectionMode: TImageSelectionMode Read FSelectionMode Write FSelectionMode;

    Property OnRequestAddImage: TNotifyEvent Read FOnRequestAddImage Write FOnRequestAddImage;
    Property OnRequestRefreshImages: TNotifyEvent Read FOnRequestRefreshImages
      Write FOnRequestRefreshImages;
  End;

Implementation

Uses
  BGRAThumbnail, BGRABitmapTypes, OSSupport, LazFileUtils, FileUtil, FileSupport,
  Dialogs;

  {$R *.lfm}

  { TViewerImage }

Constructor TViewerImage.Create;
Begin
  FThumbnail := nil;
  FThumbnailPending := False;

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
  FThumbnailPending := False;
End;

Procedure TViewerImage.SetThumbnail(ABitmap: TBGRABitmap);
Begin
  FreeAndNil(FThumbnail);
  FThumbnail := ABitmap; // Takes ownership.
  FThumbnailPending := False;
End;

Function TViewerImage.GetThumbnail(Const AWidth, AHeight: Integer): TBGRABitmap;
Begin
  // Non-blocking: decoding is now performed by TThumbnailWorker.
  // Dimensions are retained in the signature for source compatibility.
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

{ TThumbnailRequest }

Destructor TThumbnailRequest.Destroy;
Begin
  Bitmap.Free;
  Inherited Destroy;
End;

{ TThumbnailWorker }

Constructor TThumbnailWorker.Create(AOwner: TFrameImageGrid);
Begin
  Inherited Create(True);
  FreeOnTerminate := False;
  FOwner := AOwner;
  InitCriticalSection(FLock);
  FWake := TEvent.Create(nil, False, False, '');
  FPending := Specialize TFPGObjectList<TThumbnailRequest>.Create(True);
  FCompleted := Specialize TFPGObjectList<TThumbnailRequest>.Create(True);
  Start;
End;

Destructor TThumbnailWorker.Destroy;
Begin
  // Stop must already have been called from the UI thread.
  FCompleted.Free;
  FPending.Free;
  FWake.Free;
  DoneCriticalSection(FLock);
  Inherited Destroy;
End;

Procedure TThumbnailWorker.Request(ARequest: TThumbnailRequest);
Begin
  EnterCriticalSection(FLock);
  Try
    FPending.Add(ARequest);
  Finally
    LeaveCriticalSection(FLock);
  End;
  FWake.SetEvent;
End;

Procedure TThumbnailWorker.CancelPending;
Begin
  EnterCriticalSection(FLock);
  Try
    FPending.Clear; // The in-flight request may finish; generation rejects it.
  Finally
    LeaveCriticalSection(FLock);
  End;
End;

Procedure TThumbnailWorker.Execute;
Var
  Req: TThumbnailRequest;
  N: Integer;
Begin
  While Not Terminated Do
  Begin
    Req := nil;
    EnterCriticalSection(FLock);
    Try
      N := FPending.Count;
      If N > 0 Then
      Begin
        Req := FPending[N - 1]; // LIFO
        FPending.Extract(Req); // Transfer ownership out of pending list.
      End;
    Finally
      LeaveCriticalSection(FLock);
    End;

    If Req = nil Then
    Begin
      FWake.WaitFor(High(Cardinal));
      Continue;
    End;

    Try
      // BGRA decoding only; no canvas, window handle or LCL operations.
      Req.Bitmap := GetFileThumbnail(Req.FileName, Req.Width, Req.Height,
        BGRAWhite, False);
    Except
      // A damaged image should not kill the worker thread.
      FreeAndNil(Req.Bitmap);
    End;

    If Terminated Then
    Begin
      Req.Free;
      Break;
    End;

    EnterCriticalSection(FLock);
    Try
      FCompleted.Add(Req);
    Finally
      LeaveCriticalSection(FLock);
    End;
    // TThread.Queue schedules DeliverResults on the MAIN thread.
    Queue(@DeliverResults);
  End;
End;

Procedure TThumbnailWorker.DeliverResults;
Var
  Req: TThumbnailRequest;
Begin
  // Runs on the main thread. Do not hold the lock while calling the frame.
  While True Do
  Begin
    EnterCriticalSection(FLock);
    Try
      If FCompleted.Count = 0 Then
        Exit;
      Req := FCompleted[0];
      FCompleted.Extract(Req);
    Finally
      LeaveCriticalSection(FLock);
    End;
    Try
      If Assigned(FOwner) Then
        FOwner.ThumbnailReady(Req);
    Finally
      Req.Free;
    End;
  End;
End;

Procedure TThumbnailWorker.Stop;
Begin
  Terminate;
  CancelPending;
  FWake.SetEvent;
  WaitFor;
  // Prevent callbacks referring to a destroyed frame/worker.
  TThread.RemoveQueuedEvents(Self);
  FOwner := nil;
End;

{ TFrameImageGrid }

Constructor TFrameImageGrid.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  FThumbnailWidth := 320;
  FThumbnailHeight := 180;
  FThumbnailBorder := 4;

  FSelectionMode := ismNone;
  FCaptionSelected := 'Save';
  FCaptionNotSelected := 'Do not save';

  FUpdateCount := 0;

  FImages := TViewerImageList.Create(True);
  FGeneration := 0;
  FDestroying := False;
  FWorker := TThumbnailWorker.Create(Self);

  RefreshUI;
End;

Destructor TFrameImageGrid.Destroy;
Begin
  FDestroying := True;
  FWorker.Stop;
  FreeAndNil(FWorker);
  FreeAndNil(FImages);

  Inherited Destroy;
End;

Procedure TFrameImageGrid.UpdateGridLayout;
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
  RefreshUI;
End;

Procedure TFrameImageGrid.grdImagesDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
Var
  iIndex: Integer;
  iLeft, iTop, Flags: Integer;
  oImage: TViewerImage;
  oThumbnail: TBGRABitmap;
  R: TRect;
  sSelectionCaption: String;
  colBack: TColor;
Begin
  colBack := clWhite;

  iIndex := (aRow * grdImages.ColCount) + aCol;
  If iIndex >= FImages.Count Then
  Begin
    { Clear the complete cell }
    grdImages.Canvas.FillRect(aRect);

    Exit;
  End;

  oImage := FImages[iIndex];
  oThumbnail := oImage.CachedThumbnail;
  If (oThumbnail = nil) And (Not oImage.ThumbnailPending) Then
    QueueThumbnail(iIndex);

  { Clear the complete cell }
  If (FSelectionMode <> ismNone) And (Not oImage.Selected) Then
    colBack := clLtGray;

  grdImages.Canvas.Brush.Color := colBack;
  grdImages.Canvas.FillRect(aRect);

  If Assigned(oThumbnail) And (FSelectionMode <> ismNone) And (Not oImage.Selected) Then
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
    
    // FilterGrayscale returns a new bitmap, not the cached thumbnail.
    If oThumbnail <> oImage.CachedThumbnail Then
      oThumbnail.Free;

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
        grdImages.Canvas.Brush.Color := colBack;
        grdImages.Canvas.Font.Color := clBlack;

        grdImages.Canvas.TextOut(R.Right + 2, R.Top + 2, '  ' + sSelectionCaption + ' ');
      End;
    End;
  End;

  { Caption }
  If oImage.Selected Then
    grdImages.Canvas.Font.Color := clBlack
  Else
    grdImages.Canvas.Font.Color := clGrayText;

  // Center the caption in the cell
  iLeft := aRect.Left + ((aRect.Width - grdImages.Canvas.TextWidth(oImage.Caption)) Div 2);
  grdImages.Canvas.TextOut(iLeft, aRect.Top + FThumbnailBorder + FThumbnailHeight +
    2, oImage.Caption);
End;

Procedure TFrameImageGrid.grdImagesGetCellHint(Sender: TObject; aCol, aRow: Integer;
  Var HintText: String);
Begin
  HintText := grdImages.Hint;
End;

Procedure TFrameImageGrid.grdImagesMouseDown(Sender: TObject; Button: TMouseButton;
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

Procedure TFrameImageGrid.pmImagesPopup(Sender: TObject);
//Var
//  iIndex: Integer;
Begin
  //  iIndex := (grdImages.Row * grdImages.ColCount) + grdImages.Col;

  RefreshUI;
End;

Procedure TFrameImageGrid.RefreshUI;
Var
  iIndex: Integer;
  bHasImages, bImageSelected: Boolean;
Begin
  iIndex := (grdImages.Row * grdImages.ColCount) + grdImages.Col;

  bHasImages := (FImages.Count > 0);
  bImageSelected := (iIndex < FImages.Count);

  actViewImage.Enabled := Enabled And bHasImages And bImageSelected;
  actOpenFolder.Enabled := Enabled And bHasImages And bImageSelected;

  actDeleteImage.Enabled := Enabled And bHasImages And bImageSelected;
  actRenameImage.Enabled := Enabled And bHasImages And bImageSelected;
  actAddimage.Enabled := Enabled And Assigned(FOnRequestAddImage);
  actRefreshList.Enabled := Enabled And Assigned(FOnRequestRefreshImages);

  actDeleteImage.Visible := ToolBar.Visible;
  actRenameImage.Visible := ToolBar.Visible;
  actAddimage.Visible := ToolBar.Visible;
  actAddimage.Visible := ToolBar.Visible;
  actRefreshList.Visible := ToolBar.Visible;
End;

Procedure TFrameImageGrid.SetShowToolbar(Const AValue: Boolean);
Begin
  ToolBar.Visible := AValue;
  RefreshUI;
End;

Procedure TFrameImageGrid.FrameResize(Sender: TObject);
Begin
  UpdateGridLayout;
End;

Procedure TFrameImageGrid.actOpenFolderExecute(Sender: TObject);
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

Procedure TFrameImageGrid.actRefreshListExecute(Sender: TObject);
Begin
  If Assigned(FOnRequestRefreshImages) Then
    FOnRequestRefreshImages(Self);
End;

Procedure TFrameImageGrid.actRenameImageExecute(Sender: TObject);
Var
  iIndex: Integer;
  oImage: TViewerImage;
  sFilename, sFolder, sNewFilename, sMessage: String;
Begin
  iIndex := (grdImages.Row * grdImages.ColCount) + grdImages.Col;

  If iIndex >= FImages.Count Then
    Exit;

  oImage := FImages[iIndex];

  sFilename := ExtractFileName(oImage.FileName);
  sFolder := IncludeTrailingBackslash(ExtractFileDir(oImage.FileName));

  sNewFilename := InputBox('Rename Image', 'New filename', sFilename);

  sNewFilename := MakeFilenameSafe(sNewFilename);

  If (sNewFilename <> '') And (sNewFilename <> sFilename) Then
    If Not FileExists(sFolder + sNewFilename) Then
    Begin
      If RenameFile(oImage.FileName, sFolder + sNewFilename) Then
      Begin
        sMessage := 'File ' + oImage.FileName + ' renamed to ' + sNewFilename;
        oImage.FileName := sFolder + sNewFilename;
        oImage.Caption := sNewFilename;

        grdImages.Invalidate;
      End
      Else
      Begin
        sMessage := 'Unable to rename ' + oImage.FileName;
        ShowMessage(sMessage);
      End;

      DebugLn('Image Viewer: ' + sMessage);
    End;
End;

Procedure TFrameImageGrid.actAddimageExecute(Sender: TObject);
Begin
  If Assigned(FOnRequestAddImage) Then
    FOnRequestAddImage(Self);
End;

Procedure TFrameImageGrid.actDeleteImageExecute(Sender: TObject);
Var
  iIndex: Integer;
  oImage: TViewerImage;
  sMessage: String;
Begin
  iIndex := (grdImages.Row * grdImages.ColCount) + grdImages.Col;

  If iIndex >= FImages.Count Then
    Exit;

  oImage := FImages[iIndex];

  If QuestionDlg('Delete Image', 'Are you sure you wish to delete this image?' +
    LineEnding + oImage.FileName, mtConfirmation, [mrYes, mrNo], 0) = mrYes Then
  Begin
    If DeleteFile(oImage.FileName) Then
    Begin
      sMessage := 'Image ' + oImage.FileName + ' deleted';
      
      CancelThumbnails;
      FImages.Delete(iIndex);
      FImages.InvalidateThumbnails;

      UpdateGridLayout;
    End
    Else
    Begin
      sMessage := 'Unable to delete ' + oImage.FileName;
      ShowMessage(sMessage);
    End;

    DebugLn('Image Viewer: ' + sMessage);
  End;
End;

Procedure TFrameImageGrid.actViewImageClick(Sender: TObject);
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

Procedure TFrameImageGrid.grdImagesClick(Sender: TObject);
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
    RefreshUI;
  End;
End;

Procedure TFrameImageGrid.SetThumbnailBorder(Const AValue: Integer);
Begin
  If FThumbnailBorder = AValue Then Exit;

  FThumbnailBorder := AValue;

  UpdateGridLayout;
End;

Function TFrameImageGrid.GetImage(AIndex: Integer): TViewerImage;
Begin
  If (AIndex >= 0) And (AIndex < FImages.Count) Then
    Result := FImages[AIndex]
  Else
    Result := nil;
End;

Function TFrameImageGrid.GetShowToolbar: Boolean;
Begin
  Result := ToolBar.Visible;;
End;

Procedure TFrameImageGrid.SetThumbnailSize(Const AWidth, AHeight: Integer);
Begin
  If (FThumbnailWidth = AWidth) And (FThumbnailHeight = AHeight) Then
    Exit;

  FThumbnailWidth := AWidth;
  FThumbnailHeight := AHeight;

  CancelThumbnails;
  FImages.InvalidateThumbnails;

  UpdateGridLayout;
End;

Procedure TFrameImageGrid.BeginUpdate;
Begin
  Inc(FUpdateCount);
End;

Procedure TFrameImageGrid.EndUpdate;
Begin
  If FUpdateCount = 0 Then
    Exit;

  Dec(FUpdateCount);

  If FUpdateCount = 0 Then
    UpdateGridLayout;
End;

Function TFrameImageGrid.AddImage(Const AFilename, ACaption: String): TViewerImage;
Var
  oImage: TViewerImage;
Begin
  oImage := TViewerImage.Create;
  oImage.FileName := AFilename;
  oImage.Caption := ACaption;

  FImages.Add(oImage);

  Result := oImage;

  If FUpdateCount = 0 Then
    UpdateGridLayout;
End;

Procedure TFrameImageGrid.ClearImages;
Begin
  CancelThumbnails;
  FImages.Clear;

  If FUpdateCount = 0 Then
    UpdateGridLayout;
End;

Procedure TFrameImageGrid.RefreshImages;
Begin
  actRefreshList.Execute;
End;

Function TFrameImageGrid.ImageCount: Integer;
Begin
  Result := FImages.Count;
End;

Procedure TFrameImageGrid.CancelThumbnails;
Var
  Img: TViewerImage;
Begin
  Inc(FGeneration);
  FWorker.CancelPending;
  For Img In FImages Do
    Img.ThumbnailPending := False;
  // Any completed/in-flight work from the old generation is ignored.
End;

Procedure TFrameImageGrid.QueueThumbnail(AIndex: Integer);
Var
  Req: TThumbnailRequest;
  Img: TViewerImage;
Begin
  If FDestroying Or (AIndex < 0) Or (AIndex >= FImages.Count) Then Exit;
  Img := FImages[AIndex];
  If Img.ThumbnailPending Or Assigned(Img.CachedThumbnail) Then Exit;
  Req := TThumbnailRequest.Create;
  Req.FileName := Img.FileName;
  Req.Index := AIndex;
  Req.Width := FThumbnailWidth;
  Req.Height := FThumbnailHeight;
  Req.Generation := FGeneration;
  Img.ThumbnailPending := True;
  FWorker.Request(Req);
End;

Procedure TFrameImageGrid.ThumbnailReady(ARequest: TThumbnailRequest);
Var
  Img: TViewerImage;
  C, R: Integer;
Begin
  If FDestroying Or (ARequest.Generation <> FGeneration) Then Exit;
  If (ARequest.Index < 0) Or (ARequest.Index >= FImages.Count) Then Exit;
  Img := FImages[ARequest.Index];
  If Img.FileName <> ARequest.FileName Then Exit;
  Img.ThumbnailPending := False;
  If Assigned(ARequest.Bitmap) Then
  Begin
    Img.SetThumbnail(ARequest.Bitmap);
    ARequest.Bitmap := nil; // Image now owns the bitmap.
  End;
  C := ARequest.Index Mod grdImages.ColCount;
  R := ARequest.Index Div grdImages.ColCount;
  grdImages.InvalidateCell(C, R);
End;

Procedure TFrameImageGrid.LoadSettings(AInifile: TIniFile);
Begin

End;

Procedure TFrameImageGrid.SaveSettings(AInifile: TIniFile);
Begin

End;

End.
