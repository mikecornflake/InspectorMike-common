Unit FramePDFViewer;

{-------------------------------------------------------------------------------
  Package   : IM_forms.media
  Unit      : FramePDFViewer.pas
  Description
    Frame for browsing PDF pages with thumbnails and toolbar controls.

  Source
    Copyright (c) 2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    ~2008: Creation date unknown, original local SVN repository lost
    2014-07-05: Uploaded to SourceForge/Package "Shared"
    2024-01-22: Migrated to Github.  Refactored package to "IM_application"
    2025-11-29: Added LGPL-3.0-or-later license header
    2026-06-19: Refactored into split InspectorMike package structure

  License
    This file is part of IM_forms.media.lpk.

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
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls, ExtCtrls, ComCtrls,
  FrameBase, FrameImages, Graphics, Dialogs, PDF;

Type

  { TFramePDFViewer }

  TFramePDFViewer = Class(TFrameBase)
    btnAddAttachment: TToolButton;
    btnDeleteAttachment: TToolButton;
    btnEditDescription: TToolButton;
    btnExportAttachment: TToolButton;
    btnImportAttachments: TToolButton;
    btnInfo: TToolButton;
    btnSavePDF2: TToolButton;
    btnSep1: TToolButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    ilThumbs: TImageList;
    ilToolbar: TImageList;
    ilToolbarDisabled: TImageList;
    ilPageControl: TImageList;
    lvAttachments: TListView;
    lvThumbNails: TListView;
    pcNavigation: TPageControl;
    pnlAll: TPanel;
    pnlAttachments: TPanel;
    pnlImages: TPanel;
    pnlToolbar: TPanel;
    splThumbNails: TSplitter;
    sbPDF: TStatusBar;
    tbAttachments: TToolBar;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    tsAttachments: TTabSheet;
    tvTOC: TTreeView;
    tsThumbnails: TTabSheet;
    tsTOC: TTabSheet;
    tbMain: TToolBar;
    btnFirstPage: TToolButton;
    btnPrevPage: TToolButton;
    btnNextPage: TToolButton;
    btnLastPage: TToolButton;
    ToolButton1: TToolButton;
    btnFitPage: TToolButton;
    btnFitWidth: TToolButton;
    btnZoom100: TToolButton;
    btnZoomIn: TToolButton;
    btnZoomOut: TToolButton;
    btnRotateCCW: TToolButton;
    btnRotateCW: TToolButton;
    ToolButton2: TToolButton;
    ToolButton5: TToolButton;
    Procedure btnAddAttachmentClick(Sender: TObject);
    Procedure btnDeleteAttachmentClick(Sender: TObject);
    Procedure btnEditDescriptionClick(Sender: TObject);
    Procedure btnExportAttachmentClick(Sender: TObject);
    Procedure btnFirstPageClick(Sender: TObject);
    Procedure btnFitPageClick(Sender: TObject);
    Procedure btnFitWidthClick(Sender: TObject);
    Procedure btnImportAttachmentsClick(Sender: TObject);
    Procedure btnInfoClick(Sender: TObject);
    Procedure btnLastPageClick(Sender: TObject);
    Procedure btnNextPageClick(Sender: TObject);
    Procedure btnPrevPageClick(Sender: TObject);
    Procedure btnRotateCCWClick(Sender: TObject);
    Procedure btnRotateCWClick(Sender: TObject);
    Procedure btnSavePDFClick(Sender: TObject);
    Procedure btnZoom100Click(Sender: TObject);
    Procedure btnZoomInClick(Sender: TObject);
    Procedure btnZoomOutClick(Sender: TObject);
    Procedure lvAttachmentsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    Procedure lvThumbNailsCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; Var DefaultDraw: Boolean);
    Procedure lvThumbNailsData(Sender: TObject; Item: TListItem);
    Procedure lvThumbNailsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    Procedure sbPDFResize(Sender: TObject);
    Procedure tvTOCSelectionChanged(Sender: TObject);
  Private
    FPicture: TPicture; // Cache for thumbnailer
    FActive: Boolean;
    FFilename: String;
    fmeImages: TFrameImage;
    FPage: Integer;
    FPageCount: Integer;
    FPDFDir: String;
    FTempDir: String;

    FAttachments: TPDFAttachments;
    FDirty: Boolean;

    Procedure CreateThumbnail(sFile: String; sThumb: String);
    Procedure SetFilename(AFilename: String);

    // PDF Operations
    Function ConvertActivePageToImage: String;
    Function ConvertPageToImage(sDir: String; iPage: Integer): String;
    Procedure SetPage(AValue: Integer);

    Procedure OnScrollToNext(Sender: TObject; AScrollUp: Boolean);
    Procedure UpdateAttachmentsList;
  Public
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;

    Function Info: String;
    Procedure RefreshUI; Override;

    Property TempDir: String Read FTempDir Write FTempDir;
    Property PDFDir: String Read FPDFDir;

    // This is the PDF image folder in the TEMP Dir
    Property Filename: String Read FFilename Write SetFilename;

    Property PageCount: Integer Read FPageCount;
    Property Page: Integer Read FPage Write SetPage;

    Property Active: Boolean Read FActive;
    Property Dirty: Boolean Read FDirty;
  End;

Implementation

Uses
  VersionSupport, FileSupport, OSSupport, Types,
  XPDFSupport, ImageMagickSupport, qpdfSupport, Math;

  {$R *.lfm}

Const
  FILES_PDF: String = 'PDF Files|*.pdf|All files|*.*';
  FILES_ALL: String = 'All files|*.*';


  { TFramePDFViewer }

Constructor TFramePDFViewer.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  fmeImages := TFrameImage.Create(Self);
  fmeImages.Parent := pnlImages;
  fmeImages.Align := alClient;
  fmeImages.Name := 'fmeImages';
  fmeImages.PDF := True;
  fmeImages.Stretch := True;
  fmeImages.OnScrollToNext := @OnScrollToNext;

  //  FTempDir := IncludeTrailingBackslash(Application.Location) + 'Temp';
  FTempDir := IncludeTrailingBackslash(GetTempDir(False)) +
    ChangeFileExt(ExtractFileName(Application.ExeName), '');

  FActive := False;

  FPicture := TPicture.Create;

  FAttachments := TPDFAttachments.Create(True);
  FDirty := False;


  RefreshUI;
End;

Destructor TFramePDFViewer.Destroy;
Begin
  FreeAndNil(FPicture);
  FreeAndNil(FAttachments);

  Inherited Destroy;
End;

Procedure TFramePDFViewer.btnInfoClick(Sender: TObject);
Begin
  ShowMessage(Info);
End;

Procedure TFramePDFViewer.SetPage(AValue: Integer);
Begin
  AValue := EnsureRange(AValue, 1, FPageCount);
  If FPage <> AValue Then
  Begin
    SetBusy;
    Try
      FPage := AValue;

      lvThumbNails.Refresh;

      fmeImages.Filename := ConvertActivePageToImage;
      RefreshUI;
    Finally
      ClearBusy;
    End;
  End;
End;

Procedure TFramePDFViewer.OnScrollToNext(Sender: TObject; AScrollUp: Boolean);
Begin
  If AScrollUp And btnPrevPage.Enabled Then
    btnPrevPage.Click
  Else If Not AScrollUp And btnNextPage.Enabled Then
    btnNextPage.Click;
End;

Procedure TFramePDFViewer.btnLastPageClick(Sender: TObject);
Begin
  Page := FPageCount;
End;

Procedure TFramePDFViewer.btnNextPageClick(Sender: TObject);
Begin
  Page := Page + 1;
  fmeImages.ScrollToTop;
End;

Procedure TFramePDFViewer.btnPrevPageClick(Sender: TObject);
Begin
  Page := Page - 1;
  fmeImages.ScrollToBottom;
End;

Procedure TFramePDFViewer.btnRotateCCWClick(Sender: TObject);
Var
  sFile: String;
Begin
  If ImageMagickExe = '' Then
    Exit;

  sFile := fmeImages.Filename;
  RunAndCapture(Format('"%s" mogrify -rotate -90 "%s"', [ImageMagickExe, sFile]));
  fmeImages.Filename := '';
  fmeImages.Filename := sFile;

  DeleteFile(ChangeFileExt(sFile, '-thumb.png'));
  If lvThumbNails.Visible Then
    lvThumbNails.Refresh;
End;

Procedure TFramePDFViewer.btnRotateCWClick(Sender: TObject);
Var
  sFile: String;
Begin
  If ImageMagickExe = '' Then
    Exit;

  sFile := fmeImages.Filename;
  RunAndCapture(Format('"%s" mogrify -rotate +90 "%s"', [ImageMagickExe, sFile]));
  fmeImages.Filename := '';
  fmeImages.Filename := sFile;

  DeleteFile(ChangeFileExt(sFile, '-thumb.png'));
  If lvThumbNails.Visible Then
    lvThumbNails.Refresh;
End;

Procedure TFramePDFViewer.btnZoom100Click(Sender: TObject);
Begin
  fmeImages.Zoom100;

  RefreshUI;
End;

Procedure TFramePDFViewer.btnZoomInClick(Sender: TObject);
Begin
  fmeImages.ZoomIn;

  RefreshUI;
End;

Procedure TFramePDFViewer.btnZoomOutClick(Sender: TObject);
Begin
  fmeImages.ZoomOut;

  RefreshUI;
End;

Procedure TFramePDFViewer.CreateThumbnail(sFile: String; sThumb: String);
Begin
  If ImageMagickExe = '' Then
    Exit;

  RunAndCapture(Format('"%s" "%s" -resize %dx%d "%s"', [ImageMagickExe, sFile,
    3 * (ilThumbs.Width - 8), 3 * (ilThumbs.Height - 8), sThumb]));
End;

Procedure TFramePDFViewer.lvThumbNailsCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; Var DefaultDraw: Boolean);
Var
  aRect, aRect2: TRect;
  sFile: String;
  sThumb: String;
Begin
  sFile := ConvertPageToImage(FPDFDir, Item.Index + 1);
  sThumb := ChangeFileExt(sFile, '-thumb.png');

  If Not FileExists(sThumb) Then
    CreateThumbnail(sFile, sThumb);

  If Not FileExists(sThumb) Then
  Begin
    aRect := Item.DisplayRect(drBounds);

    If Item.Selected Then
      Sender.Canvas.Brush.Color := clGray
    Else
      Sender.Canvas.Brush.Color := clWhite;

    Sender.Canvas.FillRect(aRect);

    Sender.Canvas.Pen.Color := clSilver;
    Sender.Canvas.Rectangle(aRect);

    Sender.Canvas.TextOut(aRect.Left + 6, aRect.Top + 6,
      IntToStr(Item.Index + 1));

    DefaultDraw := False;
    Exit;
  End;

  FPicture.LoadFromFile(sThumb);

  aRect := Item.DisplayRect(drBounds);

  // create a smaller rect for the thumbnail, so we have room to paint a
  // selection border if required
  aRect2 := aRect;
  InflateRect(aRect2, -4, -4);

  // paint the selection border
  If Item.Selected Then
    Sender.Canvas.Brush.Color := clGray
  Else
    Sender.Canvas.Brush.Color := clWhite;
  Sender.Canvas.FillRect(aRect);

  // paint the thumbnail
  Sender.Canvas.StretchDraw(aRect2, FPicture.Graphic);

  // add a page number to the thumbnail
  Sender.Canvas.TextOut(aRect.Left + 4, aRect.Top, IntToStr(Item.Index + 1));

  DefaultDraw := False;
End;

Procedure TFramePDFViewer.lvThumbNailsData(Sender: TObject; Item: TListItem);
Begin
  If FActive Then
    Item.Selected := (Item.Index = FPage - 1);

  If Item.Selected Then
    Item.MakeVisible(False);
End;

Procedure TFramePDFViewer.lvThumbNailsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
Begin
  If Assigned(Item) Then
    If Item.Selected And (Page <> Item.Index + 1) Then
      Page := Item.Index + 1;
End;

Procedure TFramePDFViewer.sbPDFResize(Sender: TObject);
Begin
  sbPDF.Panels[0].Width := sbPDF.Width - sbPDF.Panels[1].Width;
End;

Procedure TFramePDFViewer.tvTOCSelectionChanged(Sender: TObject);
Var
  iPage: Integer;
Begin
  If Assigned(tvTOC.Selected) Then
  Begin
    iPage := PtrInt(tvTOC.Selected.Data);
    SetPage(iPage);
  End;
End;

Procedure TFramePDFViewer.btnFirstPageClick(Sender: TObject);
Begin
  Page := 1;
End;

Procedure TFramePDFViewer.btnFitPageClick(Sender: TObject);
Begin
  fmeImages.ZoomToPage;

  RefreshUI;
End;

Procedure TFramePDFViewer.btnFitWidthClick(Sender: TObject);
Begin
  fmeImages.ZoomToWidth := True;

  RefreshUI;
End;


Procedure TFramePDFViewer.SetFilename(AFilename: String);
Begin
  If (FFilename <> AFilename) Then
  Begin
    FActive := False;
    FPageCount := 0;
    fmeImages.Filename := '';  // Clear the existing Picture

    If FileExists(AFilename) Then
    Begin
      SetBusy;
      Try
        FAttachments.Clear;
        FDirty := False;

        FPage := -1;  // Forces a reload of the Picture in case one is already loaded

        FFilename := AFilename;
        FPDFDir := IncludeTrailingBackslash(TempDir) + ExtractFileNameOnly(FFilename);

        // Initialise the page count
        // TODO Simplify this
        Info;

        FActive := True;

        Page := 1;
        lvThumbNails.Items.Count := FPageCount;

        qpdfListAttachments(AFilename, FAttachments);
        UpdateAttachmentsList;

        qpdfPopulateTOC(AFilename, tvTOC);

        If tvTOC.Items.Count>0 Then
          pcNavigation.ActivePage := tsTOC
        Else
          pcNavigation.ActivePage := tsThumbnails;
      Finally
        ClearBusy;
      End;
    End
    Else
      FFilename := '';

    RefreshUI;
  End;
End;

Procedure TFramePDFViewer.RefreshUI;
Var
  bHasqdf, bSelected: Boolean;
Begin
  pnlAll.Visible := FActive;

  btnAddAttachment.Enabled := False;
  btnImportAttachments.Enabled := False;
  btnDeleteAttachment.Enabled := False;
  btnEditDescription.Enabled := False;
  btnExportAttachment.Enabled := False;
  btnSavePDF2.Enabled := False;

  If FActive Then
  Begin
    sbPDF.Panels[0].Text := FFilename;
    sbPDF.Panels[1].Text := Format('Page %d of %d', [FPage, FPageCount]);

    btnFirstPage.Enabled := FPage <> 1;
    btnLastPage.Enabled := FPage <> FPageCount;
    btnNextPage.Enabled := btnLastPage.Enabled;
    btnPrevPage.Enabled := btnFirstPage.Enabled;

    bHasqdf := qpdfAvailable;
    bSelected := bHasqdf And Assigned(lvAttachments.Selected) And (lvAttachments.SelCount = 1);

    btnAddAttachment.Enabled := bHasqdf;
    btnEditDescription.Enabled := bSelected;
    btnDeleteAttachment.Enabled := bSelected;
    btnExportAttachment.Enabled := bSelected;
    btnImportAttachments.Enabled := bHasqdf;
    btnSavePDF2.Enabled := FDirty;
  End
  Else
  Begin
    sbPDF.Panels[0].Text := 'No file loaded';
    sbPDF.Panels[1].Text := '';
  End;

  If fmeImages.Stretch Then
  Begin
    btnFitPage.Down := True;
    btnFitWidth.Down := False;
    btnZoom100.Down := False;
  End
  Else If fmeImages.ZoomToWidth Then
  Begin
    btnFitPage.Down := False;
    btnFitWidth.Down := True;
    btnZoom100.Down := False;
  End
  Else If fmeImages.Scale = 1 Then
  Begin
    btnFitPage.Down := False;
    btnFitWidth.Down := False;
    btnZoom100.Down := True;
  End
  Else
  Begin
    btnFitPage.Down := False;
    btnFitWidth.Down := False;
    btnZoom100.Down := False;
  End;
End;

Function TFramePDFViewer.ConvertActivePageToImage: String;
Begin
  Result := ConvertPageToImage(FPDFDir, FPage);
End;

Function TFramePDFViewer.ConvertPageToImage(sDir: String; iPage: Integer): String;
Var
  sError: String;
  sFullFilename: String;
  sFile: String;
Begin
  SetBusy;
  Try
    sDir := ExcludeTrailingBackslash(sDir);
    ForceDirectory(sDir);

    sFile := Format('\Page-%0:6d', [iPage]);
    sFile := StringReplace(sFile, ' ', '0', [rfReplaceAll]);
    sFullFilename := sDir + sFile;

    If Not FileExists(sFullFilename + '.png') Then
    Begin
      sError := RunAndCapture(Format('"%s" -f %d -l %d "%s" "%s\Page"',
        [PDFtoPNGExe, iPage, iPage, FFilename, sDir]));

      sError := Trim(sError);

      If sError <> '' Then
        ShowMessage(sError);
    End;

    Result := sFullFilename + '.png';
  Finally
    ClearBusy;
  End;
End;

Function TFramePDFViewer.Info: String;
Var
  slInfo: TStringList;
  i: Integer;
  oPDFAttachments: TPDFAttachments;
  oPDFAttachment: TPDFAttachment;
Begin
  slInfo := TStringList.Create;
  Try
    slInfo.Text := XPDFInfo(FFilename);

    For i := 0 To slInfo.Count - 1 Do
      slInfo[i] := StringReplace(slInfo[i], ': ', '=', [rfIgnoreCase]);

    Result := slInfo.Text;

    FPageCount := StrToIntDef(slInfo.Values['Pages'], 0);

    If qpdfAvailable Then
    Begin
      Result := Result + LineEnding;

      oPDFAttachments := TPDFAttachments.Create(True);
      Try
        qpdfListAttachments(FFilename, oPDFAttachments);

        If oPDFAttachments.Count = 0 Then
          Result := Result + 'No attachments in PDF'
        Else
        Begin
          Result := Result + Format('%d attachments: ', [oPDFAttachments.Count]) + LineEnding;

          For oPDFAttachment In oPDFAttachments Do
            Result := Result + oPDFAttachment.DisplayText + LineEnding;
        End;
      Finally
        oPDFAttachments.Free;
      End;
    End;
  Finally
    slInfo.Free;
  End;
End;

Procedure TFramePDFViewer.btnAddAttachmentClick(Sender: TObject);
Var
  oAttachment, oExistingAttachment: TPDFAttachment;
  sFilename, sAttachmentKey, sDescription: String;
Begin
  dlgOpen.Filter := FILES_ALL;

  If dlgOpen.Execute Then
  Begin
    sFilename := dlgOpen.FileName;
    sAttachmentKey := ExtractFileName(sFilename);

    oExistingAttachment := qpdfFindAttachment(FAttachments, sAttachmentKey);

    If Assigned(oExistingAttachment) Then
    Begin
      If MessageDlg('Overwrite Attachment', Format(
        'An attachment named "%s" already exists.' + LineEnding + LineEnding +
        'Do you want to overwrite the existing attachment?', [sAttachmentKey]),
        mtConfirmation, [mbYes, mbNo], 0) = mrNo Then
        Exit;

      sDescription := oExistingAttachment.Description;

      FAttachments.Remove(oExistingAttachment);
    End
    Else
      sDescription := '';

    oAttachment := TPDFAttachment.Create;
    oAttachment.SourceFilename := sFilename;
    oAttachment.Filename := sAttachmentKey;
    oAttachment.Description := sDescription;
    oAttachment.CreationDate := FileCreationDate(sFilename);
    oAttachment.ModificationDate := FileModificationDate(sFilename);

    // Creation time is not available on every filesystem/platform.
    If oAttachment.CreationDate = 0 Then
      oAttachment.CreationDate := oAttachment.ModificationDate;

    FAttachments.Add(oAttachment);

    FDirty := True;

    UpdateAttachmentsList;
  End;
End;

Procedure TFramePDFViewer.btnDeleteAttachmentClick(Sender: TObject);
Var
  oAttachment: TPDFAttachment;
Begin
  If lvAttachments.SelCount <> 1 Then
    Exit;

  oAttachment := TPDFAttachment(lvAttachments.Selected.Data);

  If Not Assigned(oAttachment) Then
    Exit;

  If (MessageDlg('Delete Attachment', Format(
    'Are you sure you wish to delete the attachment?' + LineEnding + LineEnding +
    '%s', [oAttachment.Filename]), mtConfirmation, [mbYes, mbNo], 0) = mrYes) Then
  Begin
    FAttachments.Remove(oAttachment);

    FDirty := True;
    UpdateAttachmentsList;
  End;
End;

Procedure TFramePDFViewer.btnEditDescriptionClick(Sender: TObject);
Var
  oAttachment: TPDFAttachment;
  sDescription: String;
Begin
  If lvAttachments.SelCount <> 1 Then
    Exit;

  oAttachment := TPDFAttachment(lvAttachments.Selected.Data);

  If Not Assigned(oAttachment) Then
    Exit;

  sDescription := oAttachment.Description;

  If InputQuery('Edit Description', 'Description:', sDescription) Then
  Begin
    oAttachment.Description := sDescription;
    lvAttachments.Selected.SubItems[1] := sDescription;
    FDirty := True;

    RefreshUI;
  End;
End;

Procedure TFramePDFViewer.btnExportAttachmentClick(Sender: TObject);
Var
  oAttachment: TPDFAttachment;
  sFilename: String;
Begin
  If lvAttachments.SelCount <> 1 Then
    Exit;

  oAttachment := TPDFAttachment(lvAttachments.Selected.Data);

  If Not Assigned(oAttachment) Then
    Exit;

  sFilename := IncludeSlash(ExtractFileDir(FFilename)) + oAttachment.Filename;

  dlgSave.Filter := FILES_ALL;
  dlgSave.Options := dlgSave.Options + [ofOverwritePrompt];
  dlgSave.FileName := sFilename;

  If dlgSave.Execute Then
  Begin
    sFilename := dlgSave.FileName;
    If qpdfExtractAttachment(FFilename, oAttachment.Filename, sFilename) Then
      ShowMessageFmt('Attachment "%s" has been exported as:%s %s',
        [oAttachment.Filename, LineEnding, sFilename]);
  End;
End;

Procedure TFramePDFViewer.btnImportAttachmentsClick(Sender: TObject);
Var
  oImportAttachments: TPDFAttachments;
  oImportAttachment: TPDFAttachment;
  oExistingAttachment: TPDFAttachment;
  oNextAttachment: TPDFAttachment;
Begin
  dlgOpen.Filter := FILES_PDF;

  If Not dlgOpen.Execute Then
    Exit;

  oImportAttachments := TPDFAttachments.Create(True);
  Try
    If Not qpdfLoadAttachmentsForImport(dlgOpen.FileName, FTempDir, oImportAttachments) Then
    Begin
      MessageDlg('Import Attachments', 'The attachments could not be read from the selected PDF.',
        mtError, [mbOK], 0);

      Exit;
    End;

    While oImportAttachments.Count > 0 Do
    Begin
      oImportAttachment := oImportAttachments[0];

      oExistingAttachment :=
        qpdfFindAttachment(FAttachments, oImportAttachment.Filename);

      If Assigned(oExistingAttachment) Then
      Begin
        If MessageDlg('Replace Attachment', Format(
          'An attachment named "%s" already exists.' + LineEnding + LineEnding +
          'Do you want to replace it with the attachment ' + 'from the selected PDF?',
          [oImportAttachment.Filename]), mtConfirmation, [mbYes, mbNo], 0) = mrNo Then
        Begin
          // Removing it from the owning import list also frees it.
          oImportAttachments.Delete(0);
          Continue;
        End;

        FAttachments.Remove(oExistingAttachment);
      End;

      // Transfer ownership from oImportAttachments to FAttachments.
      oNextAttachment := oImportAttachments.Extract(oImportAttachment);

      FAttachments.Add(oNextAttachment);
      FDirty := True;
    End;

    If FDirty Then
    Begin
      UpdateAttachmentsList;
      RefreshUI;
    End;
  Finally
    oImportAttachments.Free;
  End;
End;

Procedure TFramePDFViewer.btnSavePDFClick(Sender: TObject);
Var
  sTemp: String;
Begin
  If Not FDirty Then
    Exit;

  If Not FileExists(FFilename) Then
    Exit;

  OSSupport.SetBusy;
  Try
    If Not qpdfWriteAttachments(FFilename, FAttachments) Then
    Begin
      MessageDlg('Save Attachments', 'The PDF attachments could not be saved.' +
        LineEnding + LineEnding + 'The original PDF has not been changed.',
        mtError, [mbOK], 0);

      Exit;
    End;

    // Reload from the finished PDF. This clears SourceFilename from
    // newly added attachments and verifies what qpdf actually wrote.
    sTemp := FFilename;
    Filename := '';
    Filename := sTemp;

    MessageDlg('Save Attachments', 'The PDF attachments have been saved.',
      mtInformation, [mbOK], 0);
  Finally
    OSSupport.ClearBusy;
  End;
End;

Procedure TFramePDFViewer.lvAttachmentsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
Begin
  RefreshUI;
End;

Procedure TFramePDFViewer.UpdateAttachmentsList;
Var
  oAttachment: TPDFAttachment;
  oItem: TListItem;
Begin
  lvAttachments.Items.Clear;

  If Not FileExists(FFilename) Then
    Exit;

  lvAttachments.BeginUpdate;
  lvAttachments.Items.BeginUpdate;
  Try
    For oAttachment In FAttachments Do
    Begin
      oItem := lvAttachments.Items.Add;

      oItem.Caption := oAttachment.Filename;
      oItem.SubItems.Add(oAttachment.CreationDateAsText);
      oItem.SubItems.Add(oAttachment.Description);
      oItem.Data := oAttachment;
    End;
  Finally
    lvAttachments.EndUpdate;
    lvAttachments.Items.EndUpdate;
  End;

  // Changing selection forces a RefreshUI
  If lvAttachments.Items.Count > 0 Then
    lvAttachments.Selected := lvAttachments.Items[0]
  Else
    RefreshUI;
End;

Initialization
  InitializeXPDF;
  InitializeImageMagick;
  Initializeqpdf;

End.
