Unit FramePDFViewers;

{-------------------------------------------------------------------------------
  Package   : IM_application
  Unit      : FramePDFViewers.pas
  Description
    Frame for browsing PDF pages with thumbnails and toolbar controls.

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
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls, ExtCtrls, ComCtrls,
  FrameBase, FrameImages, Graphics;

Type

  { TFramePDFViewer }

  TFramePDFViewer = Class(TfmeBase)
    btnInfo: TToolButton;
    ilThumbs: TImageList;
    ilToolbar: TImageList;
    ilToolbarDisabled: TImageList;
    lvThumbNails: TListView;
    pnlAll: TPanel;
    pnlImages: TPanel;
    pnlToolbar: TPanel;
    splThumbNails: TSplitter;
    sbPDF: TStatusBar;
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
    Procedure btnFirstPageClick(Sender: TObject);
    Procedure btnFitPageClick(Sender: TObject);
    Procedure btnFitWidthClick(Sender: TObject);
    Procedure btnInfoClick(Sender: TObject);
    Procedure btnLastPageClick(Sender: TObject);
    Procedure btnNextPageClick(Sender: TObject);
    Procedure btnPrevPageClick(Sender: TObject);
    Procedure btnRotateCCWClick(Sender: TObject);
    Procedure btnRotateCWClick(Sender: TObject);
    Procedure btnZoom100Click(Sender: TObject);
    Procedure btnZoomInClick(Sender: TObject);
    Procedure btnZoomOutClick(Sender: TObject);
    Procedure lvThumbNailsCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; Var DefaultDraw: Boolean);
    Procedure lvThumbNailsData(Sender: TObject; Item: TListItem);
    Procedure lvThumbNailsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    Procedure sbPDFResize(Sender: TObject);
  Private
    FPicture: TPicture; // Cache for thumbnailer
    FActive: Boolean;
    FFilename: String;
    FImageMagickDir: String;
    fmeImages: TFrameImage;
    FPage: Integer;
    FPageCount: Integer;
    FPDFDir: String;
    FTempDir: String;
    FXPDFDir: String;
    Procedure CreateThumbnail(sFile: String; sThumb: String);
    Procedure SetFilename(AValue: String);

    // PDF Operations
    Function ConvertActivePageToImage: String;
    Function ConvertPageToImage(sDir: String; iPage: Integer): String;
    Procedure SetPage(AValue: Integer);

    Procedure OnScrollToNext(Sender: TObject; AScrollUp: Boolean);
  Public
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;

    Function Info: String;
    Procedure RefreshUI; Override;

    Property TempDir: String read FTempDir write FTempDir;
    Property PDFDir: String read FPDFDir;
    // This is the PDF image folder in the TEMP Dir
    Property Filename: String read FFilename write SetFilename;

    // Third Party SDKs - the power behind this code
    Property XPDFDir: String read FXPDFDir write FXPDFDir;
    Property ImageMagickDir: String read FImageMagickDir write FImageMagickDir;

    Property PageCount: Integer read FPageCount;
    Property Page: Integer read FPage write SetPage;

    Property Active: Boolean read FActive;
  End;

Implementation

Uses
  VersionSupport, Dialogs, OSSupport, Types,
  XPDFSupport, ImageMagickSupport;

{$R *.lfm}

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

  FXPDFDir := XPDFSupport.XPDFPath;

  FImageMagickDir := ImageMagickSupport.ImageMagickPath;

  FActive := False;

  FPicture := TPicture.Create;

  RefreshUI;
End;

Destructor TFramePDFViewer.Destroy;
Begin
  FPicture.Free;

  Inherited Destroy;
End;

Procedure TFramePDFViewer.btnInfoClick(Sender: TObject);
Begin
  ShowMessage(Info);
End;

Procedure TFramePDFViewer.SetPage(AValue: Integer);
Begin
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
  sFile := fmeImages.Filename;
  Run(Format('%s\convert.exe "%s" -rotate -90 "%s"', [FImageMagickDir, sFile, sFile]));
  fmeImages.Filename := '';
  fmeImages.Filename := sFile;
End;

Procedure TFramePDFViewer.btnRotateCWClick(Sender: TObject);
Var
  sFile: String;
Begin
  sFile := fmeImages.Filename;
  Run(Format('%s\convert.exe "%s" -rotate +90 "%s"', [FImageMagickDir, sFile, sFile]));
  fmeImages.Filename := '';
  fmeImages.Filename := sFile;
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
  Run(Format('%s\convert.exe "%s" -resize %dx%d "%s"', [FImageMagickDir, sFile,
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


Procedure TFramePDFViewer.SetFilename(AValue: String);
Begin
  If (FFilename <> AValue) Then
  Begin
    FActive := False;
    FPageCount := 0;
    fmeImages.Filename := '';  // Clear any the existing Picture

    If FileExists(AValue) Then
    Begin
      SetBusy;
      Try
        FPage := -1;  // Forces a reload of the Picture in case one is already loaded

        FFilename := AValue;
        FPDFDir := IncludeTrailingBackslash(TempDir) + ExtractFileNameOnly(FFilename);

        // Initialise the page count
        Info;

        // only show the thumbnails if we've more than one page
        lvThumbNails.Visible := FPageCount > 1;
        splThumbNails.Visible := lvThumbNails.Visible;
        splThumbNails.Left := lvThumbNails.Left + lvThumbNails.Width;

        FActive := True;

        Page := 1;
        lvThumbNails.Items.Count := FPageCount;
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
Begin
  pnlAll.Visible := FActive;

  If FActive Then
  Begin
    sbPDF.Panels[0].Text := FFilename;
    sbPDF.Panels[1].Text := Format('Page %d of %d', [FPage, FPageCount]);

    btnFirstPage.Enabled := FPage <> 1;
    btnLastPage.Enabled := FPage <> FPageCount;
    btnNextPage.Enabled := btnLastPage.Enabled;
    btnPrevPage.Enabled := btnFirstPage.Enabled;
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
(*
      Run(Format('%s\convert.exe -density 300 "%s[%d]" -quality 100 "%s.png"',
       [FImageMagickDir, FFilename, iPage, sFullFilename]));
*)
      // Generate the original .ppm file, which TImage won't load (Lazarus bug?)
      sError := Run(Format('%s\pdftopng.exe -f %d -l %d "%s" "%s\Page"',
        [FXPDFDir, iPage, iPage, FFilename, sDir]));

      sError := Trim(sError);

      If sError <> '' Then
        ShowMessage(sError);

      // Now convert the ppm file to a more handlable format
(*
      Run(Format('%s\convert.exe "%s.ppm" "%s.png"', [FImageMagickDir,
        sFullFilename, sFullFilename]));

      DeleteFile(Format('%s.ppm', [sFullFilename]));
*)
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
Begin
  slInfo := TStringList.Create;
  Try
    slInfo.Text := Run(Format('%s\pdfinfo.exe "%s"', [XPDFDir, FFilename]));

    For i := 0 To slInfo.Count - 1 Do
      slInfo[i] := StringReplace(slInfo[i], ': ', '=', [rfIgnoreCase]);

    Result := slInfo.Text;

    FPageCount := StrToIntDef(slInfo.Values['Pages'], 0);
  Finally
    slInfo.Free;
  End;
End;

Initialization
  InitializeXPDF;
  InitializeImageMagick;

End.
