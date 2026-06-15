Unit FrameVideoPlayers;

{-------------------------------------------------------------------------------
  Package   : IM_application
  Unit      : FrameVideoPlayers.pas
  Description
    Toolbar-driven video player frame hosting a TfmeVideoBase playback engine.

  Source
    Copyright (c) 2025-2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    ~2008: Creation date unknown, original local SVN repository lost
    2014-07-05: Uploaded to SourceForge/Package "Shared"
    2024-01-22: Migrated to Github. Refactored package to "IM_application"
    2025-11-29: Added LGPL-3.0-or-later license header
    2026-06-12: Refactored by OpenAI ChatGPT (GPT-5.5) from DirectShow host to TfmeVideoBase playback host

  License
    This file is part of IM_application.lpk.

    SPDX-License-Identifier: LGPL-3.0-or-later
-------------------------------------------------------------------------------}

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Menus, FrameBase, FrameVideoBase;

Type

  { TFrameVideoPlayer }

  TFrameVideoPlayer = Class(TfmeBase)
    btnGrab: TToolButton;
    btnOpenInExplorer: TToolButton;
    btnPause: TToolButton;
    btnPlay: TToolButton;
    dlgSaveLocation: TSelectDirectoryDialog;
    ilToolbar: TImageList;
    lblStatus: TLabel;
    lblTime: TLabel;
    mnuGrabAlwaysAsk: TMenuItem;
    mnuGrabOnlyAskOnce: TMenuItem;
    mnuGrabVideoFolder: TMenuItem;
    pmnuGrab: TPopupMenu;
    pnlToolbar: TPanel;
    pnlVideo: TPanel;
    tbVideo: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    btnRewind: TToolButton;
    btnFastForward: TToolButton;
    btnStepBack: TToolButton;
    btnStepForward: TToolButton;
    trackVideo: TTrackBar;
    Procedure btnFastForwardClick(Sender: TObject);
    Procedure btnGrabClick(Sender: TObject);
    Procedure btnOpenInExplorerClick(Sender: TObject);
    Procedure btnPauseClick(Sender: TObject);
    Procedure btnPlayClick(Sender: TObject);
    Procedure btnRewindClick(Sender: TObject);
    Procedure btnStepForwardClick(Sender: TObject);
    Procedure mnuGrabClick(Sender: TObject);
    Procedure pnlToolbarResize(Sender: TObject);
    Procedure btnStepBackClick(Sender: TObject);
    Procedure trackVideoChange(Sender: TObject);
    Procedure trackVideoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  Private
    FAutoplay: Boolean;
    FFilename: String;
    FLastImageFolder: String;
    FOnStop: TNotifyEvent;
    FPlaybackClass: TfmeVideoBaseClass;
    fmeVideo: TfmeVideoBase;

    FUpdatingTracker: Boolean;
    FLastSeekTick: QWord;

    Function EnsurePlaybackFrame: Boolean;
    Function GetFilename: String;
    Function GetShowLabel: Boolean;
    Function GetVideoFileCount: Integer;
    Procedure SetPlaybackClass(AValue: TfmeVideoBaseClass);
    Procedure SetShowLabel(AValue: Boolean);

    Procedure VideoPosition(Sender: TObject; PositionMS, DurationMS: TVideoTime);
    Procedure VideoStateChanged(Sender: TObject; State: TVideoState);
  Public
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure RefreshUI; Override;
    Function Load(Const AFilename: String): Boolean;
    Procedure Pause;

    Property Filename: String Read GetFilename;
    Property PlaybackClass: TfmeVideoBaseClass Read FPlaybackClass Write SetPlaybackClass;

    Property Autoplay: Boolean Read FAutoplay Write FAutoplay;
    Property ShowLabel: Boolean Read GetShowLabel Write SetShowLabel;
    Property OnStop: TNotifyEvent Read FOnStop Write FOnStop;

    Property PlaybackFrame: TfmeVideoBase Read fmeVideo;

    Property VideoFileCount: Integer Read GetVideoFileCount;
  End;

Implementation

Uses
  OSSupport, FormMain, Math, Clipbrd;

  {$R *.lfm}

  { TFrameVideoPlayer }

Constructor TFrameVideoPlayer.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  fmeVideo := nil;
  FPlaybackClass := nil;
  FFilename := '';
  FAutoplay := False;
  FLastImageFolder := '';
  FOnStop := nil;

  lblStatus.Caption := '';
  lblTime.Caption := '';

  FUpdatingTracker := False;
  FLastSeekTick := 0;

  trackVideo.Min := 0;
  trackVideo.Max := 0;
  trackVideo.Position := 0;

  RefreshUI;
End;

Destructor TFrameVideoPlayer.Destroy;
Begin
  FreeAndNil(fmeVideo);
  Inherited Destroy;
End;

Function TFrameVideoPlayer.EnsurePlaybackFrame: Boolean;
Begin
  Result := Assigned(fmeVideo);

  If Result Then
    Exit;

  If Not Assigned(FPlaybackClass) Then
    Exit(False);

  fmeVideo := FPlaybackClass.Create(pnlVideo);
  fmeVideo.Parent := pnlVideo;
  fmeVideo.Name := 'fmeVideo';
  fmeVideo.Align := alClient;

  fmeVideo.OnPosition := @VideoPosition;
  fmeVideo.OnStateChanged := @VideoStateChanged;

  If FFilename <> '' Then
    fmeVideo.Load(FFilename);

  Result := True;
End;

Procedure TFrameVideoPlayer.SetPlaybackClass(AValue: TfmeVideoBaseClass);
Begin
  If FPlaybackClass = AValue Then
    Exit;

  FreeAndNil(fmeVideo);
  FPlaybackClass := AValue;

  EnsurePlaybackFrame;

  If Assigned(fmeVideo) And FileExists(FFilename) And FAutoplay Then
    fmeVideo.Play;

  RefreshUI;
End;

Procedure TFrameVideoPlayer.RefreshUI;
Var
  bHasEngine, bHasFile, bCanSeek, bCanRate, bCanGrab: Boolean;
Begin
  Inherited RefreshUI;

  bHasEngine := Assigned(fmeVideo);
  bHasFile := bHasEngine And (fmeVideo.VideoFileCount > 0);
  bCanSeek := bHasEngine And fmeVideo.CanSeek;
  bCanRate := bHasEngine And fmeVideo.CanSetRate;
  bCanGrab := bHasEngine And fmeVideo.CanGrabBitmap;

  btnPlay.Enabled := bHasEngine And bHasFile And (fmeVideo.State In
    [vsStopped, vsPaused, vsEnded]);
  btnPause.Enabled := bHasEngine And bHasFile And (fmeVideo.State = vsPlaying);
  btnStepBack.Enabled := bCanSeek;
  btnStepForward.Enabled := bCanSeek;
  btnFastForward.Enabled := bCanRate;
  btnRewind.Enabled := bCanRate;
  btnGrab.Enabled := bCanGrab;
  btnOpenInExplorer.Enabled := bHasFile;

  trackVideo.Enabled := bHasEngine And bCanSeek;

  If bHasEngine Then
  Begin
    If fmeVideo.Rate = 1 Then
      btnFastForward.Hint := 'Play faster'
    Else
      btnFastForward.Hint :=
        Format('Play faster: Current rate %.1f times normal', [fmeVideo.Rate]);
  End;
End;

Function TFrameVideoPlayer.Load(Const AFilename: String): Boolean;
Begin
  FLastImageFolder := '';
  FFilename := AFilename;

  Result := False;

  If EnsurePlaybackFrame Then
    Result := fmeVideo.Load(FFilename);

  lblStatus.Caption := Format('File: %s', [FFilename]);
  MainForm.Status := Format('File: %s', [FFilename]);

  If FileExists(FFilename) And FAutoplay And Assigned(fmeVideo) Then
    fmeVideo.Play;

  RefreshUI;
End;

Procedure TFrameVideoPlayer.Pause;
Begin
  If Assigned(fmeVideo) And (fmeVideo.State = vsPlaying) Then
    fmeVideo.Pause;

  RefreshUI;
End;

Procedure TFrameVideoPlayer.btnPlayClick(Sender: TObject);
Begin
  If EnsurePlaybackFrame Then
  Begin
    If fmeVideo.Rate <> 1 Then
      fmeVideo.Rate := 1;

    If fmeVideo.State In [vsEmpty, vsStopped, vsEnded, vsError] Then
      fmeVideo.Load(FFilename);

    If fmeVideo.State = vsPaused Then
      fmeVideo.Resume
    Else
      fmeVideo.Play;
  End;

  RefreshUI;
End;

Procedure TFrameVideoPlayer.btnPauseClick(Sender: TObject);
Begin
  If Assigned(fmeVideo) Then
  Begin
    If fmeVideo.State = vsPaused Then
      fmeVideo.Resume
    Else If fmeVideo.State = vsPlaying Then
      fmeVideo.Pause;
  End;

  RefreshUI;
End;

Procedure TFrameVideoPlayer.btnFastForwardClick(Sender: TObject);
Begin
  If Assigned(fmeVideo) And fmeVideo.CanSetRate Then
    fmeVideo.Rate := fmeVideo.Rate * Sqrt(2);

  RefreshUI;
End;

Procedure TFrameVideoPlayer.btnRewindClick(Sender: TObject);
Begin
  If Assigned(fmeVideo) And fmeVideo.CanSetRate Then
    fmeVideo.Rate := -1 * Abs(fmeVideo.Rate * Sqrt(2));

  RefreshUI;
End;

Procedure TFrameVideoPlayer.btnStepBackClick(Sender: TObject);
Begin
  If Assigned(fmeVideo) And fmeVideo.CanSeek Then
  Begin
    If fmeVideo.State = vsPlaying Then
      fmeVideo.Pause;

    fmeVideo.Position := Max(0, fmeVideo.Position - 1000);
  End;

  RefreshUI;
End;

Procedure TFrameVideoPlayer.trackVideoChange(Sender: TObject);
Begin
  If FUpdatingTracker Then
    Exit;

  If Not Assigned(fmeVideo) Then
    Exit;

  If Not fmeVideo.CanSeek Then
    Exit;

  If (FLastSeekTick <> 0) And (GetTickCount64 - FLastSeekTick < 200) Then
    Exit;

  FLastSeekTick := GetTickCount64;

  fmeVideo.Position := trackVideo.Position;
End;

Procedure TFrameVideoPlayer.trackVideoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  NewPos: Integer;
Begin
  FLastSeekTick := 0;

  If Button <> mbLeft Then
    Exit;

  If trackVideo.Max <= trackVideo.Min Then
    Exit;

  NewPos :=
    trackVideo.Min + Round((X / trackVideo.ClientWidth) * (trackVideo.Max - trackVideo.Min));

  If NewPos < trackVideo.Min Then
    NewPos := trackVideo.Min
  Else If NewPos > trackVideo.Max Then
    NewPos := trackVideo.Max;

  trackVideo.Position := NewPos;

  If Assigned(fmeVideo) And fmeVideo.CanSeek Then
    fmeVideo.Position := NewPos;
End;

Procedure TFrameVideoPlayer.btnStepForwardClick(Sender: TObject);
Begin
  If Assigned(fmeVideo) And fmeVideo.CanSeek Then
    fmeVideo.Position := fmeVideo.Position + 5000;

  RefreshUI;
End;

Procedure TFrameVideoPlayer.btnGrabClick(Sender: TObject);
Var
  oBitmap: TBitmap;
  oJPEG: TJPEGImage;
  sPath, sFile, sSaveFile: String;
  i: Integer;
Begin
  If Not Assigned(fmeVideo) Then
    Exit;

  oBitmap := TBitmap.Create;
  Try
    If fmeVideo.GetBitmap(oBitmap) Then
    Begin
      If dlgSaveLocation.InitialDir = '' Then
        dlgSaveLocation.InitialDir :=
          IncludeTrailingBackslash(ExtractFilePath(fmeVideo.Filename));

      If mnuGrabOnlyAskOnce.Checked Then
      Begin
        If FLastImageFolder = '' Then
        Begin
          If dlgSaveLocation.Execute Then
          Begin
            sPath := IncludeTrailingBackslash(dlgSaveLocation.FileName);
            FLastImageFolder := sPath;
          End
          Else
            sPath := '';
        End
        Else
          sPath := FLastImageFolder;
      End
      Else If mnuGrabAlwaysAsk.Checked Then
      Begin
        If dlgSaveLocation.Execute Then
        Begin
          sPath := IncludeTrailingBackslash(dlgSaveLocation.FileName);
          FLastImageFolder := sPath;
        End
        Else
          sPath := '';
      End
      Else
        sPath := IncludeTrailingBackslash(ExtractFilePath(fmeVideo.Filename));

      If sPath <> '' Then
      Begin
        sFile := ExtractFileNameWithoutExt(ExtractFileNameOnly(fmeVideo.Filename));

        i := 0;
        Repeat
          sSaveFile := Format('%s%s-%d.jpg', [sPath, sFile, i]);
          Inc(i);
        Until Not FileExists(sSaveFile);

        Clipboard.Assign(oBitmap);

        oJPEG := TJPEGImage.Create;
        Try
          oJPEG.CompressionQuality := 100;
          oJPEG.Assign(oBitmap);
          oJPEG.SaveToFile(sSaveFile);
        Finally
          oJPEG.Free;
        End;

        lblStatus.Caption := 'Saved ' + sSaveFile;
        MainForm.Status := 'Saved ' + sSaveFile;
      End;
    End
    Else
    Begin
      lblStatus.Caption := 'Failed to grab image';
      MainForm.Status := 'Failed to grab image';
    End;
  Finally
    oBitmap.Free;
  End;

  RefreshUI;
End;

Procedure TFrameVideoPlayer.btnOpenInExplorerClick(Sender: TObject);
Begin
  If FFilename <> '' Then
    LaunchFile('explorer.exe', Format('/e,/select,"%s"', [FFilename]));
End;

Procedure TFrameVideoPlayer.mnuGrabClick(Sender: TObject);
Begin
  If Sender Is TMenuItem Then
    TMenuItem(Sender).Checked := True;
End;

Procedure TFrameVideoPlayer.pnlToolbarResize(Sender: TObject);
Begin
  tbVideo.Top := 0;
  tbVideo.Left := Max(0, (pnlToolbar.Width - tbVideo.Width) Div 2);
End;

Function TFrameVideoPlayer.GetFilename: String;
Begin
  Result := FFilename;
End;

Function TFrameVideoPlayer.GetShowLabel: Boolean;
Begin
  Result := lblStatus.Visible;
End;

Function TFrameVideoPlayer.GetVideoFileCount: Integer;
Begin
  If Assigned(fmeVideo) Then
    Result := fmeVideo.VideoFileCount
  Else
    Result := 0;
End;

Procedure TFrameVideoPlayer.SetShowLabel(AValue: Boolean);
Begin
  lblStatus.Visible := AValue;
End;

Procedure TFrameVideoPlayer.VideoPosition(Sender: TObject; PositionMS, DurationMS: TVideoTime);

  Function ToTime(Const ATimeMS: TVideoTime): String;
  Begin
    If ATimeMS < 0 Then
      Result := '--:--:--'
    Else
      Result := FormatDateTime('HH:mm:ss', (ATimeMS / 1000) / SecsPerDay);
  End;

Begin
  FUpdatingTracker := True;
  Try
    If DurationMS > 0 Then
      trackVideo.Max := DurationMS
    Else
      trackVideo.Max := 0;

    If PositionMS < 0 Then
      trackVideo.Position := 0
    Else If PositionMS > trackVideo.Max Then
      trackVideo.Position := trackVideo.Max
    Else
      trackVideo.Position := PositionMS;
  Finally
    FUpdatingTracker := False;
  End;

  lblTime.Caption := ToTime(PositionMS) + LineEnding + ToTime(DurationMS);
End;

Procedure TFrameVideoPlayer.VideoStateChanged(Sender: TObject; State: TVideoState);
Begin
  If State = vsEnded Then
    If Assigned(FOnStop) Then
      FOnStop(Self);

  RefreshUI;
End;

End.
