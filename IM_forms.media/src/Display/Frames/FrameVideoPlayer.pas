Unit FrameVideoPlayer;

{-------------------------------------------------------------------------------
  Package   : IM_forms.media
  Unit      : FrameVideoPlayer.pas
  Description
    Toolbar-driven video player frame hosting a TFrameVideoBase playback engine.

  Source
    Copyright (c) 2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    ~2008: Creation date unknown, original local SVN repository lost
    2014-07-05: Uploaded to SourceForge/Package "Shared"
    2024-01-22: Migrated to Github. Refactored package to "IM_application"
    2025-11-29: Added LGPL-3.0-or-later license header
    2026-06-12: Refactored by OpenAI ChatGPT (GPT-5.5) from DirectShow host to TFrameVideoBase playback host
    2026-06-19: Refactored into split InspectorMike package structure

  License
    This file is part of IM_forms.media.lpk.

    SPDX-License-Identifier: LGPL-3.0-or-later
-------------------------------------------------------------------------------}

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, Menus, ActnList, FrameBase, FrameVideoBase,
  Types;

Type

  { TFrameVideoPlayer }

  TFrameVideoPlayer = Class(TFrameBase)
    actCopyToClipboard: TAction;
    actStepBack: TAction;
    actStepForward: TAction;
    actPlayFaster: TAction;
    actPlaySlower: TAction;
    actPlay: TAction;
    actPause: TAction;
    actResetRate: TAction;
    actPlayPause: TAction;
    actVideo: TActionList;
    btnGrab: TToolButton;
    btnOpenInExplorer: TToolButton;
    btnPause: TToolButton;
    btnPlay: TToolButton;
    dlgSaveLocation: TSelectDirectoryDialog;
    ilToolbar: TImageList;
    lblStatus: TLabel;
    lblTime: TLabel;
    mnuCopyToClipboard: TMenuItem;
    Separator1: TMenuItem;
    mnuResetRate: TMenuItem;
    mnuPlayPause: TMenuItem;
    mnuGrabAlwaysAsk: TMenuItem;
    mnuGrabOnlyAskOnce: TMenuItem;
    mnuGrabVideoFolder: TMenuItem;
    pmnuGrab: TPopupMenu;
    pnlToolbar: TPanel;
    pnlVideo: TPanel;
    pmnuPlayer: TPopupMenu;
    tbVideo: TToolBar;
    btnSep2: TToolButton;
    btnSep1: TToolButton;
    btnSep3: TToolButton;
    btnPlaySlower: TToolButton;
    btnPlayFaster: TToolButton;
    btnStepBack: TToolButton;
    btnStepForward: TToolButton;
    trackVideo: TTrackBar;
    Procedure actCopyToClipboardExecute(Sender: TObject);
    Procedure actPlayPauseExecute(Sender: TObject);
    Procedure actResetRateExecute(Sender: TObject);
    Procedure actPlayFasterClick(Sender: TObject);
    Procedure btnGrabClick(Sender: TObject);
    Procedure btnOpenInExplorerClick(Sender: TObject);
    Procedure actPauseClick(Sender: TObject);
    Procedure actPlayClick(Sender: TObject);
    Procedure actPlaySlowerClick(Sender: TObject);
    Procedure actStepForwardClick(Sender: TObject);
    Procedure mnuGrabClick(Sender: TObject);
    Procedure pnlToolbarResize(Sender: TObject);
    Procedure actStepBackClick(Sender: TObject);
    Procedure pnlVideoMouseEnter(Sender: TObject);
    Procedure pnlVideoMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; Var Handled: Boolean);
    Procedure trackVideoChange(Sender: TObject);
    Procedure trackVideoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  Private
    FAutoplay: Boolean;
    FFilename: String;
    FLastImageFolder: String;
    FOnStop: TNotifyEvent;
    FVideoEngineClass: TFrameVideoBaseClass;
    fmeVideo: TFrameVideoBase;

    FUpdatingTracker: Boolean;
    FLastSeekTick: QWord;

    Function EnsurePlaybackFrame: Boolean;
    Function GetFilename: String;
    Function GetShowLabel: Boolean;
    Function GetVideoFileCount: Integer;
    Procedure SetAutoplay(AValue: Boolean);
    Procedure SetVideoEngineClass(AValue: TFrameVideoBaseClass);
    Procedure SetShowLabel(AValue: Boolean);
    Function StepDelta: Integer;

    Procedure VideoPosition(Sender: TObject; PositionMS, DurationMS: TVideoTime);
    Procedure VideoStateChanged(Sender: TObject; State: TVideoState);
  Public
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure RefreshUI; Override;
    Function Load(Const AFilename: String; AChannel: String = '';
      AStartDateTime: TDateTime = 0): Boolean;
    Function Clear: Boolean;
    Procedure Pause;

    Property Filename: String Read GetFilename;
    Property VideoEngineClass: TFrameVideoBaseClass Read FVideoEngineClass
      Write SetVideoEngineClass;

    Property Autoplay: Boolean Read FAutoplay Write SetAutoplay;
    Property ShowLabel: Boolean Read GetShowLabel Write SetShowLabel;
    Property OnStop: TNotifyEvent Read FOnStop Write FOnStop;

    Property PlaybackFrame: TFrameVideoBase Read fmeVideo;

    Property VideoFileCount: Integer Read GetVideoFileCount;
  End;

Const
  MIN_RATE = 0.25;
  MAX_RATE = 8.0;
  RATE_STEP = Sqrt(2);

Implementation

Uses
  OSSupport, Math, Clipbrd;


  {$R *.lfm}

  { TFrameVideoPlayer }

Constructor TFrameVideoPlayer.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  fmeVideo := nil;
  FVideoEngineClass := nil;
  FFilename := '';
  FAutoplay := True;
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

  If Not Assigned(FVideoEngineClass) Then
    Exit(False);

  fmeVideo := FVideoEngineClass.Create(pnlVideo);
  fmeVideo.Parent := pnlVideo;
  fmeVideo.Name := 'fmeVideo';
  fmeVideo.Align := alClient;
  fmeVideo.Autoplay := FAutoplay;

  fmeVideo.OnPosition := @VideoPosition;
  fmeVideo.OnStateChanged := @VideoStateChanged;

  If FFilename <> '' Then
    fmeVideo.Load(FFilename);

  Result := True;
End;

Procedure TFrameVideoPlayer.SetVideoEngineClass(AValue: TFrameVideoBaseClass);
Begin
  If FVideoEngineClass = AValue Then
    Exit;

  FreeAndNil(fmeVideo);
  FVideoEngineClass := AValue;

  EnsurePlaybackFrame;

  If Assigned(fmeVideo) And FileExists(FFilename) And FAutoplay Then
    fmeVideo.Play;

  RefreshUI;
End;

Procedure TFrameVideoPlayer.RefreshUI;
Var
  bHasFile, bCanSeek, bCanRate, bCanGrab: Boolean;
  sExtra: String;
Begin
  Inherited RefreshUI;

  If Not Assigned(fmeVideo) Then
  Begin
    actPlay.Enabled := False;
    actPause.Enabled := False;
    actStepBack.Enabled := False;
    actStepForward.Enabled := False;
    actPlayFaster.Enabled := False;
    actPlaySlower.Enabled := False;
    btnGrab.Enabled := False;
    btnOpenInExplorer.Enabled := False;

    actPlayPause.Enabled := False;
    actResetRate.Enabled := False;
    actCopyToClipboard.Enabled := False;

    Exit;
  End;

  bHasFile := fmeVideo.HasVideo;
  bCanSeek := bHasFile And fmeVideo.CanSeek;
  bCanRate := bHasFile And fmeVideo.CanSetRate;
  bCanGrab := bHasFile And fmeVideo.CanGrabFrame;

  actPlay.Enabled := bHasFile And (fmeVideo.State In [vsStopped, vsPaused, vsEnded]);
  actPause.Enabled := bHasFile And (fmeVideo.State = vsPlaying);
  actStepBack.Enabled := bCanSeek;
  actStepForward.Enabled := bCanSeek;
  actPlayFaster.Enabled := bCanRate;
  actPlaySlower.Enabled := bCanRate;

  actPlayPause.Enabled := bHasFile;
  actResetRate.Enabled := bCanSeek And (Abs(fmeVideo.Rate - 1.0) > 0.01);

  If Not actPlay.Enabled Then
    actPlayPause.ImageIndex := 5 //pause
  Else
    actPlayPause.ImageIndex := 0;//play

  btnGrab.Enabled := bCanGrab;
  btnOpenInExplorer.Enabled := bHasFile;
  actCopyToClipboard.Enabled := bCanGrab;

  trackVideo.Enabled := bCanSeek;

  actPlayFaster.Hint := 'Play faster: ';
  actPlaySlower.Hint := 'Play slower: ';

  sExtra := Format('Current rate %.2f', [fmeVideo.Rate]);

  If actResetRate.Enabled Then
    sExtra := sExtra + LineEnding + 'Press Pause, then Play to reset rate';

  actPlayFaster.Hint := actPlayFaster.Hint + sExtra;
  actPlaySlower.Hint := actPlaySlower.Hint + sExtra;
End;

Function TFrameVideoPlayer.Load(Const AFilename: String; AChannel: String;
  AStartDateTime: TDateTime): Boolean;
Begin
  FLastImageFolder := '';
  FFilename := AFilename;

  Result := False;

  If EnsurePlaybackFrame Then
  Begin
    fmeVideo.Autoplay := FAutoplay;
    Result := fmeVideo.Load(FFilename, AChannel, AStartDateTime);
  End;

  lblStatus.Caption := Format('File: %s', [FFilename]);
  RefreshUI;
End;

Function TFrameVideoPlayer.Clear: Boolean;
Begin
  If Assigned(fmeVideo) Then
    Result := fmeVideo.Clear
  Else
    Result := True;
End;

Procedure TFrameVideoPlayer.Pause;
Begin
  If Assigned(fmeVideo) And (fmeVideo.State = vsPlaying) Then
    fmeVideo.Pause;

  RefreshUI;
End;

Procedure TFrameVideoPlayer.actPlayClick(Sender: TObject);
Begin
  If EnsurePlaybackFrame Then
  Begin
    If fmeVideo.Rate <> 1 Then
      fmeVideo.Rate := 1;

    If fmeVideo.State = vsPaused Then
      fmeVideo.Resume
    Else
      fmeVideo.Play;
  End;

  RefreshUI;
End;

Procedure TFrameVideoPlayer.actPauseClick(Sender: TObject);
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

Procedure TFrameVideoPlayer.actPlayPauseExecute(Sender: TObject);
Begin
  If Not Assigned(fmeVideo) Then
    Exit;

  If fmeVideo.State = vsPlaying Then
    fmeVideo.Pause
  Else
    fmeVideo.Play;

  RefreshUI;
End;

Procedure TFrameVideoPlayer.actCopyToClipboardExecute(Sender: TObject);
Begin
  If Not Assigned(fmeVideo) Then
    Exit;

  fmeVideo.CopyFrameToClipboard;
End;

Procedure TFrameVideoPlayer.actResetRateExecute(Sender: TObject);
Begin
  If Assigned(fmeVideo) And fmeVideo.CanSetRate Then
    fmeVideo.Rate := 1.0;

  RefreshUI;
End;

Procedure TFrameVideoPlayer.actPlayFasterClick(Sender: TObject);
Begin
  If Assigned(fmeVideo) And fmeVideo.CanSetRate Then
    fmeVideo.Rate := Min(MAX_RATE, fmeVideo.Rate * RATE_STEP);

  RefreshUI;
  Application.CancelHint;
  Application.ActivateHint(Mouse.CursorPos);
End;

Procedure TFrameVideoPlayer.actPlaySlowerClick(Sender: TObject);
Begin
  If Assigned(fmeVideo) And fmeVideo.CanSetRate Then
    fmeVideo.Rate := Max(MIN_RATE, fmeVideo.Rate / RATE_STEP);

  RefreshUI;
  Application.CancelHint;
  Application.ActivateHint(Mouse.CursorPos);
End;

Function TFrameVideoPlayer.StepDelta: Integer;
Var
  ShiftState: TShiftState;
Begin
  ShiftState := GetKeyShiftState;

  If ssCtrl In ShiftState Then
    Result := 1000
  Else If ssShift In ShiftState Then
    Result := 6000
  Else
    Result := 3000;
End;

Procedure TFrameVideoPlayer.actStepBackClick(Sender: TObject);
Begin
  If Assigned(fmeVideo) And fmeVideo.CanSeek Then
  Begin
    If fmeVideo.State = vsPlaying Then
      fmeVideo.Pause;

    fmeVideo.Position := Max(0, fmeVideo.Position - StepDelta);
  End;
End;

Procedure TFrameVideoPlayer.pnlVideoMouseEnter(Sender: TObject);
Begin
  If pnlVideo.CanFocus Then
    pnlVideo.SetFocus;
End;

Procedure TFrameVideoPlayer.actStepForwardClick(Sender: TObject);
Begin
  If Assigned(fmeVideo) And fmeVideo.CanSeek Then
  Begin
    If fmeVideo.State = vsPlaying Then
      fmeVideo.Pause;

    fmeVideo.Position := Min(fmeVideo.Duration, fmeVideo.Position + StepDelta);
  End;
End;

Procedure TFrameVideoPlayer.pnlVideoMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; Var Handled: Boolean);
Begin
  If Assigned(fmeVideo) And fmeVideo.CanSeek Then
  Begin
    If fmeVideo.State = vsPlaying Then
      fmeVideo.Pause;

    If WheelDelta > 0 Then
      fmeVideo.Position := Min(fmeVideo.Duration, fmeVideo.Position + StepDelta)
    Else
      fmeVideo.Position := Max(0, fmeVideo.Position - StepDelta);
  End;
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

Procedure TFrameVideoPlayer.btnGrabClick(Sender: TObject);
Var
  sPath: String;
Begin
  If Not Assigned(fmeVideo) Then
    Exit;

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

  // By default, leave it to the player to determine filename
  If fmeVideo.SaveFrameToFile(sPath) Then
    lblStatus.Caption := 'Saved image to ' + sPath
  Else
    lblStatus.Caption := 'Failed to grab image';

  RefreshUI;
End;

Procedure TFrameVideoPlayer.btnOpenInExplorerClick(Sender: TObject);
Begin
  If fmeVideo.Filename <> '' Then
    LaunchFile('explorer.exe', Format('/e,/select,"%s"', [fmeVideo.Filename]));
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

Procedure TFrameVideoPlayer.SetAutoplay(AValue: Boolean);
Begin
  FAutoplay := AValue;

  If Assigned(fmeVideo) Then
    fmeVideo.Autoplay := FAutoplay;
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

  If fmeVideo.StartDateTime = 0 Then
    lblTime.Caption := ToTime(PositionMS) + LineEnding + ToTime(DurationMS)
  Else
    lblTime.Caption := TimeToStr(fmeVideo.PositionAsTime) + LineEnding +
      TimeToStr(fmeVideo.EndDateTime);
End;

Procedure TFrameVideoPlayer.VideoStateChanged(Sender: TObject; State: TVideoState);
Begin
  If State = vsEnded Then
    If Assigned(FOnStop) Then
      FOnStop(Self);

  RefreshUI;
End;

End.
