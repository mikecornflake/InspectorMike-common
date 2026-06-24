Unit FrameSyncedVideo;

{-------------------------------------------------------------------------------
  Package   : IM_forms.media
  Unit      : FrameSyncedVideo.pas
  Description
    Multi-channel video playback frame that synchronises multiple
    TFrameVideoBase descendants and presents them as a single logical
    video player.

    Supports master/slave synchronisation, time-based seeking,
    configurable grid layouts and mixed playback engine implementations.

  Source
    Copyright (c) 2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    2026-06-12: Initial implementation.
                Designed as a composite TFrameVideoBase descendant allowing
                multiple video engines to be synchronised and controlled
                through a single playback interface.
                Initial implementation generated with assistance from
                OpenAI ChatGPT GPT-5.5 and reviewed by Mike Thompson.
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
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, FGL,
  FrameVideoBase, ControlGridLayout;

Type
  { TFrameSyncedVideo }

  TFrameSyncedVideo = Class(TFrameVideoBase)
    Procedure FrameResize(Sender: TObject);
  Private
    FVideoEngineClass: TFrameVideoBaseClass;
    FVideos: TFrameVideoBaseList;
    FMaster: TFrameVideoBase;
    FState: TVideoState;
    FSyncTimer: TTimer;
    FSyncSeekThresholdMS: TVideoTime;
    FStartTime: TDateTime;
    FReadyToPlay: Boolean;
    FLayout: TControlGridLayout;

    Procedure SetState(AValue: TVideoState);
    Procedure SetMaster(AValue: TFrameVideoBase);

    Function GetCurrentTime: TDateTime;
    Procedure SetCurrentTime(AValue: TDateTime);
    Function GetEndTime: TDateTime;

    Procedure MasterPosition(Sender: TObject; PositionMS, DurationMS: TVideoTime);
    Procedure VideoStateChanged(Sender: TObject; AState: TVideoState);

    Function LoadedVideoCount: Integer;
    Function AllVideosLoaded: Boolean;
    Procedure CheckAllVideosLoaded;
    Procedure UpdateStateFromChildren;

    Procedure SyncTimerTimer(Sender: TObject);
    Procedure SyncVideos;

  Protected
    Function GetPosition: TVideoTime; Override;
    Procedure SetPosition(AValue: TVideoTime); Override;
    Function GetDuration: TVideoTime; Override;
    Function GetRate: Double; Override;
    Procedure SetRate(AValue: Double); Override;
    Function GetState: TVideoState; Override;

    Procedure SetAutoplay(AValue: Boolean); Override;
  Public
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure Layout(ARows, ACols: Integer; ASequence: TControlLayoutSequence);

    Procedure ClearUnloadedVideoFrames;
    Procedure ClearVideoCount;

    Procedure BeginLoadVideos;
    Procedure EndLoadVideos;

    Function Load(Const AFilename: String; AChannel: String = '';
      AStartDateTime: TDateTime = 0): Boolean; Override;

    Function Play: Boolean; Override;
    Function Pause: Boolean; Override;
    Function Resume: Boolean; Override;
    Function Stop: Boolean; Override;

    Function CanSeek: Boolean; Override;
    Function CanSetRate: Boolean; Override;

    Function CanGrabFrame: Boolean; Override;
    Function SaveFrameToFile(Const AFilename: String): Boolean; Override;
    Function CopyFrameToClipboard: Boolean; Override;

    Property Videos: TFrameVideoBaseList Read FVideos;
    Property Master: TFrameVideoBase Read FMaster Write SetMaster;

    Property StartTime: TDateTime Read FStartTime Write FStartTime;
    Property CurrentTime: TDateTime Read GetCurrentTime Write SetCurrentTime;
    Property EndTime: TDateTime Read GetEndTime;

    Property SyncSeekThresholdMS: TVideoTime Read FSyncSeekThresholdMS Write FSyncSeekThresholdMS;

    Property VideoEngineClass: TFrameVideoBaseClass Read FVideoEngineClass Write FVideoEngineClass;
  End;

Implementation

Uses
  DateUtils;

  {$R *.lfm}

  { TFrameSyncedVideo }

Constructor TFrameSyncedVideo.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  FVideos := TFrameVideoBaseList.Create(True); // Does own videos.
  FMaster := nil;
  FState := vsEmpty;

  // End user will have to provide an active class
  FVideoEngineClass := nil;

  FStartTime := 0;
  FSyncSeekThresholdMS := 1000;
  FReadyToPlay := False;
  FAutoPlay := False;

  FSyncTimer := TTimer.Create(Self);
  FSyncTimer.Enabled := False;
  FSyncTimer.Interval := 500;
  FSyncTimer.OnTimer := @SyncTimerTimer;

  FLayout := TControlGridLayout.Create(Self);
  FLayout.RowCount := 2;
  FLayout.ColCount := 1;
  FLayout.Sequence := clsLeftToRightThenDown;
  FLayout.PanelMargin := 0;
  FLayout.CellSpacing := 0;

  FVideoFileCount := 0;

  // Use the generic placeholder as this is collection of channels
  FChannel := CHANNEL_PARAM;
End;

Destructor TFrameSyncedVideo.Destroy;
Begin
  FSyncTimer.Enabled := False;
  FreeAndNil(FVideos);
  FreeAndNil(FLayout);
  FreeAndNil(FSyncTimer);

  Inherited Destroy;
End;

Procedure TFrameSyncedVideo.FrameResize(Sender: TObject);
Begin
  If Assigned(FLayout) Then
    FLayout.LayoutControls(FVideos);
End;

Procedure TFrameSyncedVideo.SetState(AValue: TVideoState);
Begin
  If FState = AValue Then
    Exit;

  FState := AValue;
  DoStateChanged;
End;

Procedure TFrameSyncedVideo.SetMaster(AValue: TFrameVideoBase);
Var
  i: Integer;
Begin
  If FMaster = AValue Then
    Exit;

  If Assigned(FMaster) Then
    FMaster.OnPosition := nil;

  FMaster := AValue;

  If Assigned(FMaster) Then
  Begin
    FMaster.OnPosition := @MasterPosition;

    For i := 0 To FVideoFileCount - 1 Do
      FVideos[i].Muted := FVideos[i] <> FMaster;

    SetState(FMaster.State);
  End
  Else
    SetState(vsEmpty);

  DoPosition;
End;

Procedure TFrameSyncedVideo.Layout(ARows, ACols: Integer; ASequence: TControlLayoutSequence);
Begin
  FLayout.RowCount := ARows;
  FLayout.ColCount := ACols;
  FLayout.Sequence := ASequence;
  FLayout.LayoutControls(FVideos, FVideoFileCount);
End;

Procedure TFrameSyncedVideo.ClearUnloadedVideoFrames;
Var
  fmeVideo: TFrameVideoBase;
  i: Integer;
Begin
  For i := FVideoFileCount To FVideos.Count - 1 Do
  Begin
    fmeVideo := FVideos[i];

    // Unload Video
    fmeVideo.Clear;

    // Hide video frame
    fmeVideo.Visible := False;
  End;

  If (FVideoFileCount = 0) Then
    ClearVideoCount;
End;

Procedure TFrameSyncedVideo.ClearVideoCount;
Begin
  Master := nil;
  FFilename := '';
  FVideoFileCount := 0;
  FReadyToPlay := False;
  SetState(vsEmpty);
End;

Function TFrameSyncedVideo.GetPosition: TVideoTime;
Begin
  If Assigned(FMaster) Then
    Result := FMaster.Position
  Else
    Result := 0;
End;

Procedure TFrameSyncedVideo.SetPosition(AValue: TVideoTime);
Var
  i: Integer;
Begin
  For i := 0 To FVideoFileCount - 1 Do
    If FVideos[i].CanSeek Then
      FVideos[i].Position := AValue;

  DoPosition;
End;

Function TFrameSyncedVideo.GetDuration: TVideoTime;
Begin
  If Assigned(FMaster) Then
    Result := FMaster.Duration
  Else
    Result := -1;
End;

Function TFrameSyncedVideo.GetRate: Double;
Begin
  If Assigned(FMaster) Then
    Result := FMaster.Rate
  Else
    Result := 1.0;
End;

Procedure TFrameSyncedVideo.SetRate(AValue: Double);
Var
  i: Integer;
Begin
  For i := 0 To FVideoFileCount - 1 Do
    If FVideos[i].CanSetRate Then
      FVideos[i].Rate := AValue;
End;

Function TFrameSyncedVideo.GetState: TVideoState;
Begin
  Result := FState;
End;

Procedure TFrameSyncedVideo.SetAutoplay(AValue: Boolean);
Var
  fmeVideo: TFrameVideoBase;
  i: Integer;
Begin
  Inherited SetAutoplay(AValue);

  For i := 0 To FVideos.Count - 1 Do
  Begin
    fmeVideo := FVideos[i];
    fmeVideo.Autoplay := AValue;
  End;
End;

Procedure TFrameSyncedVideo.UpdateStateFromChildren;
Var
  i: Integer;
  bAnyPlaying: Boolean;
  bAnyPaused: Boolean;
  bAnyLoading: Boolean;
  bAnyError: Boolean;
  fmeVideo: TFrameVideoBase;
Begin
  bAnyPlaying := False;
  bAnyPaused := False;
  bAnyLoading := False;
  bAnyError := False;

  For i := 0 To FVideoFileCount - 1 Do
  Begin
    fmeVideo := FVideos[i];

    Case fmeVideo.State Of
      vsPlaying: bAnyPlaying := True;
      vsPaused: bAnyPaused := True;
      vsLoading: bAnyLoading := True;
      vsError: bAnyError := True;
    End;
  End;

  If bAnyError Then
    SetState(vsError)
  Else If bAnyPlaying Then
    SetState(vsPlaying)
  Else If bAnyLoading Then
    SetState(vsLoading)
  Else If bAnyPaused Then
    SetState(vsPaused)
  Else If FVideoFileCount > 0 Then
    SetState(vsStopped)
  Else
    SetState(vsEmpty);
End;

Function TFrameSyncedVideo.GetCurrentTime: TDateTime;
Begin
  If FStartTime = 0 Then
    Result := 0
  Else
    Result := FStartTime + (Position / MSecsPerDay);
End;

Procedure TFrameSyncedVideo.SetCurrentTime(AValue: TDateTime);
Var
  OffsetMS: TVideoTime;
Begin
  If FStartTime = 0 Then
    Exit;

  OffsetMS := MilliSecondsBetween(AValue, FStartTime);

  If AValue < FStartTime Then
    OffsetMS := -OffsetMS;

  Position := OffsetMS;
End;

Function TFrameSyncedVideo.GetEndTime: TDateTime;
Begin
  If (FStartTime = 0) Or (Duration < 0) Then
    Result := 0
  Else
    Result := FStartTime + (Duration / MSecsPerDay);
End;

Function TFrameSyncedVideo.Load(Const AFilename: String; AChannel: String;
  AStartDateTime: TDateTime): Boolean;
Var
  fmeVideo: TFrameVideoBase;
Begin
  Result := False;

  If Not Assigned(FVideoEngineClass) Then
    Exit;

  FVideoFileCount += 1;

  If FVideoFileCount <= FVideos.Count Then
  Begin
    fmeVideo := FVideos[FVideoFileCount - 1];
    fmeVideo.Visible := True;
    fmeVideo.Autoplay := FAutoplay;
  End
  Else
  Begin
    fmeVideo := FVideoEngineClass.Create(nil);
    fmeVideo.Parent := Self;
    fmeVideo.OnStateChanged := @VideoStateChanged;
    fmeVideo.Autoplay := FAutoplay;

    FVideos.Add(fmeVideo);
  End;

  If FVideoFileCount > (FLayout.RowCount * FLayout.ColCount) Then
    FLayout.ColCount := FLayout.ColCount + 1;

  If Not Assigned(FMaster) Then
  Begin
    // Set the file details for TFrameSyncedVideo
    Inherited Load(AFilename, CHANNEL_PARAM, AStartDateTime);
    Master := fmeVideo;
  End;

  fmeVideo.Muted := fmeVideo <> FMaster;

  // Now set the file details for the specific channel
  Result := fmeVideo.Load(AFilename, AChannel, AStartDateTime);

  //FLayout.LayoutControls(FVideos, FVideoFileCount);
End;

Function TFrameSyncedVideo.Play: Boolean;
Var
  i: Integer;
Begin
  Result := FVideoFileCount > 0;

  FReadyToPlay := False;

  For i := 0 To FVideoFileCount - 1 Do
  Begin
    If FVideos[i].State = vsEmpty Then
      Continue;

    If FVideos[i].State = vsPaused Then
      Result := FVideos[i].Resume And Result
    Else
      Result := FVideos[i].Play And Result;
  End;

  If Result Then
  Begin
    FReadyToPlay := True;
    SetState(vsPlaying);
    FSyncTimer.Enabled := True;
  End;
End;

Function TFrameSyncedVideo.Pause: Boolean;
Var
  i: Integer;
Begin
  Result := FVideoFileCount > 0;

  FSyncTimer.Enabled := False;

  For i := 0 To FVideoFileCount - 1 Do
    Result := FVideos[i].Pause And Result;

  If Result Then
    SetState(vsPaused);
End;

Function TFrameSyncedVideo.Resume: Boolean;
Var
  i: Integer;
Begin
  Result := FVideoFileCount > 0;

  For i := 0 To FVideoFileCount - 1 Do
    Result := FVideos[i].Resume And Result;

  If Result Then
  Begin
    SetState(vsPlaying);
    FSyncTimer.Enabled := True;
  End;
End;

Function TFrameSyncedVideo.Stop: Boolean;
Var
  i: Integer;
Begin
  Result := FVideoFileCount > 0;

  FSyncTimer.Enabled := False;

  For i := 0 To FVideoFileCount - 1 Do
    Result := FVideos[i].Stop And Result;

  If Result Then
    SetState(vsStopped);
End;

Function TFrameSyncedVideo.CanSeek: Boolean;
Begin
  Result := Assigned(FMaster) And FMaster.CanSeek;
End;

Function TFrameSyncedVideo.CanSetRate: Boolean;
Begin
  Result := Assigned(FMaster) And FMaster.CanSetRate;
End;

Function TFrameSyncedVideo.CanGrabFrame: Boolean;
Begin
  Result := Assigned(FMaster) And FMaster.CanGrabFrame;
End;

Function TFrameSyncedVideo.SaveFrameToFile(Const AFilename: String): Boolean;
Var
  sFile, sExt: String;
  fmeVideo: TFrameVideoBase;
  i: Integer;
Begin
  Result := FVideoFileCount > 0;

  For i := 0 To FVideoFileCount - 1 Do
  Begin
    fmeVideo := TFrameVideobase(FVideos[i]);

    sExt := ExtractFileExt(AFilename);

    If sExt = '' Then
      sFile :=  fmeVideo.DefaultFrameFilename(IncludeTrailingBackslash(AFilename), '.png')
    Else
      sFile := AFilename.Replace(CHANNEL_PARAM, fmeVideo.Channel);

    Result := Result And FVideos[i].SaveFrameToFile(sFile);
  End;
End;

Function TFrameSyncedVideo.CopyFrameToClipboard: Boolean;
Begin
  // TODO how do we know which frame to grab?
  Result := Assigned(FMaster) And FMaster.CanGrabFrame;
  If Result Then
    Result := FMaster.CopyFrameToClipboard;
End;

Procedure TFrameSyncedVideo.MasterPosition(Sender: TObject; PositionMS, DurationMS: TVideoTime);
Begin
  DoPosition;
End;

Function TFrameSyncedVideo.LoadedVideoCount: Integer;
Var
  i: Integer;
Begin
  Result := 0;

  For i := 0 To FVideoFileCount - 1 Do
    If FVideos[i].HasVideo Then
      Inc(Result);
End;

Function TFrameSyncedVideo.AllVideosLoaded: Boolean;
Begin
  Result :=
    (FVideoFileCount > 0) And (LoadedVideoCount = FVideoFileCount);
End;

Procedure TFrameSyncedVideo.CheckAllVideosLoaded;
Begin
  If AllVideosLoaded Then
  Begin
    FReadyToPlay := True;
    FLayout.LayoutControls(FVideos);
    DoPosition;

    If FAutoPlay Then
    Begin
      FSyncTimer.Enabled := True;
      SetState(vsPlaying);
    End
    Else
    Begin
      FSyncTimer.Enabled := False;
      SetState(vsPaused);
    End;
  End
  Else
    SetState(vsLoading);
End;

Procedure TFrameSyncedVideo.VideoStateChanged(Sender: TObject; AState: TVideoState);
Begin
  Case AState Of
    vsLoading:
    Begin
      If Not FReadyToPlay Then
        SetState(vsLoading);
    End;

    vsError:
    Begin
      FSyncTimer.Enabled := False;
      SetState(vsError);
    End;

    vsPlaying:
    Begin
      If Not FReadyToPlay Then
        CheckAllVideosLoaded
      Else
      Begin
        SetState(vsPlaying);
        FSyncTimer.Enabled := True;
      End;
    End;

    vsPaused:
    Begin
      If Not FReadyToPlay Then
        CheckAllVideosLoaded
      Else
      Begin
        FSyncTimer.Enabled := False;
        UpdateStateFromChildren;
      End;
    End;

    vsStopped:
    Begin
      FSyncTimer.Enabled := False;
      SetState(vsStopped);
    End;

    vsEnded:
    Begin
      FSyncTimer.Enabled := False;
      SetState(vsEnded);
    End;

    vsEmpty:
    Begin
      FSyncTimer.Enabled := False;
      SetState(vsEmpty);
    End;
  End;
End;

Procedure TFrameSyncedVideo.SyncTimerTimer(Sender: TObject);
Begin
  SyncVideos;
End;

Procedure TFrameSyncedVideo.SyncVideos;
Var
  i: Integer;
  MasterPos: TVideoTime;
  SlavePos: TVideoTime;
  DriftMS: TVideoTime;
  Slave: TFrameVideoBase;
Begin
  If Not Assigned(FMaster) Then
    Exit;

  If FMaster.State <> vsPlaying Then
    Exit;

  MasterPos := FMaster.Position;

  For i := 0 To FVideoFileCount - 1 Do
  Begin
    Slave := FVideos[i];

    If Slave = FMaster Then
      Continue;

    If Not Slave.CanSeek Then
      Continue;

    If Not (Slave.State In [vsPlaying, vsPaused]) Then
      Continue;

    SlavePos := Slave.Position;
    DriftMS := Abs(MasterPos - SlavePos);

    If DriftMS > FSyncSeekThresholdMS Then
      Slave.Position := MasterPos;
  End;
End;

Procedure TFrameSyncedVideo.BeginLoadVideos;
Begin
  FSyncTimer.Enabled := False;
  FReadyToPlay := False;
  ClearVideoCount;
  SetState(vsLoading);
End;

Procedure TFrameSyncedVideo.EndLoadVideos;
Begin
  ClearUnloadedVideoFrames;
  FLayout.LayoutControls(FVideos, FVideoFileCount);
  CheckAllVideosLoaded;
End;

End.
