Unit FrameVideoLibmpv;

{-------------------------------------------------------------------------------
  Package   : IM_forms.media.mpv
  Unit      : FrameVideoLibmpv.pas
  Description
    LibMPV-based implementation of TFrameVideoBase.

    Provides video playback, seeking, rate control, frame capture and
    playback state monitoring using the MPV media engine.

  Source
    Copyright (c) 2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    2026-06-01: Initial MPV playback prototype.
    2026-06-12: Refactored as a TFrameVideoBase descendant and integrated
                into the common video playback framework.
                Added state management, loading watchdog, playback
                notifications and synchronisation support.
                Initial framework implementation generated with
                assistance from OpenAI ChatGPT GPT-5.5 and reviewed
                by Mike Thompson.
    2026-06-19: Refactored into split InspectorMike package structure

  License
    This file is part of IM_forms.media.mpv.lpk.

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

{$mode ObjFPC}{$H+}
{$WARN 5024 off : Parameter "$1" not used}
Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics,
  FrameVideoBase, MPVPlayer, ExtCtrls, StdCtrls;

Type

  { TFrameVideoLibmpv }

  TFrameVideoLibmpv = Class(TFrameVideoBase)
    lblMsg: TLabel;
  Private
    FmpvPlayer: TMPVPlayer;
    FState: TVideoState;
    FMuted: Boolean;

    FLoadWatchdog: TTimer;
    FLoadMediaFinalised: Boolean;

    Procedure LoadWatchdogTimer(Sender: TObject);
    Procedure FinaliseLoadedState;

    Procedure SetState(AValue: TVideoState);

    Procedure mpvStartFile(Sender: TObject);
    Procedure mpvFileLoaded(Sender: TObject);
    Procedure mpvPlay(Sender: TObject);
    Procedure mpvPause(Sender: TObject);
    Procedure mpvStop(Sender: TObject);
    Procedure mpvTimeChanged(ASender: TObject; AParam: Integer);

  Protected
    Procedure SetAutoplay(AValue: Boolean); Override;
    Function GetPosition: TVideoTime; Override;
    Procedure SetPosition(AValue: TVideoTime); Override;
    Function GetDuration: TVideoTime; Override;
    Function GetRate: Double; Override;
    Procedure SetRate(AValue: Double); Override;
    Function GetState: TVideoState; Override;
    Function GetMuted: Boolean; Override;
    Procedure SetMuted(AValue: Boolean); Override;

    Procedure SetVisible(Value: Boolean); Override;
  Public
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;

    Function Load(Const AFilename: String; AChannel: String = '';
      AStartDateTime: TDateTime = 0): Boolean; Override;
    Function Play: Boolean; Override;
    Function Pause: Boolean; Override;
    Function Resume: Boolean; Override;
    Function Stop: Boolean; Override;
    Function Clear: Boolean; Override;

    Function CanSeek: Boolean; Override;
    Function CanSetRate: Boolean; Override;

    Function CanGrabFrame: Boolean; Override;
    Function SaveFrameToFile(Const AFilename: String): Boolean; Override;
  End;

Implementation

{$R *.lfm}

Uses
  LibmpvSupport, libMPV.Client, VideoEngineFactory, FileSupport;

  { TFrameVideoLibmpv }

Constructor TFrameVideoLibmpv.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  If Not FindLibmpvDLL Then
  Begin
    lblMsg.Visible := True;
    lblMsg.Caption := 'libmpv-2.dll not found';

    FmpvPlayer := nil;

    FState := vsError;
  End
  Else
  Begin
    If Not IsLibMPV_Loaded Then
      Load_libMPV(LibmpvSupport.LibmpvDLL);

    lblMsg.Visible := False;

    FmpvPlayer := TMPVPlayer.Create(Self);
    FmpvPlayer.Parent := Self;
    FmpvPlayer.AutoSize := False;
    FmpvPlayer.Align := alClient;
    FmpvPlayer.Color := clGray;

    FmpvPlayer.AutoStartPlayback := False;
    FmpvPlayer.AutoLoadSubtitle := False;
    FmpvPlayer.KeepAspect := True;
    FmpvPlayer.NoAudioDisplay := False;
    FmpvPlayer.RendererMode := rmEmbedding;
    FmpvPlayer.RenderFailAction := rfNone;
    FmpvPlayer.LogLevel := llStatus;

    FmpvPlayer.OnStartFile := @mpvStartFile;
    FmpvPlayer.OnFileLoaded := @mpvFileLoaded;
    FmpvPlayer.OnPlay := @mpvPlay;
    FmpvPlayer.OnPause := @mpvPause;
    FmpvPlayer.OnStop := @mpvStop;
    FmpvPlayer.OnTimeChanged := @mpvTimeChanged;

    FState := vsEmpty;
  End;

  FLoadMediaFinalised := False;

  FLoadWatchdog := TTimer.Create(Self);
  FLoadWatchdog.Enabled := False;
  FLoadWatchdog.Interval := 100;
  FLoadWatchdog.OnTimer := @LoadWatchdogTimer;
End;

Destructor TFrameVideoLibmpv.Destroy;
Begin
  If Assigned(FmpvPlayer) Then
  Begin
    If FmpvPlayer.IsMediaLoaded Then
      FmpvPlayer.Close(True);
  End;

  FreeAndNil(FmpvPlayer);
  FreeAndNil(FLoadWatchdog);

  Inherited Destroy;
End;

Procedure TFrameVideoLibmpv.FinaliseLoadedState;
Begin
  If Not Assigned(FmpvPlayer) Then
    Exit;

  If FLoadMediaFinalised Then
    Exit;

  If Not FmpvPlayer.IsMediaLoaded Then
    Exit;

  FLoadMediaFinalised := True;
  FLoadWatchdog.Enabled := False;

  FmpvPlayer.SetAudioMute(FMuted);
  FmpvPlayer.Color := clBlack;
  FVideoFileCount := 1;

  DoPosition;

  If Autoplay Then
    SetState(vsPlaying)
  Else
    SetState(vsPaused);
End;

Procedure TFrameVideoLibmpv.LoadWatchdogTimer(Sender: TObject);
Begin
  FinaliseLoadedState;
End;

Procedure TFrameVideoLibmpv.SetState(AValue: TVideoState);
Begin
  If FState = AValue Then
    Exit;

  FState := AValue;
  DoStateChanged;
End;

Function TFrameVideoLibmpv.GetPosition: TVideoTime;
Begin
  Result := 0;

  If Not Assigned(FmpvPlayer) Then
    Exit;

  If Assigned(FmpvPlayer) And FmpvPlayer.IsMediaLoaded Then
    Result := FmpvPlayer.GetMediaPosInMs;
End;

Procedure TFrameVideoLibmpv.SetPosition(AValue: TVideoTime);
Begin
  If CanSeek And Assigned(FmpvPlayer) Then
    FmpvPlayer.SeekInMs(AValue);
End;

Function TFrameVideoLibmpv.GetDuration: TVideoTime;
Begin
  If Assigned(FmpvPlayer) And FmpvPlayer.IsMediaLoaded Then
    Result := FmpvPlayer.GetMediaLenInMs
  Else
    Result := -1;
End;

Function TFrameVideoLibmpv.GetRate: Double;
Begin
  If Not Assigned(FmpvPlayer) Then
    Exit(1);

  Result := FmpvPlayer.mpv_get_property_double('speed');
End;

Procedure TFrameVideoLibmpv.SetRate(AValue: Double);
Begin
  If Not Assigned(FmpvPlayer) Then
    Exit;

  FmpvPlayer.mpv_set_property_double('speed', AValue);
End;

Function TFrameVideoLibmpv.GetState: TVideoState;
Begin
  Result := FState;
End;

Function TFrameVideoLibmpv.GetMuted: Boolean;
Begin
  Result := FMuted;
End;

Procedure TFrameVideoLibmpv.SetMuted(AValue: Boolean);
Begin
  FMuted := AValue;

  If Assigned(FmpvPlayer) And FmpvPlayer.IsMediaLoaded Then
    FmpvPlayer.SetAudioMute(FMuted);
End;

Procedure TFrameVideoLibmpv.SetVisible(Value: Boolean);
Begin
  Inherited SetVisible(Value);

  If Assigned(FmpvPlayer) Then
    FmpvPlayer.Visible := Value;
End;

Function TFrameVideoLibmpv.Load(Const AFilename: String; AChannel: String;
  AStartDateTime: TDateTime): Boolean;
Begin
  Result := False;

  FLoadMediaFinalised := False;

  If Assigned(FmpvPlayer) Then
    FmpvPlayer.AutoStartPlayback := Autoplay;

  Result := Inherited Load(AFilename, AChannel, AStartDateTime);

  If Not Result Then
    Exit;

  SetState(vsLoading);
  FmpvPlayer.Play(AFilename);
  FLoadWatchdog.Enabled := True;
End;

Function TFrameVideoLibmpv.Clear: Boolean;
Begin
  Result := Inherited Clear;

  Result := Result And Assigned(FMPVPlayer);

  If Not Result Then
    Exit;

  FMPVPlayer.mpv_command_(['stop']);

  FFilename := '';
  FVideoFileCount := 0;
  SetState(vsEmpty);
End;

Function TFrameVideoLibmpv.Play: Boolean;
Begin
  Result := False;

  If Not Assigned(FmpvPlayer) Or (FFilename = '') Or (Not FileExists(FFilename)) Then
  Begin
    SetState(vsError);
    Exit;
  End;

  FmpvPlayer.AutoStartPlayback := True;

  FLoadMediaFinalised := False;
  SetState(vsLoading);

  FmpvPlayer.Play(FFilename);

  FLoadWatchdog.Enabled := True;

  Result := True;
End;

Function TFrameVideoLibmpv.Pause: Boolean;
Begin
  Result := False;

  If Not Assigned(FmpvPlayer) Then
    Exit;

  If FmpvPlayer.IsMediaLoaded And FmpvPlayer.IsPlaying Then
  Begin
    FmpvPlayer.Pause;
    Result := True;
  End;
End;

Function TFrameVideoLibmpv.Resume: Boolean;
Begin
  Result := False;

  If Not Assigned(FmpvPlayer) Then
    Exit;

  If FmpvPlayer.IsMediaLoaded And FmpvPlayer.IsPaused Then
  Begin
    FmpvPlayer.Resume(True);
    Result := True;
  End;
End;

Function TFrameVideoLibmpv.Stop: Boolean;
Begin
  Result := False;

  If Not Assigned(FmpvPlayer) Then
    Exit;

  FLoadWatchdog.Enabled := False;
  FLoadMediaFinalised := False;

  If FmpvPlayer.IsMediaLoaded Then
  Begin
    FmpvPlayer.Stop;
    Result := True;
  End;
End;

Function TFrameVideoLibmpv.CanSeek: Boolean;
Begin
  Result := Assigned(FmpvPlayer);
End;

Function TFrameVideoLibmpv.CanSetRate: Boolean;
Begin
  Result := True;
End;

Function TFrameVideoLibmpv.CanGrabFrame: Boolean;
Begin
  Result := True;
End;

Function TFrameVideoLibmpv.SaveFrameToFile(Const AFilename: String): Boolean;
Var
  sFile, sExt: String;
Begin
  Result := False;

  If Not Assigned(FmpvPlayer) Then
    Exit;

  sExt := ExtractFileExt(AFilename);

  If sExt = '' Then
    sFile := DefaultFrameFilename(IncludeTrailingBackslash(AFilename), '.png')
  Else
    sFile := AFilename.Replace(CHANNEL_PARAM, FChannel);

  FmpvPlayer.ScreenshotToFile(sFile, smVideo);

  Result := FileExists(AFilename);
End;

Procedure TFrameVideoLibmpv.mpvStartFile(Sender: TObject);
Begin
  SetState(vsLoading);
End;

Procedure TFrameVideoLibmpv.mpvFileLoaded(Sender: TObject);
Begin
  FinaliseLoadedState;
End;

Procedure TFrameVideoLibmpv.mpvPlay(Sender: TObject);
Begin
  If FLoadMediaFinalised Then
    SetState(vsPlaying);
End;

Procedure TFrameVideoLibmpv.mpvPause(Sender: TObject);
Begin
  SetState(vsPaused);
End;

Procedure TFrameVideoLibmpv.mpvStop(Sender: TObject);
Begin
  SetState(vsStopped);
End;

Procedure TFrameVideoLibmpv.mpvTimeChanged(ASender: TObject; AParam: Integer);
Begin
  DoPosition;
End;

Procedure TFrameVideoLibmpv.SetAutoplay(AValue: Boolean);
Begin
  Inherited SetAutoplay(AValue);

  If Assigned(FmpvPlayer) Then
    FmpvPlayer.AutoStartPlayback := AValue;
End;

Initialization
  TVideoEngineFactory.RegisterEngine('libmpv', TFrameVideoLibmpv);

End.
