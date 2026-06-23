Unit FrameVideoBase;

{-------------------------------------------------------------------------------
  Package   : IM_forms.media
  Unit      : FrameVideoBase.pas
  Description
    Abstract base frame for video playback implementations.

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  FrameBase, ControlsSupport;

Type
  TVideoTime = Int64; // milliseconds

  TVideoState = (
    vsEmpty,
    vsLoading,
    vsStopped,
    vsPlaying,
    vsPaused,
    vsEnded,
    vsError
    );

  TPositionEvent = Procedure(Sender: TObject; PositionMS, DurationMS: TVideoTime) Of Object;
  TStateEvent = Procedure(Sender: TObject; State: TVideoState) Of Object;

  TFrameVideoBase = Class;

  TFrameVideoBaseClass = Class Of TFrameVideoBase;

  { TFrameVideoBaseList }

  TFrameVideoBaseList = Class(TControlList)
  Public
    Function GetVideo(Index: Integer): TFrameVideoBase;
    Procedure AddVideo(AVideo: TFrameVideoBase);

    Property Videos[Index: Integer]: TFrameVideoBase Read GetVideo; Default;
  End;

  { TFrameVideoBase }

  TFrameVideoBase = Class(TFrameBase)
  Private
    Function GetPositionAsTime: TDateTime;
    Procedure SetPositionAsTime(AValue: TDateTime);
  Protected
    FFilename: String;
    FOnPosition: TPositionEvent;
    FOnStateChanged: TStateEvent;
    FVideoFileCount: Integer;
    FAutoplay: Boolean;
    FChannel: String;
    FStartDateTime: TDateTime;

    Procedure SetAutoplay(AValue: Boolean); Virtual;

    Function GetPosition: TVideoTime; Virtual;
    Procedure SetPosition(AValue: TVideoTime); Virtual;
    Function GetDuration: TVideoTime; Virtual;
    Function GetRate: Double; Virtual;
    Procedure SetRate(AValue: Double); Virtual;
    Function GetState: TVideoState; Virtual;
    Function GetMuted: Boolean; Virtual;
    Procedure SetMuted(AValue: Boolean); Virtual;

    Procedure DoPosition; Virtual;
    Procedure DoStateChanged; Virtual;

  Public
    Constructor Create(TheOwner: TComponent); Override;

    Function Load(Const AFilename: String; AChannel: String = '';
      AStartDateTime: TDateTime = 0): Boolean; Virtual;
    Function Play: Boolean; Virtual;
    Function Pause: Boolean; Virtual;
    Function Resume: Boolean; Virtual;
    Function Stop: Boolean; Virtual;
    Function Clear: Boolean; Virtual;
    Property Autoplay: Boolean Read FAutoplay Write SetAutoplay;

    Function HasVideo: Boolean; Virtual;
    Function EndTime: TDateTime;

    Function CanSeek: Boolean; Virtual;
    Function CanSetRate: Boolean; Virtual;

    Function CanGrabFrame: Boolean; Virtual;
    Function SaveFrameToFile(Const AFilename: String): Boolean; Virtual;
    Function CopyFrameToClipboard: Boolean; Virtual;
    Function DefaultFrameFilename(AFolder: String; AExt: String): String;

    Property Filename: String Read FFilename;
    Property Muted: Boolean Read GetMuted Write SetMuted;

    Property Rate: Double Read GetRate Write SetRate;
    Property State: TVideoState Read GetState;

    //Optional: The name of the camera used to record video.
    //  i.e. "Front Door", "Centre", "Port" etc
    Property Channel: String Read FChannel Write FChannel;

    // These two are in TVideoTime i.e. milliseconds
    Property Position: TVideoTime Read GetPosition Write SetPosition;
    Property Duration: TVideoTime Read GetDuration;

    Property StartDateTime: TDateTime Read FStartDateTime Write FStartDateTime;
    Property PositionAsTime: TDateTime Read GetPositionAsTime Write SetPositionAsTime;

    Property OnPosition: TPositionEvent Read FOnPosition Write FOnPosition;
    Property OnStateChanged: TStateEvent Read FOnStateChanged Write FOnStateChanged;

    Property VideoFileCount: Integer Read FVideoFileCount;
  End;

Const
  CHANNEL_PARAM: String = '<channel>';

Implementation

{$R *.lfm}

Uses
  DateUtils, Math, FileSupport;

  { TFrameVideoBase }

Constructor TFrameVideoBase.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  FAutoplay := False;
  FFilename := '';
  FOnPosition := nil;
  FOnStateChanged := nil;
  FChannel := '';
  FStartDateTime := 0;

  // Each descendent should set this when files are correctly loaded.
  FVideoFileCount := 0;
End;

Function TFrameVideoBase.GetPositionAsTime: TDateTime;
Begin
  Result := FStartDateTime;

  If VideoFileCount > 0 Then
    Result := FStartDateTime + (Position / MSecsPerDay);
End;

Procedure TFrameVideoBase.SetPositionAsTime(AValue: TDateTime);
Var
  iPosition: TVideoTime;
Begin
  If VideoFileCount = 0 Then
    Exit;

  iPosition := Round((AValue - FStartDateTime) * MSecsPerDay);

  Position := EnsureRange(iPosition, 0, Duration);
End;

Function TFrameVideoBase.EndTime: TDateTime;
Begin
  Result := FStartDateTime + (Duration / MSecsPerDay);
End;

Procedure TFrameVideoBase.SetAutoplay(AValue: Boolean);
Begin
  FAutoplay := AValue;
End;

Function TFrameVideoBase.GetPosition: TVideoTime;
Begin
  Result := 0;
End;

Procedure TFrameVideoBase.SetPosition(AValue: TVideoTime);
Begin
  // Abstract base: descendant handles seeking.
End;

Function TFrameVideoBase.GetDuration: TVideoTime;
Begin
  Result := -1;
End;

Function TFrameVideoBase.GetRate: Double;
Begin
  Result := 1.0;
End;

Procedure TFrameVideoBase.SetRate(AValue: Double);
Begin
  // Abstract base: descendant handles playback rate.
End;

Function TFrameVideoBase.GetState: TVideoState;
Begin
  Result := vsEmpty;
End;

Function TFrameVideoBase.GetMuted: Boolean;
Begin
  Result := False;
End;

Procedure TFrameVideoBase.SetMuted(AValue: Boolean);
Begin
  //Descendant handles muting
End;

Procedure TFrameVideoBase.DoPosition;
Begin
  If Assigned(FOnPosition) Then
    FOnPosition(Self, Position, Duration);
End;

Procedure TFrameVideoBase.DoStateChanged;
Begin
  If Assigned(FOnStateChanged) Then
    FOnStateChanged(Self, State);
End;

Function TFrameVideoBase.Load(Const AFilename: String; AChannel: String;
  AStartDateTime: TDateTime): Boolean;
Begin
  FFilename := AFilename;
  FStartDateTime := AStartDateTime;
  FChannel := AChannel;

  Result := FileExists(AFilename);

  ShowHint := Result;
  If Result Then
    Hint := ExtractFilename(AFilename)
  Else
    Hint := '';
End;

Function TFrameVideoBase.Play: Boolean;
Begin
  Result := False;
End;

Function TFrameVideoBase.Pause: Boolean;
Begin
  Result := False;
End;

Function TFrameVideoBase.Resume: Boolean;
Begin
  Result := False;
End;

Function TFrameVideoBase.Stop: Boolean;
Begin
  Result := False;
End;

Function TFrameVideoBase.Clear: Boolean;
Begin
  FFilename := '';
  Result := True;
End;

Function TFrameVideoBase.HasVideo: Boolean;
Begin
  Result := FVideoFileCount > 0;
End;

Function TFrameVideoBase.CanSeek: Boolean;
Begin
  Result := False;
End;

Function TFrameVideoBase.CanSetRate: Boolean;
Begin
  Result := False;
End;

Function TFrameVideoBase.CanGrabFrame: Boolean;
Begin
  Result := False;
End;

Function TFrameVideoBase.SaveFrameToFile(Const AFilename: String): Boolean;
Begin
  Result := False;
End;

Function TFrameVideoBase.CopyFrameToClipboard: Boolean;
Begin
  Result := False;
End;

Function TFrameVideoBase.DefaultFrameFilename(AFolder: String; AExt: String): String;
Var
  sBase: String;
Begin
  If FStartDateTime = 0 Then
    sBase := FormatDateTime('hhnnss', PositionAsTime)
  Else
    sBase := FormatDateTime('yyyymmdd hhnnss', PositionAsTime);

  If (FChannel <> CHANNEL_PARAM) And (Trim(FChannel) <> '') Then
    sBase := sBase + '_' + Trim(FChannel);

  Result := UniqueFilename(AFolder, sBase + '_', AExt, False, 2);
End;

{ TFrameVideoBaseList }

Function TFrameVideoBaseList.GetVideo(Index: Integer): TFrameVideoBase;
Begin
  Result := TFrameVideoBase(Inherited Items[Index]);
End;

Procedure TFrameVideoBaseList.AddVideo(AVideo: TFrameVideoBase);
Begin
  Inherited Add(AVideo);
End;

End.
