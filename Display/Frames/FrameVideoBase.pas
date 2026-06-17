Unit FrameVideoBase;

{-------------------------------------------------------------------------------
  Package   : IM_application
  Unit      : FrameVideoBase.pas
  Description
    Abstract base frame for video playback implementations.

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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  FrameBase, fgl;

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

  TfmeVideoBase = Class;

  TfmeVideoBaseClass = Class Of TfmeVideoBase;
  TfmeVideoBaseList = Specialize TFPGObjectList<TfmeVideoBase>;

  { TfmeVideoBase }

  TfmeVideoBase = Class(TfmeBase)
  Private
  Protected
    FFilename: String;
    FOnPosition: TPositionEvent;
    FOnStateChanged: TStateEvent;
    FVideoFileCount: Integer;
    FAutoplay: Boolean;

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

    Function Load(Const AFilename: String): Boolean; Virtual;
    Function Play: Boolean; Virtual;
    Function Pause: Boolean; Virtual;
    Function Resume: Boolean; Virtual;
    Function Stop: Boolean; Virtual;
    Function Clear: Boolean; Virtual;


    Function CanSeek: Boolean; Virtual;
    Function CanSetRate: Boolean; Virtual;
    Function CanGrabBitmap: Boolean; Virtual;

    Function GetBitmap(Bitmap: TBitmap): Boolean; Virtual;

    Property Filename: String Read FFilename;
    Property Position: TVideoTime Read GetPosition Write SetPosition;
    Property Duration: TVideoTime Read GetDuration;
    Property Muted: Boolean Read GetMuted Write SetMuted;
    Property Rate: Double Read GetRate Write SetRate;
    Property State: TVideoState Read GetState;

    Property Autoplay: Boolean Read FAutoplay Write SetAutoplay;

    Property OnPosition: TPositionEvent Read FOnPosition Write FOnPosition;
    Property OnStateChanged: TStateEvent Read FOnStateChanged Write FOnStateChanged;

    Property VideoFileCount: Integer Read FVideoFileCount;
  End;

Implementation

{$R *.lfm}

{ TfmeVideoBase }

Constructor TfmeVideoBase.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  FAutoplay := False;
  FFilename := '';
  FOnPosition := nil;
  FOnStateChanged := nil;

  // Each descendent should set this when files are correctly loaded.
  FVideoFileCount := 0;
End;

Procedure TfmeVideoBase.SetAutoplay(AValue: Boolean);
Begin
  FAutoplay := AValue;
End;

Function TfmeVideoBase.GetPosition: TVideoTime;
Begin
  Result := 0;
End;

Procedure TfmeVideoBase.SetPosition(AValue: TVideoTime);
Begin
  // Abstract base: descendant handles seeking.
End;

Function TfmeVideoBase.GetDuration: TVideoTime;
Begin
  Result := -1;
End;

Function TfmeVideoBase.GetRate: Double;
Begin
  Result := 1.0;
End;

Procedure TfmeVideoBase.SetRate(AValue: Double);
Begin
  // Abstract base: descendant handles playback rate.
End;

Function TfmeVideoBase.GetState: TVideoState;
Begin
  Result := vsEmpty;
End;

Function TfmeVideoBase.GetMuted: Boolean;
Begin
  Result := False;
End;

Procedure TfmeVideoBase.SetMuted(AValue: Boolean);
Begin
  //Descendant handles muting
End;

Procedure TfmeVideoBase.DoPosition;
Begin
  If Assigned(FOnPosition) Then
    FOnPosition(Self, Position, Duration);
End;

Procedure TfmeVideoBase.DoStateChanged;
Begin
  If Assigned(FOnStateChanged) Then
    FOnStateChanged(Self, State);
End;

Function TfmeVideoBase.Load(Const AFilename: String): Boolean;
Begin
  FFilename := AFilename;
  Result := FileExists(AFilename);
End;

Function TfmeVideoBase.Play: Boolean;
Begin
  Result := False;
End;

Function TfmeVideoBase.Pause: Boolean;
Begin
  Result := False;
End;

Function TfmeVideoBase.Resume: Boolean;
Begin
  Result := False;
End;

Function TfmeVideoBase.Stop: Boolean;
Begin
  Result := False;
End;

Function TfmeVideoBase.Clear: Boolean;
Begin
  FFilename := '';
  Result := True;
End;

Function TfmeVideoBase.CanSeek: Boolean;
Begin
  Result := False;
End;

Function TfmeVideoBase.CanSetRate: Boolean;
Begin
  Result := False;
End;

Function TfmeVideoBase.CanGrabBitmap: Boolean;
Begin
  Result := False;
End;

Function TfmeVideoBase.GetBitmap(Bitmap: TBitmap): Boolean;
Begin
  Result := False;
End;

End.
