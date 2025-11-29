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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, FrameBase;

Type
  TTimerEvent = Procedure(Sender: TObject; CurrentPos, Length: Cardinal) Of Object;

  { TfmeVideoBase }

  TfmeVideoBase = Class(TfmeBase)
  Protected
    FFilename: String;
    FOnTimer: TTimerEvent;

    Function GetPosition: Integer; Virtual;
    Function GetRate: Double; Virtual;
    Procedure SetFilename(AValue: String); Virtual;
    Procedure SetPosition(AValue: Integer); Virtual;
    Procedure SetRate(AValue: Double); Virtual;
  Public
    Constructor Create(TheOwner: TComponent); Override;

    Function Play: Boolean; Virtual;
    Function Pause: Boolean; Virtual;
    Function Resume: Boolean; Virtual;
    Function Stop: Boolean; Virtual;

    Function Paused: Boolean; Virtual;

    Function Duration: TDateTime; Virtual;

    Function CanRewind: Boolean; Virtual;
    Function GetBitmap(Bitmap: TBitmap): Boolean; Virtual;

    Property Filename: String read FFilename write SetFilename;
    Property OnTimer: TTimerEvent read FOnTimer write FOnTimer;

    Property Rate: Double read GetRate write SetRate;
    Property Position: Integer read GetPosition write SetPosition;
  End;

Implementation

{$R *.lfm}

{ TfmeVideoBase }

Constructor TfmeVideoBase.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  FFilename := '';
  FOnTimer := nil;
End;

Function TfmeVideoBase.GetPosition: Integer;
Begin
  Result := 0;
End;

Function TfmeVideoBase.GetRate: Double;
Begin
  Result := 0.0;
End;

Procedure TfmeVideoBase.SetFilename(AValue: String);
Begin
  If FFilename = AValue Then
    Exit;
  FFilename := AValue;
End;

Procedure TfmeVideoBase.SetPosition(AValue: Integer);
Begin

End;

Procedure TfmeVideoBase.SetRate(AValue: Double);
Begin

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

Function TfmeVideoBase.Paused: Boolean;
Begin
  Result := False;
End;

Function TfmeVideoBase.Duration: TDateTime;
Begin
  Result := -1;
End;

Function TfmeVideoBase.CanRewind: Boolean;
Begin
  Result := False;
End;

Function TfmeVideoBase.GetBitmap(Bitmap: TBitmap): Boolean;
Begin
  Result := False;
End;

End.
