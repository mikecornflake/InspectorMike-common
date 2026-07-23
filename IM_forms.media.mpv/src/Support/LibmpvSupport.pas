Unit LibmpvSupport;

{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : LazSerialSupport.pas
  Description
    My first Class Helper

    Helper unit for LazSerial.pas

  Source
    Copyright (c) 2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    2026-06-05: Creation and upload to Githib InspectorMike-Common
                   as part of  IM_common.lpk
    2026-06-19: Added this header & refactored
    2026-06-19: Refactored into split InspectorMike package structure
    2026-07-23: Refactored into new TThirdParty Class

  License
    This file is part of IM_units.lpk.

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
  Classes, SysUtils, ThirdPartySupport;

Type

  { TLibmpvSupport }

  TLibmpvSupport = Class(TThirdParty)
  Public
    Procedure DefineDefaults; Override;
    Procedure Initialise; Override;
  End;

Function LibmpvDLL: TLibmpvSupport;

Implementation

Uses
  Forms, OSSupport, FileSupport, FileUtil, libMPV.Client;

Var
  FLibmpv: TLibmpvSupport;

Function LibmpvDLL: TLibmpvSupport;
Begin
  Result := FLibmpv;
End;

{ TLibmpvSupport }

Procedure TLibmpvSupport.DefineDefaults;
Begin
  // CLI, we don't care if the exe is 32bit or 64bit
  FCPUSensitive := True;

  // Preparation for default Initialise
  FKeyFile := 'libmpv-2.dll';
  FKeyFolder := 'mpv';

  // Metadata
  FName := 'mpv';

  FSummary := 'mpv is a free (as in freedom) media player for the command line. ' +
    'It supports a wide variety of media file formats, audio and video codecs, ' +
    'and subtitle types.' + LineEnding + LineEnding + '- Version: 0.41.0-697-g13a3e3ad0 ' +
    LineEnding + '- Windows build: Shinchiro developer build';

  FProjectURL := 'https://mpv.io/';
  FCodeURL := 'hhttps://github.com/mpv-player/mpv';
End;

Procedure TLibmpvSupport.Initialise;
Var
  sFile: String;
Begin
  Inherited Initialise;

  If DirectoryExists(FFolder) Then
  Begin
    If Not IsLibMPV_Loaded Then
    Begin
      sFile := IncludeSlash(FFolder) + FKeyFile;

      FAvailable := (Load_libMPV(sFile) = MPV_ERROR_SUCCESS);
    End;
  End;
End;

Initialization
  FLibmpv := TLibmpvSupport.Create;

Finalization;
  FreeAndNil(FLibmpv);

End.
