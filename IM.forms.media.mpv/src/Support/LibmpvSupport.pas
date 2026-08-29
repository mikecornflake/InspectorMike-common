Unit LibmpvSupport;

{-------------------------------------------------------------------------------
  Package   : IM.units
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
                   as part of  IM.common.lpk
    2026-06-19: Added this header & refactored
    2026-06-19: Refactored into split InspectorMike package structure
    2026-07-23: Refactored into new TThirdParty Class

  License
    This file is part of IM.units.lpk.

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
    Constructor Create; Override;

    Procedure Initialise; Override;
  End;

Function LibmpvDLL: TLibmpvSupport;

Const
  THIRDPARTY_LIBMPV = 'libmpv';
  THIRDPARTY_UW_MPVPLAYER = 'UW_MPVPlayer';

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

Constructor TLibmpvSupport.Create;
Var
  oDef: TThirdPartyDefinition;
Begin
  oDef := Default(TThirdPartyDefinition);

  // Dynamically Linked DLL
  oDef.Kind := tpkRuntimeLibrary;

  // DLL - we care if the exe is 32bit or 64bit
  oDef.CPUSensitive := True;

  // Preparation for default Initialise
  oDef.KeyFile := 'libmpv-2.dll';
  oDef.KeyFolder := 'mpv';

  // Metadata
  oDef.Name := THIRDPARTY_LIBMPV;

  oDef.Summary := 'mpv is a free (as in freedom) media player for the command line or as a library. ' +
    'mpv supports a wide variety of media file formats, audio and video codecs, ' +
    'and subtitle types.' + LineEnding + LineEnding + '- Version: 0.41.0-697-g13a3e3ad0 ' +
    LineEnding + '- Windows build: Shinchiro developer build';

  oDef.ProjectURL := 'https://mpv.io/';
  oDef.CodeURL := 'https://github.com/mpv-player/mpv';

  Inherited Create(oDef);

  // This unit self registers
  FUsed := True;

  // Now acknowledge the creators of the libmpv wrapper.
  oDef := Default(TThirdPartyDefinition);

  oDef.Name := THIRDPARTY_UW_MPVPLAYER;
  oDef.Summary := 'This is the pascal wrapper for the libmpv media player library' + LineEnding +
    LineEnding + 'libmpv is a powerful multimedia playback engine. It supports a wide variety of media file formats, audio and video codecs, and subtitle types';
  oDef.ProjectURL := 'https://www.uruworks.net/index.html';
  oDef.CodeURL := 'https://github.com/URUWorks/UW_MPVPlayer';
  oDef.Kind := tpkLazarusPackage;
  oDef.KeyFile := 'Readme.md';
  oDef.KeyFolder := THIRDPARTY_UW_MPVPLAYER;
  oDef.CPUSensitive := False;
  TThirdParty.Create(oDef);
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

  ThirdParties.Include([THIRDPARTY_UW_MPVPLAYER]);

Finalization;
  // Free'd by FThirdParties
  //FreeAndNil(FLibmpv);

End.
