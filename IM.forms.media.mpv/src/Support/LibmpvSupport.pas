Unit LibmpvSupport;

{-------------------------------------------------------------------------------
  Package   : IM.units
  Unit      : LibmpvSupport.pas
  Description
    Support unit for URUWorks libMPV

  Source
    Copyright (c) 2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    TODO: CHECK THE ACTUAL HISTORY IN GITHUB, THIS FEELS COPY/PASTE
    2026-06-05: Creation and upload to Githib InspectorMike-Common
                   as part of  IM.common.lpk
    2026-06-19: Added this header & refactored
    2026-06-19: Refactored into split InspectorMike package structure
    2026-07-23: Refactored into new TThirdParty Class

  License
    This file is part of IM.forms.media.mpv.lpk.

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

// Extract audio as pcm from any video file mpv can handle
// Save it as a Wav File
// Audio properties from input video file are preserved
// (ie # of channels, sampling frequency etc)
Function Libmpv_ExtractAudio(AInputVideoFile: String; AOutputWaveFile: String): Boolean;

Function LibmpvDLL: TLibmpvSupport;

Const
  THIRDPARTY_LIBMPV = 'libmpv';
  THIRDPARTY_UW_MPVPLAYER = 'UW_MPVPlayer';

Implementation

Uses
  Forms, OSSupport, FileSupport, FileUtil, libMPV.Client, Math;

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

  oDef.Summary := 'mpv is a free (as in freedom) media player for the command line or as a library. '
    + 'mpv supports a wide variety of media file formats, audio and video codecs, ' +
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
  oDef.Summary := 'This is the pascal wrapper for the libmpv media player library' +
    LineEnding + LineEnding +
    'libmpv is a powerful multimedia playback engine. It supports a wide variety of media file formats, audio and video codecs, and subtitle types';
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

// Extract Audio as pcm from any video file mpv can handle
// Save it as a Wav File
// Audio properties as per input video file
// (ie # of channels, sampling frequency etc)
// Chatgpt 5.6 Sol Oct 2026
Function Libmpv_ExtractAudio(AInputVideoFile: String; AOutputWaveFile: String): Boolean;
Var
  mpv: Pmpv_handle;
  Args: Array[0..2] Of PChar;
  Err: Integer;
  Event: Pmpv_event;
  Finished: Boolean;
Begin
  Result := False;

  If Not IsLibMPV_Loaded Then
    Exit;

  If Not FileExists(AInputVideoFile) Then
    Exit;

  SetExceptionMask(GetExceptionMask + [exInvalidOp]);

  mpv := mpv_create();
  If mpv = nil Then
    Raise Exception.Create('Unable to create mpv instance');

  Try
    // We don't want video decoded/displayed.
    mpv_set_option_string(mpv^, 'video', 'no');

    // Decode audio to a PCM/WAVE file.
    mpv_set_option_string(mpv^, 'ao', 'pcm');
    mpv_set_option_string(mpv^, 'ao-pcm-file', PChar(AOutputWaveFile));

    // For waveform generation, mono is enough.
    mpv_set_option_string(mpv^, 'audio-channels', 'mono');

    Err := mpv_initialize(mpv^);
    If Err < 0 Then
      Raise Exception.CreateFmt('mpv_initialize failed: %s', [mpv_error_string(Err)]);

    Args[0] := 'loadfile';
    Args[1] := PChar(AInputVideoFile);
    Args[2] := nil;

    Err := mpv_command(mpv^, @Args[0]);
    If Err < 0 Then
      Raise Exception.CreateFmt('loadfile failed: %s', [mpv_error_string(Err)]);

    // loadfile starts processing, but we now need to wait for EOF.
    Finished := False;

    While Not Finished Do
    Begin
      // Wait up to 1 second for an event.
      Event := mpv_wait_event(mpv^, 1.0);

      If Event = nil Then
        Continue;

      Case Event^.event_id Of

        //MPV_EVENT_NONE:
        //Begin
        //  //WriteLn(' Timeout');
        //End;
        //
        //MPV_EVENT_FILE_LOADED:
        //Begin
        //  //WriteLn(' File loaded');
        //End;
        //
        //MPV_EVENT_START_FILE:
        //Begin
        //  //WriteLn(' Start of file');
        //End;

        MPV_EVENT_END_FILE:
        Begin
          //WriteLn(' End of file');
          Finished := True;
        End;

        MPV_EVENT_SHUTDOWN:
        Begin
          //WriteLn('MPV shutdown');
          Finished := True;
        End;
//
//        Else
//        Begin
//          //WriteLn(' Unknown event', Event^.event_id);
//        End;
      End;
    End;
    Result := True;
  Finally
    mpv_terminate_destroy(mpv^);
  End;
End;

Initialization
  FLibmpv := TLibmpvSupport.Create;

  ThirdParties.Include([THIRDPARTY_UW_MPVPLAYER]);

Finalization;
  // Free'd by FThirdParties
  //FreeAndNil(FLibmpv);

End.
