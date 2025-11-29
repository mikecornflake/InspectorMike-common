Unit ffmpegSupport;

{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : ffmpegSupport.pas
  Description
    CLI support for ffmpeg and ffprobe

    ffprobe is a useful implementation.  Example use can be found in FileRenamer2

    My use of ffmpeg is so simple, I mostly just use this unit to find WHERE
    I've placed ffmpeg.exe...

  Source
    Copyright (c) 2025
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    ~2008: Creation date unknown, original local SVN repository lost
    2014-07-05: Uploaded to SourceForge/Package "Shared"
    2021-12-01: Last commit in SourceForge
    2024-01-22: Migrated to Github.  Refactored package to "IM_units"
    2025-11-29: Added this header

  License
    This file is part of IM_units.lpk.

    It is free software: you can redistribute it and/or modify it under the
    terms of the GNU General Public License as published by the Free Software
    Foundation, either version 3 of the License, or (at your option) any
    later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program.  If not, see <https://www.gnu.org/licenses/>.

    SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------}
{$mode objfpc}{$H+}

// Running the command line interface itself is so easy I see no
// need to subclass that code.
// This is really here just to find the directory

Interface

Uses
  Classes, SysUtils;

Type
  TMediaInfo = Record
    Filename: String;
    Format: String;
    StreamCount: Integer;
    Duration: Extended;
    Width, Height: Integer;

    V_Stream: Integer;
    V_Codec: String;
    V_Profile: String;
    V_pixfmt: String;

    A_Stream: Integer;
    A_Codec: String;

    S_Stream: Integer;
    S_Codec: String;

    RAW: String;
  End;

Function FFmpegAvailable: Boolean;
Function FFmpegPath: String;
Procedure SetFFmpegPath(AValue: String);
Procedure InitializeFFmpeg;

Function ProbeFile(sFilename: String): String;
Function ExtractStreamByCodec(AInput, ACodec: String): String; // Results will be in lowercase
Function ExtractStreamByCodecType(AInput, ACodecType: String): String;
// Results will be in lowercase
Function ExtractFormat(sInput: String): String;        // Results will be in lowercase

Function MediaInfo(AFilename: String): TMediaInfo;

Function CreateThumbnail(sVideo, sThumbnail: String): String;

Const
  FFmpegHelpAboutBlurb = '<html><body>' +
    'This software uses libraries from the FFmpeg project under the LGPLv2.1<br>' +
    '<br>' + 'Where used, functionality of FFmpeg is obtained by running the static ' +
    'command line utilities.&nbsp; The command line utilities themselves use ' +
    'other third party libraries, and full details of the licenses for each ' +
    'of these utilites can be found in the <a href="FFMPEGLibraryLicenses">ffmpeg\licenses</a> ' +
    'folder distributed with this software.<br/><br/>' + '<b>FFmpeg Homepage</b><br/>' +
    '&nbsp;&nbsp;<a href="http://www.ffmpeg.org/">http://www.ffmpeg.org/</a>' +
    '<br><br/>' + '<b>FFmpeg License and mini-Patent FAQ</b><br/>' +
    '&nbsp;&nbsp;<a href="http://www.ffmpeg.org/legal.html">http://www.ffmpeg.org/legal.html</a>' +
    '<br><br/>' + 'This software uses code of <a href="http://ffmpeg.org">FFmpeg</a> ' +
    'licensed under the ' +
    '<a href="http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html">LGPLv2.1</a> ' +
    'and the source code can be obtained by sending an email to ' +
    '<a href="mailto:mike.cornflake@gmail.com">mailto:mike.cornflake@gmail.com</a> ' +
    'and requesting a copy.' + '</body></html>';

Implementation

Uses
  Forms, StringSupport, FileUtil, OSSupport;

Var
  FFFmpegPath: String;

Function FFmpegAvailable: Boolean;
Begin
  Result := FFFmpegPath <> '';
End;

Function FFmpegPath: String;
Begin
  Result := FFFmpegPath;
End;

Procedure SetFFmpegPath(AValue: String);
Begin
  If DirectoryExists(AValue) Then
    FFFmpegPath := AValue
  Else
    FFFmpegPath := '';
End;

Procedure InitializeFFmpeg;
Begin
  If FFFmpegPath = '' Then
  Begin
    // By default, use the ffpmeg folder distributed with the app
    FFFmpegPath := IncludeTrailingBackslash(Application.Location) + 'ffmpeg\bin';
    If DirectoryExists(FFFmpegPath) Then
      Exit;

    // Maybe it was installed in the same folder as the app?
    FFFmpegPath := IncludeTrailingBackslash(Application.Location) + '..\ffmpeg\bin';
    If DirectoryExists(FFFmpegPath) Then
      Exit;

    // Oh well, search the evironment PATH for the exe...
    FFFmpegPath := FindDefaultExecutablePath(Format('ffprobe%s', [GetExeExt]));
    If DirectoryExists(FFFmpegPath) Then
      Exit;

    FFFmpegPath := '';
  End;
End;

Function ProbeFile(sFilename: String): String;
Var
  sCommand: String;
Begin
  Result := '';
  If FFFmpegPath <> '' Then
  Begin
    sCommand := Format('%s\ffprobe%s -v quiet -show_format -show_streams "%s"',
      [FFmpegPath, GetExeExt, sFilename]);
    Result := RunEx(sCommand, nil, True);
  End;
End;

Function CreateThumbnail(sVideo, sThumbnail: String): String;
Var
  sCommand, sTemp: String;

  Function GrabThumbnail(sFromTime: String): Boolean;
  Begin
    sCommand := Format('%s\ffmpeg%s -hide_banner -i "%s" -ss %s -frames:v 1 "%s"',
      [FFmpegPath, GetExeExt, sVideo, sFromTime, sThumbnail]);

    sTemp := RunEx(sCommand, nil, True);

    Result := FileExists(sThumbnail);
  End;

Begin
  Result := '';
  If FFFmpegPath <> '' Then
  Begin
    If Not GrabThumbnail('00:01:00') Then
      If Not GrabThumbnail('00:00:10') Then
        GrabThumbnail('00:00:01');

    If FileExists(sThumbnail) Then
      Result := sTemp;
  End;
End;

Function ExtractStreamByCodec(AInput, ACodec: String): String;
Var
  sTemp: String;
  sResult: String;
  sCodecFound: String;
  i: Integer;
  bEnd: Boolean;
Begin
  sTemp := Lowercase(AInput);
  Result := '';
  bEnd := False;

  While Not bEnd Do
  Begin
    sResult := TextBetween(sTemp, '[stream]', '[/stream]');

    sCodecFound := FindReplace(TextBetween(sResult, 'codec_name=', #13), #13, '');
    If sCodecFound = Lowercase(ACodec) Then
    Begin
      Result := sResult;
      bEnd := True;
    End
    Else
    Begin
      i := Pos('[/stream]', sTemp);

      If i > 0 Then
        sTemp := Trim(Copy(sTemp, i + 9, Length(sTemp) - (i + 9) + 1));

      bEnd := (i = 0) Or (sTemp = '');
    End;
  End;
End;

Function ExtractStreamByCodecType(AInput, ACodecType: String): String;
Var
  sTemp: String;
  sResult: String;
  sCodecTypeFound: String;
  i: Integer;
  bEnd: Boolean;
Begin
  sTemp := Lowercase(AInput);
  Result := '';
  bEnd := False;

  While Not bEnd Do
  Begin
    sResult := TextBetween(sTemp, '[stream]', '[/stream]');

    sCodecTypeFound := FindReplace(TextBetween(sResult, 'codec_type=', #13), #13, '');
    If sCodecTypeFound = Lowercase(ACodecType) Then
    Begin
      Result := sResult;
      bEnd := True;
    End
    Else
    Begin
      i := Pos('[/stream]', sTemp);

      If i > 0 Then
        sTemp := Trim(Copy(sTemp, i + 9, Length(sTemp) - (i + 9) + 1));

      bEnd := (i = 0) Or (sTemp = '');
    End;
  End;
End;

Function ExtractFormat(sInput: String): String;
Var
  sTemp: String;
Begin
  sTemp := Lowercase(sInput);

  Result := Trim(TextBetween(sTemp, '[format]', '[/format]'));
End;

Function MediaInfo(AFilename: String): TMediaInfo;

  Function ExtractField(AInput, AField: String): String;
  Begin
    Result := Trim(FindReplace(TextBetween(AInput, AField + '=', #13), #13, ''));
  End;

Var
  sFFPROBE, sFormat, sCodec, sTemp: String;
Begin
  sFFPROBE := Lowercase(ProbeFile(AFilename));
  sFormat := ExtractFormat(sFFPROBE);

  If sFormat <> '' Then
  Begin
    Result.Filename := AFilename;
    Result.RAW := sFFPROBE;

    Result.Format := ExtractField(sFormat, 'format_name');
    Result.StreamCount := StrToIntDef(ExtractField(sFormat, 'nb_streams'), -1);
    Result.Duration := StrToFloatDef(ExtractField(sFormat, 'duration'), -1);

    sCodec := ExtractStreamByCodecType(sFFPROBE, 'audio');
    If sCodec <> '' Then
    Begin
      Result.A_Stream := StrToIntDef(ExtractField(sCodec, 'index'), -1);
      Result.A_Codec := ExtractField(sCodec, 'codec_name');
    End
    Else
      Result.A_Stream := -1;

    sCodec := ExtractStreamByCodecType(sFFPROBE, 'video');
    If sCodec <> '' Then
    Begin
      Result.V_Stream := StrToIntDef(ExtractField(sCodec, 'index'), -1);
      sTemp := ExtractField(sCodec, 'codec_tag_string');
      If (sTemp = '') Or (LeftStr(sTemp, 3) = '[0]') Then
        sTemp := ExtractField(sCodec, 'codec_name');

      Result.V_Codec := Format('%s [%s]', [sTemp, ExtractField(sCodec, 'codec_long_name')]);

      Result.Width := StrToIntDef(ExtractField(sCodec, 'width'), -1);
      Result.Height := StrToIntDef(ExtractField(sCodec, 'height'), -1);

      Result.V_Profile := ExtractField(sCodec, 'profile');
      Result.V_pixfmt := ExtractField(sCodec, 'pix_fmt');
    End
    Else
      Result.V_Stream := -1;

    sCodec := ExtractStreamByCodecType(sFFPROBE, 'subtitle');
    If sCodec <> '' Then
    Begin
      Result.S_Stream := StrToIntDef(ExtractField(sCodec, 'index'), -1);
      Result.S_Codec := ExtractField(sCodec, 'codec_name');
    End
    Else
      Result.S_Stream := -1;
  End;
End;

Initialization
  FFFmpegPath := '';

End.
