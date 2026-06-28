Unit InspectionSupport;

{$mode objfpc}{$H+}

{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : InspectionSupport.pas
  Description
    Helper routines for common subsea inspection packages

  Source
    Copyright (c) 2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    2026-06-28: Creation by ChatGPT5.5 under direction by Mike Thompson

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

Interface

Uses
  Classes, SysUtils;

Type
  TInspectionFilenameInfo = Record
    FoundDateTime: Boolean;
    DateTime: TDateTime;

    FoundChannel: Boolean;
    Channel: String;

    Confidence: Integer;     // 0..100
    FormatName: String;
    DateTimeText: String;
  End;

Function TryParseInspectionFilename(Const AFilename: String;
  out AInfo: TInspectionFilenameInfo): Boolean;
Function FindFilesStartingInWindow(Const AFilename: String;
  Const AStartDateTime, AEndDateTime: TDateTime): TStringArray;

Implementation

Uses
  StrUtils;

Function FindFilesStartingInWindow(Const AFilename: String;
  Const AStartDateTime, AEndDateTime: TDateTime): TStringArray;
Var
  Folder: String;
  Ext: String;
  SearchRec: TSearchRec;
  SearchMask: String;
  FullName: String;
  Info: TInspectionFilenameInfo;
  Matches: TStringList;
Begin
  SetLength(Result, 0);

  Folder := ExtractFilePath(AFilename);
  Ext := ExtractFileExt(AFilename);

  If Folder = '' Then
    Folder := IncludeTrailingPathDelimiter(GetCurrentDir);

  SearchMask := IncludeTrailingPathDelimiter(Folder) + '*' + Ext;

  Matches := TStringList.Create;
  Try
    If FindFirst(SearchMask, faAnyFile And Not faDirectory, SearchRec) = 0 Then
    Begin
      Try
        Repeat
          FullName := IncludeTrailingPathDelimiter(Folder) + SearchRec.Name;

          If TryParseInspectionFilename(FullName, Info) Then
          Begin
            If Info.FoundDateTime Then
            Begin
              If (Info.DateTime >= AStartDateTime) And
                (Info.DateTime <= AEndDateTime) Then
              Begin
                Matches.Add(FullName);
              End;
            End;
          End;

        Until FindNext(SearchRec) <> 0;
      Finally
        FindClose(SearchRec);
      End;
    End;

    Matches.Sort;
    Result := Matches.ToStringArray;
  Finally
    Matches.Free;
  End;
End;

Function DigitsOnly(Const S: String): Boolean;
Var
  i: Integer;
Begin
  Result := S <> '';
  For i := 1 To Length(S) Do
    If Not (S[i] In ['0'..'9']) Then
      Exit(False);
End;

Function TryMakeDateTime(Const Y, M, D, H, N, S: Integer; out ADateTime: TDateTime): Boolean;
Var
  DatePart, TimePart: TDateTime;
Begin
  Result := False;

  If Not TryEncodeDate(Y, M, D, DatePart) Then
    Exit;

  If Not TryEncodeTime(H, N, S, 0, TimePart) Then
    Exit;

  ADateTime := DatePart + TimePart;
  Result := True;
End;

Function TwoDigitYearToFullYear(Const YY: Integer): Integer;
Begin
  // Sensible inspection-video assumption.
  // 70..99 => 1970..1999
  // 00..69 => 2000..2069
  If YY >= 70 Then
    Result := 1900 + YY
  Else
    Result := 2000 + YY;
End;

Procedure ClearInfo(out AInfo: TInspectionFilenameInfo);
Begin
  FillChar(AInfo, SizeOf(AInfo), 0);
  AInfo.Channel := '';
  AInfo.FormatName := '';
  AInfo.DateTimeText := '';
End;

Function StripPathAndExt(Const AFilename: String): String;
Begin
  Result := ChangeFileExt(ExtractFileName(AFilename), '');
End;

Function TokenIsCount(Const S: String): Boolean;
Begin
  Result := DigitsOnly(S);
End;

Function CleanChannel(Const S: String): String;
Begin
  Result := Trim(S);

  // Strip common separators accidentally retained.
  While (Result <> '') And (Result[1] In ['_', '-', ' ', '@']) Do
    Delete(Result, 1, 1);

  While (Result <> '') And (Result[Length(Result)] In ['_', '-', ' ', '@']) Do
    Delete(Result, Length(Result), 1);
End;

Function TryParseCompactDateTime(Const S: String; Const StartIndex: Integer;
  out ADateTime: TDateTime; out AToken: String): Boolean;
Var
  Y, M, D, H, N, Sec: Integer;
Begin
  Result := False;
  AToken := '';

  If StartIndex + 13 > Length(S) Then
    Exit;

  AToken := Copy(S, StartIndex, 14);

  If Not DigitsOnly(AToken) Then
    Exit;

  Y := StrToIntDef(Copy(AToken, 1, 4), -1);
  M := StrToIntDef(Copy(AToken, 5, 2), -1);
  D := StrToIntDef(Copy(AToken, 7, 2), -1);
  H := StrToIntDef(Copy(AToken, 9, 2), -1);
  N := StrToIntDef(Copy(AToken, 11, 2), -1);
  Sec := StrToIntDef(Copy(AToken, 13, 2), -1);

  Result := TryMakeDateTime(Y, M, D, H, N, Sec, ADateTime);
End;

Function TryParseOptionsDVR(Const BaseName: String; out AInfo: TInspectionFilenameInfo): Boolean;
Var
  AtPos: Integer;
  DT: TDateTime;
  Token: String;
Begin
  Result := False;

  AtPos := Pos('@', BaseName);
  If AtPos < 15 Then
    Exit;

  If Not TryParseCompactDateTime(BaseName, 1, DT, Token) Then
    Exit;

  AInfo.FoundDateTime := True;
  AInfo.DateTime := DT;
  AInfo.DateTimeText := Token;

  AInfo.Channel := CleanChannel(Copy(BaseName, AtPos + 1, MaxInt));
  AInfo.FoundChannel := AInfo.Channel <> '';

  AInfo.Confidence := 95;
  AInfo.FormatName := 'OptionsDVR / VisualWorks';
  Result := True;
End;

Function TryParseNexusDVR(Const BaseName: String; out AInfo: TInspectionFilenameInfo): Boolean;
Var
  Parts: TStringArray;
  DateText, TimeText: String;
  Y, M, D, H, N, Sec: Integer;
  DT: TDateTime;
Begin
  Result := False;

  Parts := BaseName.Split(' - ');
  If Length(Parts) < 2 Then
    Exit;

  // Example:
  // 2014-04-26 204214 - lynx42 - 1
  If Length(Parts[0]) <> 17 Then
    Exit;

  DateText := Copy(Parts[0], 1, 10);
  TimeText := Copy(Parts[0], 12, 6);

  If (DateText[5] <> '-') Or (DateText[8] <> '-') Then
    Exit;

  If Not DigitsOnly(StringReplace(DateText, '-', '', [rfReplaceAll])) Then
    Exit;

  If Not DigitsOnly(TimeText) Then
    Exit;

  Y := StrToIntDef(Copy(DateText, 1, 4), -1);
  M := StrToIntDef(Copy(DateText, 6, 2), -1);
  D := StrToIntDef(Copy(DateText, 9, 2), -1);
  H := StrToIntDef(Copy(TimeText, 1, 2), -1);
  N := StrToIntDef(Copy(TimeText, 3, 2), -1);
  Sec := StrToIntDef(Copy(TimeText, 5, 2), -1);

  If Not TryMakeDateTime(Y, M, D, H, N, Sec, DT) Then
    Exit;

  AInfo.FoundDateTime := True;
  AInfo.DateTime := DT;
  AInfo.DateTimeText := Parts[0];

  AInfo.Channel := CleanChannel(Parts[1]);
  AInfo.FoundChannel := AInfo.Channel <> '';

  AInfo.Confidence := 95;
  AInfo.FormatName := 'Nexus DVR';
  Result := True;
End;

Function TryParseCoabis(Const BaseName: String; out AInfo: TInspectionFilenameInfo): Boolean;
Var
  Parts: TStringArray;
  i: Integer;
  DateText, TimeText: String;
  YY, Y, M, D, H, N, Sec: Integer;
  DT: TDateTime;
Begin
  Result := False;

  Parts := BaseName.Split('_');

  // Look for:
  // yy-mm-dd_hh-nn-ss_count
  For i := 0 To High(Parts) - 1 Do
  Begin
    DateText := Parts[i];
    TimeText := Parts[i + 1];

    If (Length(DateText) <> 8) Or (Length(TimeText) <> 8) Then
      Continue;

    If (DateText[3] <> '-') Or (DateText[6] <> '-') Then
      Continue;

    If (TimeText[3] <> '-') Or (TimeText[6] <> '-') Then
      Continue;

    If Not DigitsOnly(StringReplace(DateText, '-', '', [rfReplaceAll])) Then
      Continue;

    If Not DigitsOnly(StringReplace(TimeText, '-', '', [rfReplaceAll])) Then
      Continue;

    YY := StrToIntDef(Copy(DateText, 1, 2), -1);
    Y := TwoDigitYearToFullYear(YY);
    M := StrToIntDef(Copy(DateText, 4, 2), -1);
    D := StrToIntDef(Copy(DateText, 7, 2), -1);

    H := StrToIntDef(Copy(TimeText, 1, 2), -1);
    N := StrToIntDef(Copy(TimeText, 4, 2), -1);
    Sec := StrToIntDef(Copy(TimeText, 7, 2), -1);

    If Not TryMakeDateTime(Y, M, D, H, N, Sec, DT) Then
      Continue;

    AInfo.FoundDateTime := True;
    AInfo.DateTime := DT;
    AInfo.DateTimeText := DateText + '_' + TimeText;

    AInfo.FoundChannel := False;
    AInfo.Channel := '';

    AInfo.Confidence := 85;
    AInfo.FormatName := 'Coabis';
    Exit(True);
  End;
End;

Function TryParseFDVR(Const BaseName: String; out AInfo: TInspectionFilenameInfo): Boolean;
Var
  Parts: TStringArray;
  i: Integer;
  DT: TDateTime;
  Token: String;
Begin
  Result := False;

  Parts := BaseName.Split('_');

  // Look for:
  // anything_yyyymmddhhmmss_channel
  For i := 0 To High(Parts) - 1 Do
  Begin
    If Length(Parts[i]) <> 14 Then
      Continue;

    If Not TryParseCompactDateTime(Parts[i], 1, DT, Token) Then
      Continue;

    AInfo.FoundDateTime := True;
    AInfo.DateTime := DT;
    AInfo.DateTimeText := Token;

    AInfo.Channel := CleanChannel(Parts[i + 1]);
    AInfo.FoundChannel := AInfo.Channel <> '';

    AInfo.Confidence := 90;
    AInfo.FormatName := 'Fugro FDVR';
    Exit(True);
  End;
End;

Function TryParseGenericCompactDateTime(Const BaseName: String;
  out AInfo: TInspectionFilenameInfo): Boolean;
Var
  i: Integer;
  DT: TDateTime;
  Token: String;
Begin
  Result := False;

  For i := 1 To Length(BaseName) - 13 Do
  Begin
    If TryParseCompactDateTime(BaseName, i, DT, Token) Then
    Begin
      AInfo.FoundDateTime := True;
      AInfo.DateTime := DT;
      AInfo.DateTimeText := Token;

      AInfo.FoundChannel := False;
      AInfo.Channel := '';

      AInfo.Confidence := 60;
      AInfo.FormatName := 'Generic compact timestamp';
      Exit(True);
    End;
  End;
End;

Function TryParseInspectionFilename(Const AFilename: String;
  out AInfo: TInspectionFilenameInfo): Boolean;
Var
  BaseName: String;
Begin
  ClearInfo(AInfo);

  BaseName := StripPathAndExt(AFilename);

  Result :=
    TryParseOptionsDVR(BaseName, AInfo) Or TryParseNexusDVR(BaseName, AInfo) Or
    TryParseCoabis(BaseName, AInfo) Or TryParseFDVR(BaseName, AInfo) Or
    TryParseGenericCompactDateTime(BaseName, AInfo);
End;

End.
