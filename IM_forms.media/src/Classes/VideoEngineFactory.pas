Unit VideoEngineFactory;

{-------------------------------------------------------------------------------
  Package   : IM_forms.media
  Unit      : VideoEngineFactory.pas
  Description
    Factory to create registered Video Engines (ie mpv, vlc or mplayer etc).

  Source
    Copyright (c) 2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    2026-06-19: Created by ChatGPT 5.5 with direction from Mike Thompson

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
  Classes, SysUtils, FrameVideoBase;

Type
  TVideoEngineInfo = Class
  Public
    DisplayName: String;
    VideoEngineClass: TFrameVideoBaseClass;
    Constructor Create(Const ADisplayName: String; AClass: TFrameVideoBaseClass);
  End;

  TVideoEngineFactory = Class
  Private
  Class Var FEngines: TList;
  Class Var FDefaultClass: TFrameVideoBaseClass;

    Class Function GetEngines: TList; Static;
  Public
    Class Procedure RegisterEngine(Const ADisplayName: String;
      AClass: TFrameVideoBaseClass); Static;

    Class Function Count: Integer; Static;
    Class Function EngineInfo(AIndex: Integer): TVideoEngineInfo; Static;
    Class Property DefaultClass: TFrameVideoBaseClass Read FDefaultClass;
  End;

Implementation

Var
  i: Integer;

  { TVideoEngineInfo }

Constructor TVideoEngineInfo.Create(Const ADisplayName: String; AClass: TFrameVideoBaseClass);
Begin
  Inherited Create;
  DisplayName := ADisplayName;
  VideoEngineClass := AClass;
End;

{ TVideoEngineFactory }

Class Function TVideoEngineFactory.GetEngines: TList;
Begin
  If Not Assigned(FEngines) Then
    FEngines := TList.Create;

  Result := FEngines;
End;

Class Procedure TVideoEngineFactory.RegisterEngine(Const ADisplayName: String;
  AClass: TFrameVideoBaseClass);
Var
  i: Integer;
  oInfo: TVideoEngineInfo;
Begin
  If AClass = nil Then
    Raise Exception.Create('Cannot register nil video engine class');

  For i := 0 To GetEngines.Count - 1 Do
  Begin
    oInfo := TVideoEngineInfo(GetEngines[i]);

    If oInfo.VideoEngineClass = AClass Then
      Exit;
  End;

  oInfo := TVideoEngineInfo.Create(ADisplayName, AClass);
  GetEngines.Add(oInfo);

  If FDefaultClass = nil Then
    FDefaultClass := AClass;
End;

Class Function TVideoEngineFactory.Count: Integer;
Begin
  Result := GetEngines.Count;
End;

Class Function TVideoEngineFactory.EngineInfo(AIndex: Integer): TVideoEngineInfo;
Begin
  Result := TVideoEngineInfo(GetEngines[AIndex]);
End;

Finalization
  If Assigned(TVideoEngineFactory.FEngines) Then
  Begin
    For i := 0 To TVideoEngineFactory.FEngines.Count - 1 Do
      TObject(TVideoEngineFactory.FEngines[i]).Free;

    TVideoEngineFactory.FEngines.Free;
  End;

End.
