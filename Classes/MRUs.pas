Unit MRUs;

{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : MRUs.pas
  Description
    Unit to manage "Most Recently Used..." menu

  TODO
    This isn't working.  Not sure why, and stopped using it.  Still a good idea
    so lef the unit around

  Source
    Copyright (c) 2025
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    ~2008: Creation date unknown, original local SVN repository lost
    2014-07-05: Uploaded to SourceForge/Package "Shared"
    2024-01-22: Migrated to Github.  Added to refactored package "IM_units"
    2025-11-29: Added this header.

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
  Classes, SysUtils, Inifiles, Menus;

Type

  { TMRU }

  TMRU = Class(TObject)
  Private
    FFiles: Boolean;
    FItems: TStringList;
    FMax: Integer;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure Clear;
    Procedure Validate;

    Function Value(iIndex: Integer): String;
    Function Filename(iIndex: Integer): String;

    Function Add(sItem: String): Integer;
    Procedure Populate(oMenuItem: TMenuItem; TheOnClick: TNotifyEvent);
    Function Count: Integer;

    Procedure Load(oInifile: TInifile; sSection, sIdent: String);
    Procedure Save(oInifile: TInifile; sSection, sIdent: String);

    Property Max: Integer read FMax write FMax;
    Property Files: Boolean read FFiles write FFiles;
  End;


Implementation

Uses
  FileUtil;

{ TMRU }

Constructor TMRU.Create;
Begin
  FItems := TStringList.Create;
  FItems.Duplicates := dupError;

  FFiles := False;
  FMax := 10;
End;

Destructor TMRU.Destroy;
Begin
  FItems.Free;

  Inherited Destroy;
End;

Procedure TMRU.Clear;
Begin
  FItems.Clear;
End;

Procedure TMRU.Validate;
Var
  i: Integer;
Begin
  If FFiles Then
    For i := FItems.Count - 1 Downto 0 Do
      If Not FileExists(FItems[i]) Then
        FItems.Delete(i);
End;

Function TMRU.Value(iIndex: Integer): String;
Begin
  If (iIndex >= 0) And (iIndex < FItems.Count) Then
    Result := FItems[iIndex]
  Else
    Result := '';
End;

Function TMRU.Filename(iIndex: Integer): String;
Begin
  If FFiles Then
    Result := ExtractFileName(Value(iIndex))
  Else
    Result := Value(iIndex);
End;

Function TMRU.Add(sItem: String): Integer;
Begin
  Result := FItems.IndexOf(sItem);

  If Result = -1 Then
  Begin
    Result := 0;

    FItems.Insert(0, sItem);

    While FItems.Count > FMax Do
      FItems.Delete(FItems.Count - 1);
  End
  Else If Result <> 0 Then
  Begin
    // Rearrange the MRU so our file is now on top...
    FItems.Delete(Result);

    Result := 0;

    FItems.Insert(0, sItem);
  End;
End;

Procedure TMRU.Populate(oMenuItem: TMenuItem; TheOnClick: TNotifyEvent);
Var
  i: Integer;
  oSubMenuItem: TMenuItem;
Begin
  oMenuItem.Clear;
  Validate;
  oMenuItem.Enabled := (FItems.Count > 0);

  For i := 0 To FItems.Count - 1 Do
  Begin
    oSubMenuItem := TMenuItem.Create(nil);
    oSubMenuItem.Caption := Filename(i);
    oSubMenuItem.Tag := i;
    oSubMenuItem.OnClick := TheOnClick;

    oMenuItem.Add(oSubMenuItem);
  End;
End;

Function TMRU.Count: Integer;
Begin
  Result := FItems.Count;
End;

Procedure TMRU.Load(oInifile: TInifile; sSection, sIdent: String);
Var
  i, iCount: Integer;
Begin
  Clear;

  iCount := oInifile.ReadInteger(sSection, Format('%s Count', [sIdent]), 0);

  For i := 0 To iCount - 1 Do
    FItems.Add(oInifile.ReadString(sSection, Format('%s %d', [sIdent, i]), ''));
End;

Procedure TMRU.Save(oInifile: TInifile; sSection, sIdent: String);
Var
  i: Integer;
Begin
  oInifile.WriteInteger(sSection, Format('%s Count', [sIdent]), FItems.Count);

  For i := 0 To FItems.Count - 1 Do
    oInifile.WriteString(sSection, Format('%s %d', [sIdent, i]), FItems[i]);
End;

End.
