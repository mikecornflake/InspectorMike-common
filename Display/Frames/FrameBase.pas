Unit FrameBase;

{-------------------------------------------------------------------------------
  Package   : IM_application
  Unit      : FrameBase.pas
  Description
    Base frame providing persistent settings support.

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
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Inifiles,
  FormPersistent;

Type

  { TfmeBase }

  TfmeBase = Class(TFrame)
  Protected
    FParentForm: TfrmPersistent;
    FFullIdentKey: String;
    Function GetFullIdentKey: String; Virtual;
    Function GetSettingsKey: String; Virtual;
  Public
    Constructor Create(TheOwner: TComponent); Override;

    Procedure RefreshUI; Virtual;

    Procedure LoadSettings(oInifile: TIniFile); Virtual;
    Procedure SaveSettings(oInifile: TIniFile); Virtual;

    // Called on database open/close
    Procedure Open; Virtual;
    Procedure Close; Virtual;

    Property ParentForm: TfrmPersistent read FParentForm write FParentForm;
    Property SettingsKey: String read GetSettingsKey;
    Property FullIdentKey: String read GetFullIdentKey;
  End;

Implementation

{$R *.lfm}

{ TfmeBase }

Function TfmeBase.GetSettingsKey: String;
Begin
  If Assigned(FParentForm) Then
    Result := FParentForm.SettingsKey
  Else
    Result := Name;
End;

Function TfmeBase.GetFullIdentKey: String;

  Function ParentFullIdentKey(oComponent: TComponent): String;
  Var
    oParent: TComponent;
    s: String;
  Begin
    oParent := oComponent.Owner;

    If (oParent = nil) Or (oParent Is TfrmPersistent) Then
      Result := ''
    Else If oParent Is TfmeBase Then
    Begin
      s := ParentFullIdentKey(oParent);
      If (s = '') Then
        Result := oParent.Name
      Else
        Result := Format('%s.%s', [s, oParent.Name]);
    End
    Else
      Result := ParentFullIdentKey(oParent);
  End;

Var
  s: String;
Begin
  If FFullIdentKey = '' Then
  Begin
    s := ParentFullIdentKey(Self);

    If (s = '') Then
      FFullIdentKey := Name
    Else
      FFullIdentKey := Format('%s.%s', [s, Name]);
  End;

  Result := FFullIdentKey;
End;

Constructor TfmeBase.Create(TheOwner: TComponent);

  Function FindParentForm(oComponent: TComponent): TfrmPersistent;
  Begin
    If Assigned(oComponent) Then
    Begin
      If oComponent Is TfrmPersistent Then
        Result := TfrmPersistent(oComponent)
      Else
        Result := FindParentForm(oComponent.Owner);
    End
    Else
      Result := nil;
  End;

Begin
  Inherited Create(TheOwner);

  FParentForm := FindParentForm(TheOwner);
End;

Procedure TfmeBase.RefreshUI;
Begin

End;

Procedure TfmeBase.LoadSettings(oInifile: TIniFile);
Begin

End;

Procedure TfmeBase.SaveSettings(oInifile: TIniFile);
Begin

End;

Procedure TfmeBase.Open;
Begin

End;

Procedure TfmeBase.Close;
Begin

End;

Initialization

End.
