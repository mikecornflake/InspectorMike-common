Unit FrameBase;

{-------------------------------------------------------------------------------
  Package   : IM_forms
  Unit      : FrameBase.pas
  Description
    Base frame providing persistent settings support.

  Source
    Copyright (c) 2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    ~2008: Creation date unknown, original local SVN repository lost
    2014-07-05: Uploaded to SourceForge/Package "Shared"
    2024-01-22: Migrated to Github.  Refactored package to "IM_application"
    2025-11-29: Added LGPL-3.0-or-later license header
    2026-06-19: Refactored into split InspectorMike package structure

  License
    This file is part of IM_forms.lpk.

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

  { TFrameBase }

  TFrameBase = Class(TFrame)
    Procedure FrameEnter(Sender: TObject);
    Procedure FrameExit(Sender: TObject);
  Protected
    FParentForm: TFormPersistent;
    FFullIdentKey: String;
    FOnActivateFrame: TNotifyEvent;
    FFrameActive: Boolean;

    Function GetFullIdentKey: String; Virtual;
    Function GetSettingsKey: String; Virtual;

    Procedure DoActivateFrame;
  Public
    Constructor Create(TheOwner: TComponent); Override;

    Procedure RefreshUI; Virtual;

    Procedure LoadSettings(oInifile: TIniFile); Virtual;
    Procedure SaveSettings(oInifile: TIniFile); Virtual;

    // Called on database open/close
    Procedure Open; Virtual;
    Procedure Close; Virtual;

    Property ParentForm: TFormPersistent Read FParentForm Write FParentForm;
    Property SettingsKey: String Read GetSettingsKey;
    Property FullIdentKey: String Read GetFullIdentKey;

    // Implementation here is via OnEnter/OnExit.
    // If descendant frame is unable to receive focus
    // DoActivateFrame should be called in appropriate location.
    // Required for i.e. TFrameSyncedVideo.
    // See TFrameVideoLibmpv for a working example
    Property OnActivateFrame: TNotifyEvent Read FOnActivateFrame Write FOnActivateFrame;
  End;

Implementation

{$R *.lfm}

{ TFrameBase }

Function TFrameBase.GetSettingsKey: String;
Begin
  If Assigned(FParentForm) Then
    Result := FParentForm.SettingsKey
  Else
    Result := Name;
End;

Procedure TFrameBase.DoActivateFrame;
Begin
  If Assigned(FOnActivateFrame) Then
    FOnActivateFrame(Self);
End;

Procedure TFrameBase.FrameEnter(Sender: TObject);
Begin
  If Not FFrameActive Then
  Begin
    FFrameActive := True;
    DoActivateFrame;
  End;
End;

Procedure TFrameBase.FrameExit(Sender: TObject);
Begin
  FFrameActive := False;
End;

Function TFrameBase.GetFullIdentKey: String;

  Function ParentFullIdentKey(oComponent: TComponent): String;
  Var
    oParent: TComponent;
    s: String;
  Begin
    oParent := oComponent.Owner;

    If (oParent = nil) Or (oParent Is TFormPersistent) Then
      Result := ''
    Else If oParent Is TFrameBase Then
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

Constructor TFrameBase.Create(TheOwner: TComponent);

  Function FindParentForm(oComponent: TComponent): TFormPersistent;
  Begin
    If Assigned(oComponent) Then
    Begin
      If oComponent Is TFormPersistent Then
        Result := TFormPersistent(oComponent)
      Else
        Result := FindParentForm(oComponent.Owner);
    End
    Else
      Result := nil;
  End;

Begin
  Inherited Create(TheOwner);

  FParentForm := FindParentForm(TheOwner);
  FFrameActive := False;
End;

Procedure TFrameBase.RefreshUI;
Begin

End;

Procedure TFrameBase.LoadSettings(oInifile: TIniFile);
Begin

End;

Procedure TFrameBase.SaveSettings(oInifile: TIniFile);
Begin

End;

Procedure TFrameBase.Open;
Begin

End;

Procedure TFrameBase.Close;
Begin

End;

End.
