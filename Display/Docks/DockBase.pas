Unit DockBase;

{-------------------------------------------------------------------------------
  Package   : IM_application
  Unit      : DockBase.pas
  Description
    Base dockable form with category, imagery, and persistence helpers.

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
  Classes, ComCtrls, Controls, Dialogs, FileUtil, Forms, Graphics,
  IniFiles, LResources, SysUtils, Menus, FormPersistent;

Type

  { TdckBase }

  TdckBase = Class(TfrmPersistent)
  Private
    FImageIndex: Integer;
  Protected
    FCategory: Integer;
    FTabSheet: TTabSheet;
    FLastMessage: String;
    FFilterChanged: Boolean;
    Procedure SetStatus(sStatus: String);
  Public
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure Open; Virtual;
    Procedure Close; Virtual;

    Procedure Enter; Virtual;
    Procedure Leave; Virtual;

    Procedure UpdateData; Virtual;

    Procedure LoadSettings(oInifile: TIniFile); Override;
    Procedure SaveSettings(oInifile: TIniFile); Override;

    Function Active: Boolean;

    Property Tabsheet: TTabSheet read FTabSheet write FTabSheet;
    Property LastStatus: String read FLastMessage;

    Property Category: Integer read FCategory write FCategory;
    Property ImageIndex: Integer read FImageIndex write FImageIndex;

    // used to track whether external filters were changed while this
    // dock didn't have focus.
    Property FilterChanged: Boolean read FFilterChanged write FFilterChanged;
  End;


Implementation

Uses
  FormMain, FormMultiDock, FrameBase;

{ TdckBase }

{$R *.lfm}

Procedure TdckBase.SetStatus(sStatus: String);
Begin
  If sStatus <> '' Then
    sStatus := Tabsheet.Caption + ': ' + sStatus;

  FLastMessage := sStatus;
  MainForm.Status := sStatus;
End;

Constructor TdckBase.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  FTabSheet := nil;
  FLastMessage := '';

  FCategory := 4;
  FFilterChanged := False;
End;

Destructor TdckBase.Destroy;
Begin

  Inherited Destroy;
End;

Procedure TdckBase.Open;

  Procedure OpenChildren(AControl: TWinControl);
  Var
    i: Integer;
    oWinControl: TWinControl;
    oControl: TControl;
  Begin
    For i := 0 To AControl.ControlCount - 1 Do
    Begin
      oControl := AControl.Controls[i];
      If oControl Is TWinControl Then
      Begin
        oWinControl := TWinControl(oControl);

        If oWinControl Is TfmeBase Then
          TfmeBase(oWinControl).Open;

        If Assigned(oWinControl) Then
          OpenChildren(oWinControl);
      End;
    End;
  End;

Begin
  MainForm.Status := 'Opening ' + FTabSheet.Caption;
  OpenChildren(Self);
End;

Procedure TdckBase.Close;

  Procedure CloseChildren(AControl: TWinControl);
  Var
    i: Integer;
    oWinControl: TWinControl;
    oControl: TControl;
  Begin
    For i := 0 To AControl.ControlCount - 1 Do
    Begin
      oControl := AControl.Controls[i];

      If oControl Is TWinControl Then
      Begin
        oWinControl := TWinControl(oControl);

        If oWinControl Is TfmeBase Then
          TfmeBase(oWinControl).Close;

        If Assigned(oWinControl) Then
          CloseChildren(oWinControl);
      End;
    End;
  End;

Begin
  MainForm.Status := 'Closing ' + FTabSheet.Caption;
  CloseChildren(Self);
End;

Procedure TdckBase.Enter;
Begin
  MainForm.Status := 'Entering ' + FTabSheet.Caption;

  If FLastMessage <> '' Then
    MainForm.Status := FLastMessage;
End;

Procedure TdckBase.Leave;
Begin
  MainForm.Status := 'Leaving ' + FTabSheet.Caption;
End;

Procedure TdckBase.UpdateData;
Begin
  MainForm.Status := 'UpdateData for ' + FTabSheet.Caption;
End;

Procedure TdckBase.LoadSettings(oInifile: TIniFile);
Begin
  Inherited LoadSettings(oInifile);

  FCategory := oInifile.ReadInteger(SettingsKey, 'Category', 0);
  FImageIndex := oInifile.ReadInteger(SettingsKey, 'Image Index', 4);
End;

Procedure TdckBase.SaveSettings(oInifile: TIniFile);
Begin
  Inherited SaveSettings(oInifile);

  oInifile.WriteInteger(SettingsKey, 'Category', FCategory);
  oInifile.WriteInteger(SettingsKey, 'Image Index', FImageIndex);
End;

Function TdckBase.Active: Boolean;
Var
  oMultiDockForm: TfrmMultiDock;
Begin
  Result := False;

  If Application.MainForm Is TfrmMultiDock Then
  Begin
    oMultiDockForm := TfrmMultiDock(Application.MainForm);

    Result := (oMultiDockForm.ActivePageControl = FTabSheet.PageControl) And
      (FTabSheet = FTabSheet.PageControl.ActivePage);
  End;
End;

Initialization

End.
