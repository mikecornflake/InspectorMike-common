Unit FormMain;

{-------------------------------------------------------------------------------
  Package   : IM_application
  Unit      : FormMain.pas
  Description
    Base main form exposing common options and persistence hooks.

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
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, Menus, Inifiles;

Type
  TFormOption = (olMultilineGrid);
  TFormOptions = Set Of TFormOption;

  { TOptions }

  TOptions = Class
  Private
    FMultilineGridDefaults: Boolean;
    Procedure SetMultilineGridDefaults(AValue: Boolean);
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Property MultilineGridDefaults: Boolean read FMultilineGridDefaults
      write SetMultilineGridDefaults;
  End;

  { TfrmMain }

  TfrmMain = Class(TForm)
    ilImages: TImageList;
    mnuMultilineGridsFalse: TMenuItem;
    mnuMultilineGrids: TMenuItem;
    mnuMultilineGridsTrue: TMenuItem;
    mnuOptions: TMenuItem;
    mnuAbout: TMenuItem;
    mnuHelp: TMenuItem;
    mnuMain: TMainMenu;
    pbMain: TProgressBar;
    sbMain: TStatusBar;
    Procedure FormActivate(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure mnuAboutClick(Sender: TObject);
    Procedure mnuMultilineGridsValueClick(Sender: TObject);
  Private
    FBusy: Integer;
    FProgress: Integer;
    Procedure DoLoadSettings;
    Procedure DoSaveGlobalSettings;
    Procedure DoSaveLocalSettings;
    Function GetBusy: Boolean;
    Function GetStatus: String;
  Protected
    FOptions: TOptions;
    FSettingsLoaded: Boolean;
    FIndent: String;
    FAlwaysSaveSettings: Boolean;

    Procedure ApplicationClosing(Var CanClose: Boolean); Virtual;
    Procedure SetBusy(Const AValue: Boolean); Virtual;
    Procedure SetProgress(AValue: Integer); Virtual;
    Procedure SetStatus(AValue: String); Virtual;

    Procedure UpdateOptions(FFormOption: TFormOptions); Virtual;

    Function SettingsFileLocal: String;
    Function SettingsFileGlobal: String;

    Procedure RefreshUI; Virtual;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure LoadGlobalSettings(oInifile: TIniFile); Virtual;
    Procedure SaveGlobalSettings(oInifile: TIniFile); Virtual;

    Procedure LoadLocalSettings(oInifile: TIniFile); Virtual;
    Procedure SaveLocalSettings(oInifile: TIniFile); Virtual;

    Property Options: TOptions read FOptions;

    Property Progress: Integer read FProgress write SetProgress;
    Property Status: String read GetStatus write SetStatus;

    Property Busy: Boolean read GetBusy write SetBusy;
  End;

Function MainForm: TfrmMain;

Implementation

Uses
  Math, FormAbout, FileSupport;

  {$R *.lfm}

Function MainForm: TfrmMain;
Begin
  If Assigned(Application.MainForm) And (Application.MainForm Is TfrmMain) Then
    Result := TfrmMain(Application.MainForm)
  Else
    Raise Exception.Create(
      'FormMainBase.MainForm is only for apps where MainForm is a TfrmMain descendent');
End;

{ TOptions }

Procedure TOptions.SetMultilineGridDefaults(AValue: Boolean);
Begin
  If FMultilineGridDefaults = AValue Then Exit;
  FMultilineGridDefaults := AValue;
End;

Constructor TOptions.Create;
Begin
  Inherited Create;

  FMultilineGridDefaults := True;
End;

Destructor TOptions.Destroy;
Begin
  Inherited Destroy;
End;

Constructor TfrmMain.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);

  FSettingsLoaded := False;
  FBusy := 0;
  FIndent := '  ';

  Caption := Application.Title;

  FAlwaysSaveSettings := False;

  FOptions := TOptions.Create;
End;

Destructor TfrmMain.Destroy;
Begin
  FreeAndNil(FOptions);

  Inherited Destroy;
End;

Procedure TfrmMain.FormActivate(Sender: TObject);
Begin
  If Not FSettingsLoaded Then
    DoLoadSettings;
End;

Procedure TfrmMain.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  ApplicationClosing(CanClose);

  Application.CaseSensitiveOptions := False;

  // Only allow power users to save global settings
  If FAlwaysSaveSettings Or (Application.HasOption('writesettings') Or
    Application.HasOption('configure')) Then
    DoSaveGlobalSettings;

  // Always save settings pertinent to the local user
  DoSaveLocalSettings;
End;

Procedure TfrmMain.mnuAboutClick(Sender: TObject);
Begin
  ShowAbout;
End;


Procedure TfrmMain.mnuMultilineGridsValueClick(Sender: TObject);
Begin
  FOptions.MultilineGridDefaults := Not FOptions.MultilineGridDefaults;

  UpdateOptions([olMultilineGrid]);

  RefreshUI;
End;

Procedure TfrmMain.DoLoadSettings;
Var
  oInifile: TIniFile;
Begin
  Busy := True;
  Try
    oInifile := TIniFile.Create(SettingsFileLocal);
    Try
      LoadLocalSettings(oInifile);
    Finally
      oInifile.Free;
    End;
  Finally
    Busy := False;
  End;

  oInifile := TIniFile.Create(SettingsFileGlobal);
  Try
    LoadGlobalSettings(oInifile);
  Finally
    oInifile.Free;
  End;

  FSettingsLoaded := True;

  RefreshUI;
End;

Procedure TfrmMain.DoSaveGlobalSettings;
Var
  sFilename: String;

  Procedure InnerSaveSettings;
  Var
    oInifile: TIniFile;
  Begin
    Busy := True;
    Try
      // Oops - DeleteFile raises an exception if file doesn't exist...
      If FileExists(sFilename) Then
      Begin
        // Make a backup of the settings
        DeleteFile(sFilename + '.bak');
        CopyFile(sFilename, sFilename + '.bak');
      End;

      oInifile := TIniFile.Create(sFilename);

      // Do all the SaveSettings work in memory
      oInifile.CacheUpdates := True;
      Try
        SaveGlobalSettings(oIniFile);

        // And flush the settings out in one go
        // This works around an AVG issue whereby
        // it locks the ini file during repeated writes
        // causing a CreateError exception to be thrown
        oInifile.UpdateFile;
      Finally
        oInifile.Free;
      End;
    Finally
      Busy := False;
    End;
  End;

Begin
  sFilename := SettingsFileGlobal;

  If Not FileExists(sFilename) Then
    InnerSaveSettings
  Else If Not FileIsReadOnly(sFilename) Then
    InnerSaveSettings;
End;

Procedure TfrmMain.DoSaveLocalSettings;
Var
  oInifile: TIniFile;
Begin
  oInifile := TIniFile.Create(SettingsFileLocal);

  // Do all the SaveSettings work in memory
  oInifile.CacheUpdates := True;
  Try
    SaveLocalSettings(oIniFile);

    // And flush the settings out in one go
    // This works around an AVG issue whereby
    // it locks the ini file during repeated writes
    // causing a CreateError exception to be thrown
    oInifile.UpdateFile;
  Finally
    oInifile.Free;
  End;

End;

Procedure TfrmMain.LoadGlobalSettings(oInifile: TIniFile);
Begin

End;

Procedure TfrmMain.SaveGlobalSettings(oInifile: TIniFile);
Begin

End;

Procedure TfrmMain.UpdateOptions(FFormOption: TFormOptions);
Begin

End;

Procedure TfrmMain.LoadLocalSettings(oInifile: TIniFile);
Var
  iLeft, iWidth, iTop, iHeight: Integer;
Begin
  // Default, load Window Settings
  iLeft := oInifile.ReadInteger(Name, 'Left', Application.MainForm.Left);
  iTop := oInifile.ReadInteger(Name, 'Top', Application.MainForm.Top);
  iWidth := oInifile.ReadInteger(Name, 'Width', Application.MainForm.Width);
  iHeight := oInifile.ReadInteger(Name, 'Height', Application.MainForm.Height);

  SetBounds(iLeft, iTop, iWidth, iHeight);
  MakeFullyVisible;

  If oInifile.ReadBool(Name, 'Maximised', False) Then
    Application.MainForm.WindowState := wsMaximized
  Else
    Application.MainForm.WindowState := wsNormal;
End;

Procedure TfrmMain.SaveLocalSettings(oInifile: TIniFile);
Begin
  oInifile.WriteBool(Name, 'Maximised', Application.MainForm.WindowState = wsMaximized);

  If WindowState <> wsMaximized Then
  Begin
    oInifile.WriteInteger(Name, 'Left', Application.MainForm.Left);
    oInifile.WriteInteger(Name, 'Top', Application.MainForm.Top);
    oInifile.WriteInteger(Name, 'Width', Application.MainForm.Width);
    oInifile.WriteInteger(Name, 'Height', Application.MainForm.Height);
  End;
End;

Procedure TfrmMain.SetBusy(Const AValue: Boolean);
Var
  i: Integer;
Begin
  If AValue Then
    Inc(FBusy)
  Else
    FBusy := Max(FBusy - 1, 0);

  FIndent := '  ';
  For i := 1 To FBusy Do
    FIndent := FIndent + ' ';

  If (FBusy > 0) Then
  Begin
    // Calling Multiple Cursor := seems to stop the nice animated cursor effect
    If (Cursor <> crHourGlass) Then
    Begin
      Cursor := crHourglass;
      Screen.Cursor := crHourglass;
    End;
  End
  Else
  Begin
    Cursor := crDefault;
    Screen.Cursor := crDefault;
  End;
End;

Procedure TfrmMain.SetProgress(AValue: Integer);
Begin
  If FProgress = AValue Then
    exit;
  FProgress := AValue;

  pbMain.Visible := (AValue > 0);
  pbMain.Position := AValue;
End;

Procedure TfrmMain.SetStatus(AValue: String);
Begin
  sbMain.SimpleText := AValue;
  sbMain.Update;
End;

Function TfrmMain.SettingsFileLocal: String;
Begin
  Result := IncludeSlash(GetAppConfigDir(False)) + 'Settings.ini';
End;

Function TfrmMain.SettingsFileGlobal: String;
Begin
  Result := ChangeFileExt(Application.Exename, '.ini');
End;

Procedure TfrmMain.RefreshUI;
Begin
  mnuMultilineGridsTrue.Checked := FOptions.MultilineGridDefaults;
  mnuMultilineGridsFalse.Checked := Not FOptions.MultilineGridDefaults;
End;

Function TfrmMain.GetBusy: Boolean;
Begin
  Result := FBusy > 0;
End;

Function TfrmMain.GetStatus: String;
Begin
  Result := sbMain.SimpleText;
End;

Procedure TfrmMain.ApplicationClosing(Var CanClose: Boolean);
Begin

End;

Initialization

End.
