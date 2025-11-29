Unit Settings;

{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : Settings.pas
  Description
    Framework to persist Application Settings
    This works, but was clumsy to use.
    Was superceded by the Settings persistence layer in IM_application package
    File remains as it is used in a few test harnesses.

  Source
    Copyright (c) 2025
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    ~2008: Creation date unknown, original local SVN repository lost
    2014-07-05: Uploaded to SourceForge/Package "Shared"
    2024-01-22: Migrated to Github.  Refactored package to "IM_units"
    2025-11-29: Added this header

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
  Classes, SysUtils, XMLConf, Forms;

Type

  { TSetting }

  TSetting = Class(TObject)
  Private
    FSimpleSettings: TStringList;
    FOwner: TSetting;
    FName: String;
    FSettings: TList;

    Function GetPath: String;
    Function GetOwner: TSetting;
    Function GetName: String;
    Function GetSetting(iIndex: Integer): TSetting;
    Procedure SetName(Const AValue: String);
    Procedure SetOwner(Const AValue: TSetting);
    Function GetBoolean(sName: String): Boolean;
    Function GetInteger(sName: String): Integer;
    Function GetString(sName: String): String;
    Procedure SetBoolean(sName: String; Const AValue: Boolean);
    Procedure SetInteger(sName: String; Const AValue: Integer);
    Procedure SetString(sName: String; Const AValue: String);
  Public
    Constructor Create(AOwner: TSetting); Virtual;
    Destructor Destroy; Override;

    Procedure Load(oXMLConfig: TXMLConfig); Virtual;
    Procedure Save(oXMLConfig: TXMLConfig); Virtual;

    Function Root: TSetting;
    Function Add: TSetting;
    Function Count: Integer;

    Function AsStringDef(sName: String; Const sDefault: String): String; Overload;
    Function AsIntegerDef(sName: String; iDefault: Integer): Integer;
    Function AsBooleanDef(sName: String; bDefault: Boolean): Boolean;

    Property Path: String read GetPath;
    Property Owner: TSetting read GetOwner write SetOwner;
    Property Name: String read GetName write SetName;
    Property Setting[iIndex: Integer]: TSetting read GetSetting;

    Property AsString[sName: String]: String read GetString write SetString;
    Property AsInteger[sName: String]: Integer read GetInteger write SetInteger;
    Property AsBoolean[sName: String]: Boolean read GetBoolean write SetBoolean;
  End;

  { TSettings }

  TSettings = Class(TObject)
  Private
    FRoot: TSetting;
    FFilename: String;
    FXMLConfig: TXMLConfig;

    Function GetFilename: String;
    Procedure SetFilename(Const Avalue: String);
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure Load;
    Procedure Save;

    Procedure LoadForm(oForm: TForm; sPath: String);
    Procedure SaveForm(oForm: TForm; sPath: String);

    Property Filename: String read GetFilename write SetFilename;

    Property Root: TSetting read FRoot;
  End;

Const
  S_NOT_DEFINED = 'SETTING_NOT_DEFINED';

Function G_Settings: TSettings;
Procedure LoadStringList(oXMLConfig: TXMLConfig; oList: TStrings; Const sName: String);
Procedure SaveStringList(oXMLConfig: TXMLConfig; oList: TStrings; Const sName: String);
Function IncludeSlash(sInput: String): String;


Implementation

Var
  FSettings: TSettings;

Function G_Settings: TSettings;
Begin
  If Not Assigned(FSettings) Then
    FSettings := TSettings.Create;

  Result := FSettings;
End;

Procedure LoadStringList(oXMLConfig: TXMLConfig; oList: TStrings; Const sName: String);
Var
  i, Count: Integer;
  s: String;
Begin
  Count := oXMLConfig.GetValue(sName + 'Count', 0);
  oList.Clear;
  For i := 1 To Count Do
  Begin
    s := oXMLConfig.GetValue(sName + 'Item' + IntToStr(i) + '/Value', '');
    If s <> '' Then
      oList.Add(s);
  End;
End;

Procedure SaveStringList(oXMLConfig: TXMLConfig; oList: TStrings; Const sName: String);
Var
  i: Integer;
Begin
  oXMLConfig.SetDeleteValue(sName + 'Count', oList.Count, 0);
  For i := 0 To oList.Count - 1 Do
    oXMLConfig.SetDeleteValue(sName + 'Item' + IntToStr(i + 1) + '/Value', oList[i], '');
End;

Function IncludeSlash(sInput: String): String;
Begin
  If RightStr(sInput, 1) <> '/' Then
    Result := sInput + '/'
  Else
    Result := sInput;
End;

{ TSetting }

Function TSetting.GetName: String;
Begin
  Result := IncludeSlash(FName);
End;

Procedure TSetting.SetOwner(Const AValue: TSetting);
Begin
  FOwner := AValue;
End;

Function TSetting.GetPath: String;
Var
  oTemp: TSetting;
Begin
  Result := Name;

  oTemp := Owner;

  While Assigned(oTemp) Do
  Begin
    Result := oTemp.Name + Result;

    oTemp := oTemp.Owner;
  End;
End;

Function TSetting.GetOwner: TSetting;
Begin
  Result := FOwner;
End;

Function TSetting.GetSetting(iIndex: Integer): TSetting;
Begin
  If (iIndex <= 0) And (iIndex < FSettings.Count) Then
    Result := TSetting(FSettings[iIndex])
  Else
    Raise Exception.Create('Index out of bounds');
End;

Procedure TSetting.SetName(Const AValue: String);
Begin
  FName := StringReplace(AValue, ' ', '_', [rfReplaceAll]);
End;

Constructor TSetting.Create(AOwner: TSetting);
Begin
  FOwner := AOwner;

  FSettings := TList.Create;

  FSimpleSettings := TStringList.Create;
End;

Destructor TSetting.Destroy;
Begin
  While FSettings.Count > 0 Do
  Begin
    TSetting(FSettings.Last).Free;
    FSettings.Delete(FSettings.Count - 1);
  End;

  FSettings.Free;

  FSimpleSettings.Free;
  FSimpleSettings := nil;

  Inherited Destroy;
End;

Procedure TSetting.Load(oXMLConfig: TXMLConfig);
Var
  i: Integer;
Begin
  LoadStringList(oXMLConfig, FSimpleSettings, Path + 'Simple/');

  For i := 0 To FSettings.Count - 1 Do
    Setting[i].Load(oXMLCOnfig);
End;

Procedure TSetting.Save(oXMLConfig: TXMLConfig);
Var
  i: Integer;
Begin
  For i := 0 To FSettings.Count - 1 Do
    Setting[i].Save(oXMLCOnfig);

  SaveStringList(oXMLConfig, FSimpleSettings, Path + 'Simple/');
End;

Function TSetting.Root: TSetting;
Var
  oTemp: TSetting;
Begin
  oTemp := Owner;

  While Assigned(oTemp) And Assigned(oTemp.Owner) Do
    oTemp := oTemp.Owner;

  Result := oTemp;
End;

Function TSetting.Add: TSetting;
Begin
  Result := TSetting.Create(Self);
  FSettings.Add(Result);
End;

Function TSetting.Count: Integer;
Begin
  Result := FSettings.Count;
End;

Function TSetting.AsStringDef(sName: String; Const sDefault: String): String;
Begin
  If FSimpleSettings.IndexOfName(sName) = -1 Then
  Begin
    Result := sDefault;
    AsString[sName] := sDefault;
  End
  Else
    Result := AsString[sName];
End;

Function TSetting.AsIntegerDef(sName: String; iDefault: Integer): Integer;
Begin
  If FSimpleSettings.IndexOfName(sName) = -1 Then
  Begin
    Result := iDefault;
    AsInteger[sName] := iDefault;
  End
  Else
    Result := StrToIntDef(AsString[sName], iDefault);
End;

Function TSetting.AsBooleanDef(sName: String; bDefault: Boolean): Boolean;
Begin
  If FSimpleSettings.IndexOfName(sName) = -1 Then
  Begin
    Result := bDefault;
    AsBoolean[sName] := bDefault;
  End
  Else
    Result := AsBoolean[sName];
End;

Function TSetting.GetBoolean(sName: String): Boolean;
Begin
  Result := AsString[sName] = 'True';
End;

Function TSetting.GetInteger(sName: String): Integer;
Begin
  Result := StrToIntDef(AsString[sName], -1);
End;

Function TSetting.GetString(sName: String): String;
Begin
  Result := FSimpleSettings.Values[sName];
End;

Procedure TSetting.SetInteger(sName: String; Const AValue: Integer);
Begin
  AsString[sName] := IntToStr(AValue);
End;

Procedure TSetting.SetString(sName: String; Const AValue: String);
Begin
  FSimpleSettings.Values[sName] := AValue;
End;

Procedure TSetting.SetBoolean(sName: String; Const AValue: Boolean);
Begin
  If AValue Then
    AsString[sName] := 'True'
  Else
    AsString[sName] := 'False';
End;


{ TSettings }

Constructor TSettings.Create;
Begin
  Inherited Create;

  FFilename := ChangeFileExt(ParamStr(0), '.Settings');

  FRoot := TSetting.Create(nil);
  FRoot.Name := 'Settings/';

  FXMLConfig := TXMLConfig.Create(nil);
  FXMLConfig.RootName := StringReplace(Application.Title, ' ', '_', [rfReplaceAll]);
  FXMLConfig.Filename := FFilename;
End;

Destructor TSettings.Destroy;
Begin
  FXMLConfig.Free;

  FRoot.Free;
  FRoot := nil;

  Inherited Destroy;
End;

Function TSettings.GetFilename: String;
Begin
  Result := FFilename;
End;

Procedure TSettings.SetFilename(Const Avalue: String);
Begin
  Save;

  FFilename := AValue;

  FXMLConfig.Filename := FFilename;

  Load;
End;

Procedure TSettings.Load;
Begin
  Root.Load(FXMLConfig);
End;

Procedure TSettings.Save;

  Procedure DoSave;
  Begin
    Root.Save(FXMLConfig);
    FXMLConfig.Flush;
  End;

Begin
  // I'm getting old, there's a nice boolean solution to this somewhere...
  Try
    If Not FileExists(FXMLConfig.Filename) Then
      DoSave
    Else If Not FileIsReadOnly(FXMLConfig.Filename) Then
      DoSave;
  Except
  End;
End;

Procedure TSettings.LoadForm(oForm: TForm; sPath: String);
Var
  iTop, iLeft, iWidth, iHeight: Integer;
Begin
  If Assigned(oForm) Then
  Begin
    sPath := IncludeSlash(sPath);

    iTop := FXMLConfig.GetValue(sPath + 'Top', oForm.Top);
    iLeft := FXMLConfig.GetValue(sPath + 'Left', oForm.Left);
    iWidth := FXMLConfig.GetValue(sPath + 'Width', oForm.Width);
    iHeight := FXMLConfig.GetValue(sPath + 'Height', oForm.Height);
    oForm.SetBounds(iLeft, iTop, iWidth, iHeight);
    oForm.MakeFullyVisible;

    If FXMLConfig.GetValue(sPath + 'Maximized', oForm.WindowState = wsMaximized) Then
      oForm.WindowState := wsMaximized
    Else
      oForm.WindowState := wsNormal;
  End;
End;

Procedure TSettings.SaveForm(oForm: TForm; sPath: String);
Begin
  If Assigned(oForm) Then
  Begin
    sPath := IncludeSlash(sPath);

    If oForm.WindowState <> wsMaximized Then
    Begin
      FXMLConfig.SetValue(sPath + 'Top', oForm.Top);
      FXMLConfig.SetValue(sPath + 'Left', oForm.Left);
      FXMLConfig.SetValue(sPath + 'Width', oForm.Width);
      FXMLConfig.SetValue(sPath + 'Height', oForm.Height);
    End;

    FXMLConfig.SetValue(sPath + 'Maximized', oForm.WindowState = wsMaximized);
  End;
End;

Initialization
  FSettings := nil;

Finalization
  If Assigned(FSettings) Then
  Begin
    FSettings.Free;
    FSettings := nil;
  End;
End.
