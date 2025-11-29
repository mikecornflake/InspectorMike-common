Unit VersionSupport;

{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : VersionSupport.pas
  Description
    Support for extracting Build information for displaying in ie About forms

    Building on the excellent vinfo.pas supplied by Paul Ishenin and available elsewhere on the Lazarus
    Forums
      - I hid the TVersionInfo class from the end user to simplify their (mine) number of required Uses...
      - Added defensive code to TVersionInfo if no build info is compiled into the exe
      - Deduced GetResourceStrings - works under Linux 64/GTK2 with Lazarus 0.9.30, but fails under
        Win XP 32bit/Lazarus 0.9.29 - suspecting my install as the lazresexplorer example also fails
        for me under Lazarus 0.9.29, but works with Lazarus 0.9.30

    Trawled through IDE source code, FPC source code and Lazarus supplied example program lasresexplorer
    to find the other defines and lookups...

    End user only needs to use VersionSupport - no other units necessary for their project.

    Jedi CodeFormatter seems to fail on the {$I %VARIABLE%} references, so sticking them all in here
    means end user code can be neatly formatted using Jedi CodeFormatter

    Other interesting includes I picked up in my travels are...
    //  {$I %HOME%} = User Home Directory
    //  {$I %FILE%} = Current pas file
    //  {$I %LINE%} = current line number

    Mike Thompson - mike.cornflake@gmail.com
    July 24 2011

  Source
    Copyright (c) 2025
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    2011-07-04: Creation. Original local SVN repository lost, but code shared on Lazarus Forum
    2014-07-05: Uploaded to SourceForge/Package "Shared"
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

{$mode objfpc}

Interface

(*
  Building on the excellent vinfo.pas supplied by Paul Ishenin and available elsewhere on the Lazarus
  Forums
    - I hid the TVersionInfo class from the end user to simplify their (mine) number of required Uses...
    - Added defensive code to TVersionInfo if no build info is compiled into the exe
    - Deduced GetResourceStrings - works under Linux 64/GTK2 with Lazarus 0.9.30, but fails under
      Win XP 32bit/Lazarus 0.9.29 - suspecting my install as the lazresexplorer example also fails
      for me under Lazarus 0.9.29, but works with Lazarus 0.9.30

  Trawled through IDE source code, FPC source code and Lazarus supplied example program lasresexplorer
  to find the other defines and lookups...

  End user only needs to use VersionSupport - no other units necessary for their project.

  Jedi CodeFormatter seems to fail on the {$I %VARIABLE%} references, so sticking them all in here
  means end user code can be neatly formatted using Jedi CodeFormatter

  Other interesting includes I picked up in my travels are...
  //  {$I %HOME%} = User Home Directory
  //  {$I %FILE%} = Current pas file
  //  {$I %LINE%} = current line number

  Mike Thompson - mike.cornflake@gmail.com
  July 24 2011
*)

Uses
  Classes, SysUtils;

// Surfacing general defines and lookups
Function GetCompiledDate: String;
Function GetCompilerInfo: String;
Function GetTargetInfo: String;
Function GetOS: String;
Function GetCPU: String;
Function GetLCLVersion: String;
Function GetWidgetSet: String;

// Exposing resource and version info compiled into exe
Function GetResourceStrings(oStringList: TStringList): Boolean;
Function GetFileVersion: String;
Function GetProductVersion: String;

Implementation

Uses
  resource, versiontypes, versionresource, LCLVersion, InterfaceBase, LCLPlatformDef;

Function GetWidgetSet: String;
Begin
  Result := LCLPlatformDisplayNames[WidgetSet.LCLPlatform];
End;

Function GetCompilerInfo: String;
Begin
  Result := 'FPC ' + {$I %FPCVERSION%};
End;

Function GetTargetInfo: String;
Begin
  Result := {$I %FPCTARGETCPU%} + ' - ' + {$I %FPCTARGETOS%};
End;

Function GetOS: String;
Begin
  Result := {$I %FPCTARGETOS%};
End;

Function GetCPU: String;
Begin
  Result := {$I %FPCTARGETCPU%};
End;

Function GetLCLVersion: String;
Begin
  Result := 'LCL ' + lcl_version;
End;

Function GetCompiledDate: String;
Var
  sDate, sTime: String;
Begin
  sDate := {$I %DATE%};
  sTime := {$I %TIME%};

  Result := sDate + ' at ' + sTime;
End;

{ Routines to expose TVersionInfo data }

Type
  TVersionInfo = Class
  Private
    FBuildInfoAvailable: Boolean;
    FVersResource: TVersionResource;
    Function GetFixedInfo: TVersionFixedInfo;
    Function GetStringFileInfo: TVersionStringFileInfo;
    Function GetVarFileInfo: TVersionVarFileInfo;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Procedure Load(Instance: THandle);

    Property BuildInfoAvailable: Boolean Read FBuildInfoAvailable;

    Property FixedInfo: TVersionFixedInfo Read GetFixedInfo;
    Property StringFileInfo: TVersionStringFileInfo Read GetStringFileInfo;
    Property VarFileInfo: TVersionVarFileInfo Read GetVarFileInfo;
  End;

Var
  FInfo: TVersionInfo;

Procedure CreateInfo;
Begin
  If Not Assigned(FInfo) Then
  Begin
    FInfo := TVersionInfo.Create;
    FInfo.Load(HINSTANCE);
  End;
End;

Function GetResourceStrings(oStringList: TStringList): Boolean;
Var
  i, j: Integer;
  oTable: TVersionStringTable;
Begin
  CreateInfo;

  oStringList.Clear;
  Result := False;

  If FInfo.BuildInfoAvailable Then
  Begin
    Result := True;
    For i := 0 To FInfo.StringFileInfo.Count - 1 Do
    Begin
      oTable := FInfo.StringFileInfo.Items[i];

      For j := 0 To oTable.Count - 1 Do
        If Trim(oTable.ValuesByIndex[j]) <> '' Then
          oStringList.Values[oTable.Keys[j]] := oTable.ValuesByIndex[j];
    End;
  End;
End;

Function ProductVersionToString(PV: TFileProductVersion): String;
Begin
  Result := Format('%d.%d.%d.%d', [PV[0], PV[1], PV[2], PV[3]]);
End;

Function GetProductVersion: String;
Begin
  CreateInfo;

  If FInfo.BuildInfoAvailable Then
    Result := ProductVersionToString(FInfo.FixedInfo.ProductVersion)
  Else
    Result := 'No build information available';
End;

Function GetFileVersion: String;
Begin
  CreateInfo;

  If FInfo.BuildInfoAvailable Then
    Result := ProductVersionToString(FInfo.FixedInfo.FileVersion)
  Else
    Result := 'No build information available';
End;

{ TVersionInfo }

Function TVersionInfo.GetFixedInfo: TVersionFixedInfo;
Begin
  Result := FVersResource.FixedInfo;
End;

Function TVersionInfo.GetStringFileInfo: TVersionStringFileInfo;
Begin
  Result := FVersResource.StringFileInfo;
End;

Function TVersionInfo.GetVarFileInfo: TVersionVarFileInfo;
Begin
  Result := FVersResource.VarFileInfo;
End;

Constructor TVersionInfo.Create;
Begin
  Inherited Create;

  FVersResource := TVersionResource.Create;
  FBuildInfoAvailable := False;
End;

Destructor TVersionInfo.Destroy;
Begin
  FVersResource.Free;

  Inherited Destroy;
End;

Procedure TVersionInfo.Load(Instance: THandle);
Var
  Stream: TResourceStream;
  ResID: Integer;
  Res: TFPResourceHandle;
Begin
  FBuildInfoAvailable := False;
  ResID := 1;

  // Defensive code to prevent failure if no resource available...
  Res := FindResource(Instance, PChar(PtrInt(ResID)), PChar(RT_VERSION));
  If Res = 0 Then
    Exit;

  Stream := TResourceStream.CreateFromID(Instance, ResID, PChar(RT_VERSION));
  Try
    FVersResource.SetCustomRawDataStream(Stream);

    // access some property to load from the stream
    FVersResource.FixedInfo;

    // clear the stream
    FVersResource.SetCustomRawDataStream(nil);

    FBuildInfoAvailable := True;
  Finally
    Stream.Free;
  End;
End;

Initialization
  FInfo := nil;

Finalization
  If Assigned(FInfo) Then
    FInfo.Free;
End.
