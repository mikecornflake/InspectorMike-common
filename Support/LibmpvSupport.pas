Unit LibmpvSupport;

//libmpv-2.dll
//Version: 0.41.0-697-g13a3e3ad0
//Source: mpv project (https://mpv.io)
//Windows build: Shinchiro developer build
//License: See mpv Copyright and License files


{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils;

Function LibmpvAvailable: Boolean;
Function LibmpvFile: String;
Procedure SetLibmpvFile(AValue: String);
Function InitializeLibmpv: Integer;

Implementation

Uses
  Forms, OSSupport, FileSupport, FileUtil, libMPV.Client;

Var
  FLibmpvFile: String;

Function LibmpvAvailable: Boolean;
Begin
  Result := (FLibmpvFile <> '') And FileExists(FLibmpvFile);
End;

Function LibmpvFile: String;
Begin
  Result := FLibmpvFile;
End;

Procedure SetLibmpvFile(AValue: String);
Begin
  If FileExists(AValue) Then
    FLibmpvFile := AValue
  Else
    FLibmpvFile := '';
End;

Function InitializeLibmpv: Integer;
Var
  sApplicationFolder, sTemp: String;
Begin
  Result := MPV_ERROR_SUCCESS;

  If IsLibMPV_Loaded Then
    Exit;

  Result := MPV_ERROR_GENERIC;

  If (FLibmpvFile = '') Then
  Begin
    // TODO Linux
    sApplicationFolder := IncludeTrailingBackslash(Application.Location);
    sTemp := FindFolder([sApplicationFolder, sApplicationFolder + 'Apps',
      sApplicationFolder + '..', sApplicationFolder + '..\Apps'], 'mpv', 'libmpv-2.dll');

    If DirectoryExists(sTemp) Then
      FLibmpvFile := IncludeSlash(sTemp) + 'libmpv-2.dll';
  End;

  If (FLibmpvFile <> '') And FileExists(FLibmpvFile) Then
    Result := Load_libMPV(FLibmpvFile)
  Else
  Begin
    FLibmpvFile := '';
  End;
End;

End.
