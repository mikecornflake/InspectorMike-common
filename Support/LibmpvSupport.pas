Unit LibmpvSupport;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils;

Function LibmpvAvailable: Boolean;
Function LibmpvFile: String;
Procedure SetLibmpvFile(AValue: String);
Procedure InitializeLibmpv;

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

Procedure InitializeLibmpv;
Var
  sApplicationFolder, sTemp: String;
Begin
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
    Load_libMPV(FLibmpvFile)
  Else
    FLibmpvFile := '';
End;

End.
