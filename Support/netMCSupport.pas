Unit netMCSupport;

{$mode objfpc}{$H+}

// Running the command line interface itself is so easy I see no
// need to subclass that code.
// This is really here just to find the directory

Interface

Uses
  Classes, SysUtils;

Function netMCAvailable: Boolean;
Function netMCPath: String;
Procedure SetnetMCPath(AValue: String);
Procedure InitializenetMC;

Function ProbeFile(sFilename: String): String;

Implementation

Uses
  Forms, OSSupport;

Var
  FnetMCPath: String;

Function netMCAvailable: Boolean;
Begin
  Result := FnetMCPath <> '';
End;

Function netMCPath: String;
Begin
  Result := FnetMCPath;
End;

Procedure SetnetMCPath(AValue: String);
Begin
  If DirectoryExists(AValue) Then
    FnetMCPath := AValue
  Else
    FnetMCPath := '';
End;

Procedure InitializenetMC;
Begin
  FnetMCPath := IncludeTrailingBackslash(Application.Location) + 'netMC';
  If DirectoryExists(FnetMCPath) Then
    Exit;

  FnetMCPath := IncludeTrailingBackslash(Application.Location) + '..\netMC';
  If DirectoryExists(FnetMCPath) Then
    Exit;

  FnetMCPath := '';
End;

Function ProbeFile(sFilename: String): String;
Var
  sCommand: String;
Begin
  sCommand := Format('%s\mnet_epkt.exe "%s"', [netMCPath, sFilename]);
  Result := RunEx(sCommand);
End;

Initialization
  FnetMCPath := '';

End.
