Unit XPDFSupport;

{$mode objfpc}{$H+}

// Running the command line interface itself is so easy I see no
// need to subclass that code.
// This is really here just to find the directory

Interface

Uses
  Classes, SysUtils;

Function XPDFAvailable: Boolean;
Function XPDFPath: String;
Procedure SetXPDFPath(AValue: String);
Procedure InitializeXPDF;

Implementation

Uses
  Forms;

Var
  FXPDFPath: String;

Function XPDFAvailable: Boolean;
Begin
  Result := FXPDFPath <> '';
End;

Function XPDFPath: String;
Begin
  Result := FXPDFPath;
End;

Procedure SetXPDFPath(AValue: String);
Begin
  If DirectoryExists(AValue) Then
    FXPDFPath := AValue
  Else
    FXPDFPath := '';
End;

Procedure InitializeXPDF;
Begin
  FXPDFPath := IncludeTrailingBackslash(Application.Location) + 'XPDF\bin32';
  If DirectoryExists(FXPDFPath) Then
    Exit;

  FXPDFPath := IncludeTrailingBackslash(Application.Location) + '..\XPDF\bin32';
  If DirectoryExists(FXPDFPath) Then
    Exit;

  FXPDFPath := '';
End;

Initialization
  FXPDFPath := '';

End.
