Unit ImageMagickSupport;

{$mode objfpc}{$H+}

// Running the command line interface itself is so easy I see no
// need to subclass that code.
// This is really here just to find the directory

Interface

Uses
  Classes, SysUtils;

Function ImageMagickAvailable: Boolean;
Function ImageMagickPath: String;
Procedure SetImageMagickPath(AValue: String);
Procedure InitializeImageMagick;

Implementation

Uses
  Forms;

Var
  FImageMagickPath: String;

Function ImageMagickAvailable: Boolean;
Begin
  Result := FImageMagickPath <> '';
End;

Function ImageMagickPath: String;
Begin
  Result := FImageMagickPath;
End;

Procedure SetImageMagickPath(AValue: String);
Begin
  If DirectoryExists(AValue) Then
    FImageMagickPath := AValue
  Else
    FImageMagickPath := '';
End;

Procedure InitializeImageMagick;
Begin
  FImageMagickPath := IncludeTrailingBackslash(Application.Location) + 'ImageMagick';
  If DirectoryExists(FImageMagickPath) Then
    Exit;

  FImageMagickPath := IncludeTrailingBackslash(Application.Location) + '..\ImageMagick';
  If DirectoryExists(FImageMagickPath) Then
    Exit;

  FImageMagickPath := '';
End;

Initialization
  FImageMagickPath := '';

End.
