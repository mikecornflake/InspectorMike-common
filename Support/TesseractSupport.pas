Unit TesseractSupport;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils;

Function TesseractAvailable: Boolean;
Function TesseractPath: String;
Procedure SetTesseractPath(AValue: String);
Procedure InitializeTesseract;

Function OCR(AFilename: String): String;

Implementation

Uses
  Forms, StringSupport, FileSupport, FileUtil, OSSupport;

Var
  FTesseractPath: String;

Function TesseractAvailable: Boolean;
Begin
  Result := FTesseractPath <> '';
End;

Function TesseractPath: String;
Begin
  Result := FTesseractPath;

End;

Procedure SetTesseractPath(AValue: String);
Begin
  If DirectoryExists(AValue) Then
    FTesseractPath := AValue
  Else
    FTesseractPath := '';
End;

Procedure InitializeTesseract;
Begin
  If FTesseractPath = '' Then
  Begin
    // By default, use the ffpmeg folder distributed with the app
    FTesseractPath := IncludeTrailingBackslash(Application.Location) + 'Tesseract-OCR';
    If DirectoryExists(FTesseractPath) Then
      Exit;

    // Maybe it was installed in the same folder as the app?
    FTesseractPath := IncludeTrailingBackslash(Application.Location) + '..\Tesseract-OCR';
    If DirectoryExists(FTesseractPath) Then
      Exit;

    // Oh well, search the evironment PATH for the exe...
    FTesseractPath := FindDefaultExecutablePath(Format('tesseract%s', [GetExeExt]));

    If DirectoryExists(FTesseractPath) Then
      Exit;

    FTesseractPath := '';
  End;
End;

Function OCR(AFilename: String): String;
Var
  sCommand: String;
  sTempDir, sOutput: String;
  oStrings: TStringList;
Begin
  If TesseractAvailable Then
  Begin
    sTempDir := IncludeTrailingBackslash(SysUtils.GetTempDir(False)) +
      ChangeFileExt(ExtractFileName(Application.ExeName), '');

    ForceDirectories(sTempDir);

    DeleteFile(sTempDir + '\OCR.txt');
    sOutput := sTempDir + '\OCR';

    sCommand := Format('%s\tesseract%s "%s" "%s"', [FTesseractPath, GetExeExt,
      AFilename, sOutput]);

    RunEx(sCommand, nil, True);

    oStrings := TStringList.Create;
    Try
      oStrings.LoadFromFile(sOutput + '.txt');
      Result := oStrings.Text;
    Finally
      oStrings.Free;
    End;
  End;
End;

Initialization
  FTesseractPath := '';
End.
