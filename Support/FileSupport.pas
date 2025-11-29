Unit FileSupport;

{$mode objfpc}{$H+}

// If you're here because you forgot the name of the LCL file management unit
// its FileUtil

Interface

Uses
  Classes, Graphics, Clipbrd, LCLType, SysUtils, Variants,
  {$IFDEF MSWINDOWS}
  Windows
  {$ELSE}
  BaseUnix
  {$ENDIF}
  ;

Type
  TRenameSubFolderOption = (rsfLowercase, rsfUppercase, rsfProperCase);

// File routines
Function IncludeSlash(Const sFolder: String): String;
Function ExcludeSlash(Const sFolder: String): String;
Function FileCount(sFolder: String; sFilemask: String; ARecurse: Boolean = False): Integer;
Function FileSize(sFolder: String; sFilemask: String): Integer;
Function FileSize(sFilename: String): Integer;
Function DeleteDirectoryEx(AFolder: String; AFilemask: String = '';  ARemoveEmptyRoot: Boolean = True): Boolean;
Function FileRename(ASource, ADestination: String): Boolean;
Function RenameSubfolders(ARoot: String; AOperation: TRenameSubFolderOption): Boolean;
Function CopyFileForce(sSource, sDestination: String; bFailIfExists: Boolean = False): Boolean;
Function IsFileAbsolute(AFilename: String): Boolean;

// OS Safety
Function FixOSPathDelimiter(AInput: String): String;

// turns <EXEDIR> into actual exe folder and back again
Function ExpandFolder(sFolder: String): String;
Function ExpandFile(sFile: String): String;
Function ShrinkFolder(sFolder: String): String;
Function ShrinkFile(sFile: String): String;

// Filename Extension helpers
Function IsImage(sExt: String): Boolean;
Function IsVideo(sExt: String): Boolean;
Function IsCSV(sExt: String): Boolean;
Function IsTextfile(sExt: String): Boolean;

// Text file & routines
Function LoadTextFile(AFilename: String): String;
Procedure SaveTextFile(AFilename: String; AText: String);
Function GetIOErrorText(code: Integer): String;
Procedure FlushFileStreamToDisk(AStream: TFileStream);

// Filename generators
Function UniqueFilename(Const ADir, APrefix, AExt: String; AUseGUID: Boolean = True): String;
Function DateToFilename(ADate: TDateTime): String;
Function TimeToFilename(ATime: TDateTime): String;
Function DateTimeToFilename(ADateTime: TDateTime): String;

Const
  EXE_DIR = '<EXEDIR>';
  faAnyFilesExcDirs = faAnyFile And Not faDirectory;
  FileExtAudio: Array[1..4] Of String =
    ('.mp3', '.wma', '.ogg', '.m4b');
  FileExtImage: Array[1..6] Of String =
    ('.jpg', '.bmp', '.tif', '.png', '.gif', '.jpeg');
  FileExtVideo: Array[1..12] Of String =
    ('.mp4', '.mkv', '.wmv', '.avi', '.asf', '.mpg', '.mpeg', '.m4v', '.mov',
    '.flv', '.pkt', '.vob');
  FileExtCSV: Array[1..3] Of String =
    ('.csv', '.survey', '.txt');
  FileExtText: Array[1..8] Of String =
    ('.txt', '.lua', '.pas', '.c', '.me', '.1st', '.ini', '.alias');

Implementation

Uses
  FileUtil, LazFileUtils, Forms, StrUtils, StringSupport;

Function GetIOErrorText(code: Integer): String;
Begin
  Result := 'File I/O Error: ';

  Case code Of
    2: Result += 'File not found';
    3: Result += 'Path not found';
    5: Result += 'Access denied';
    6: Result += 'Invalid handle';
    15: Result += 'Drive not ready';
    103: Result += 'File not open';
    105: Result += 'File not assigned';
    106: Result += 'File not open for input';
    107: Result += 'File not open for output';
    Else
      Result += 'Unknown I/O error';
  End;
End;

Procedure FlushFileStreamToDisk(AStream: TFileStream);
Begin
  {$IFDEF MSWINDOWS}
  If Not FlushFileBuffers(AStream.Handle) Then
    Raise Exception.Create('FlushFileBuffers failed: ' + SysErrorMessage(GetLastError));
  {$ELSE}
  fpfsync(AStream.Handle);
  {$ENDIF}
End;

// Based on code from ChatGPT 5.1 on 29 Nov 2025
// and from GetTempFileName() in osutil.inc (see comment)
Function UniqueFilename(Const ADir, APrefix, AExt: String; AUseGUID: Boolean = True): String;
Var
  GUID: TGUID;
  sGUID: String;
  sExt, sDir: String;
  sFileBase: String;
  i: Integer;
Begin
  // For Threadsafe, MUST use AUseGuid=True

  sExt := AExt;
  If (sExt <> '') And (sExt[1] <> '.') Then
    sExt := '.' + sExt;

  If ADir <> '' Then
    sDir := IncludeTrailingBackslash(ADir)
  Else
    sDir := '';

  If (AUseGuid) Or (sDir = '') Then
  Begin
    CreateGUID(GUID);
    sGUID := GUIDToString(GUID);
    sGUID := StringReplace(sGUID, '{', '', [rfReplaceAll]);
    sGUID := StringReplace(sGUID, '}', '', [rfReplaceAll]);

    Result := sDir + APrefix + sGUID + sExt;
  End
  Else
  Begin
    // Logic swiped from osutil.inc  GetTempFileName()
    // Unfortunately the existing code always used a .tmp extension
    //  so wasn't useful for use with other extensions
    // TODO: Submit a patch to fpc...
    If (APrefix = '') Then
      sFileBase := 'TMP'
    Else
      sFileBase := APrefix;

    i := 0;
    Repeat
      Result := Format('%s%s%.5d%s', [sDir, sFileBase, i, sExt]);
      Inc(i);
    Until Not (FileExists(Result) Or DirectoryExists(Result));
  End;
End;

Function DateToFilename(ADate: TDateTime): String;
Begin
  Result := DateToStr(ADate, GFilenameDateTimeFormat);
End;

Function TimeToFilename(ATime: TDateTime): String;
Begin
  Result := TimeToStr(ATime, GFilenameDateTimeFormat);
End;

Function DateTimeToFilename(ADateTime: TDateTime): String;
Begin
  Result := DateTimeToStr(ADateTime, GFilenameDateTimeFormat);
End;

Function FixOSPathDelimiter(AInput: String): String;
Begin
  {$IFDEF WINDOWS}
  Result := StringReplace(AInput, '/', '\', [rfReplaceAll]);
  {$ELSE}
  Result := StringReplace(AInput, '\', '/', [rfReplaceAll]);
  {$ENDIF}
End;

Function ExpandFolder(sFolder: String): String;
Begin
  Result := IncludeSlash(ExpandFile(sFolder));
End;

Function ShrinkFolder(sFolder: String): String;
Begin
  Result := IncludeSlash(ShrinkFile(sFolder));
End;

Function ExpandFile(sFile: String): String;
Var
  sExeDir: String;
Begin
  sExeDir := ExcludeSlash(Application.Location);

  If Pos(EXE_DIR, sFile) > 0 Then
    Result := sExeDir + Copy(sFile, Length(EXE_DIR) + 1, Length(sFile))
  Else
    Result := sFile;
End;

Function ShrinkFile(sFile: String): String;
Var
  sExeDir: String;
Begin
  sExeDir := IncludeSlash(Application.Location);

  If (Pos(sExeDir, sFile) > 0) Then
    Result := EXE_DIR + '\' + Copy(sFile, Length(sExeDir) + 1, Length(sFile))
  Else
    Result := sFile;
End;

// TODO Move to String Support
Function IsImage(sExt: String): Boolean;
Begin
  Result := InArray(Lowercase(sExt), FileExtImage);
End;

Function IsVideo(sExt: String): Boolean;
Begin
  Result := InArray(Lowercase(sExt), FileExtVideo);
End;

Function IsCSV(sExt: String): Boolean;
Begin
  Result := InArray(Lowercase(sExt), FileExtCSV);
End;

Function IsTextfile(sExt: String): Boolean;
Begin
  Result := InArray(Lowercase(sExt), FileExtText) Or IsCSV(sExt);
End;

Function LoadTextFile(AFilename: String): String;
Var
  oStream: TFileStream;
  oString: TStringStream;
Begin
  If Not FileExists(AFilename) Then
    Exit('');
  oStream := TFileStream.Create(AFilename, fmOpenRead Or fmShareDenyWrite);
  Try
    oString := TStringStream.Create('', TEncoding.UTF8);
    Try
      oString.CopyFrom(oStream, oStream.Size);
      Result := oString.DataString;
    Finally
      oString.Free;
    End;
  Finally
    oStream.Free;
  End;
end;

Procedure SaveTextFile(AFilename: String; AText: String);
Var
  oStream: TFileStream;
  oString: TStringStream;
Begin
  // Create a string ostream from your Atext
  oString := TStringStream.Create(AText, TEncoding.UTF8);
  Try
    // Create or overwrite the file
    oStream := TFileStream.Create(AFilename, fmCreate);
    Try
      oStream.CopyFrom(oString, oString.Size);
    Finally
      oStream.Free;
    End;
  Finally
    oString.Free;
  End;
End;

Function IncludeSlash(Const sFolder: String): String;
Begin
  Result := IncludeTrailingPathDelimiter(sFolder);
End;

Function ExcludeSlash(Const sFolder: String): String;
Begin
  Result := ExcludeTrailingPathDelimiter(sFolder);
End;

Function FileCount(sFolder: String; sFilemask: String; ARecurse: Boolean): Integer;
Var
  srTemp: TSearchRec;
  sTemp: String;
  iError: Longint;
Begin
  Result := 0;

  sTemp := IncludeSlash(sFolder);

  If ARecurse Then
  Begin
    iError := FindFirst(sTemp + '*', faDirectory, srTemp);
    Try
      While (iError = 0) Do
      Begin
        If (srTemp.Name <> '.') And (srTemp.Name <> '..') Then
          If (srTemp.Attr And faDirectory) = faDirectory Then
            Result := Result + FileCount(sTemp + srTemp.Name, sFilemask, True);

        iError := SysUtils.FindNext(srTemp);
      End;
    Finally
      SysUtils.FindClose(srTemp);
    End;
  End;

  iError := FindFirst(sTemp + sFilemask, faAnyFile, srTemp);
  Try
    While (iError = 0) Do
    Begin
      If (srTemp.Attr And faDirectory) <> faDirectory Then
        Result := Result + 1;

      iError := SysUtils.FindNext(srTemp);
    End;
  Finally
    SysUtils.FindClose(srTemp);
  End;
End;

//http://forum.lazarus.freepascal.org/index.php/topic,8541.msg196918.html#msg196918
Function FileSize(sFolder: String; sFilemask: String): Integer;
Var
  oSearchRec: TSearchRec;
  iTemp: Integer;
Begin
  Result := 0;

  If sFolder[Length(sFolder)] <> '\' Then
    sFolder := sFolder + '\';

  iTemp := FindFirst(sFolder + sFilemask, faAnyFile, oSearchRec);

  While iTemp = 0 Do
  Begin
    Inc(Result, oSearchRec.Size);

    If (oSearchRec.Attr And faDirectory > 0) And ((oSearchRec.Name <> '.') And
      (oSearchRec.Name <> '..')) Then
      Inc(Result, FileSize(sFolder + oSearchRec.Name, sFilemask));

    iTemp := FindNext(oSearchRec);
  End;

  SysUtils.FindClose(oSearchRec);
End;

Function FileSize(sFilename: String): Integer;
Begin
  Result := FileUtil.FileSize(sFilename);
End;

Function FileRename(ASource, ADestination: String): Boolean;
Var
  sFolder: String;
Begin
  // Ensure the folder exists
  sFolder := ExtractFileDir(ADestination);
  ForceDirectory(sFolder);

  Result := SysUtils.RenameFile(ASource, ADestination);
End;

Function RenameSubfolders(ARoot: String; AOperation: TRenameSubFolderOption): Boolean;
Var
  oSearchRec: TSearchRec;
  iTemp: Integer;

  Function New(AInput: String): String;
  Begin
    Result := '';

    Case AOperation Of
      rsfLowercase: Result := Lowercase(AInput);
      rsfUppercase: Result := Uppercase(AInput);
      rsfProperCase: Result := AnsiProperCase(AInput, StdWordDelims);
    End;
  End;

Begin
  Result := True;

  ARoot := IncludeSlash(ARoot);
  iTemp := FindFirst(ARoot + '*.*', faAnyFile, oSearchRec);

  While iTemp = 0 Do
  Begin
    If (oSearchRec.Attr And faDirectory > 0) And ((oSearchRec.Name <> '.') And
      (oSearchRec.Name <> '..')) Then
    Begin
      If Not FileRename(ARoot + IncludeSlash(oSearchRec.Name), ARoot +
        IncludeSlash(New(oSearchRec.Name))) Then
        Result := False;

      If Not RenameSubfolders(ARoot + New(oSearchRec.Name), AOperation) Then
        Result := False;
    End;

    iTemp := FindNext(oSearchRec);
  End;

  SysUtils.FindClose(oSearchRec);
End;

(*  Looks like the default CopyFile will return false if the file exists
  this forces an overwrite WITHOUT prompting the user....   *)
Function CopyFileForce(sSource, sDestination: String; bFailIfExists: Boolean): Boolean;
Var
  bDestExists: Boolean;
Begin
  bDestExists := FileExists(sDestination);
  Result := False;

  If bFailIfExists And bDestExists Then
    Exit;

  If bDestExists And Not bFailIfExists Then
    SysUtils.DeleteFile(sDestination);

  Result := CopyFile(sSource, sDestination);
End;

Function IsFileAbsolute(AFilename: String): Boolean;
Begin
  AFilename := Trim(AFilename);

  {$IFDEF WINDOWS}
  Result := (Copy(AFilename, 2, 1) = ':') Or (Copy(Afilename, 1, 2) = '\\');
  {$ELSE}
  Result := (Copy(AFilename, 1, 1) = '/');
  {$ENDIF}
End;

//  http://forum.lazarus.freepascal.org/index.php/topic,16093.msg87124.html#msg87124

Function DeleteDirectoryEx(AFolder: String; AFilemask: String = '';
  ARemoveEmptyRoot: Boolean = True): Boolean;
  // Lazarus fileutil.DeleteDirectory on steroids, works like
  // deltree <directory>, rmdir /s /q <directory> or rm -rf <directory>
  // - removes read-only files/directories (DeleteDirectory doesn't)
  // - removes directory itself
  // Adapted from fileutil.DeleteDirectory, thanks to Paweł Dmitruk

  // Modified by Mike Thompson 25 Oct 2019 to support additional paramters
Var
  oSearchRec: TSearchRec;
  sSourceFolder: String;
  sFilename: String;
  sMask: String;
Begin
  Result := False;

  sSourceFolder := LazFileUtils.CleanAndExpandDirectory(AFolder);

  If AFilemask = '' Then
    sMask := GetAllFilesMask
  Else
    sMask := AFilemask;

  If FindFirstUTF8(sSourceFolder + sMask, faAnyFile, oSearchRec) = 0 Then
    Repeat
      // Ignore directories and files without name:
      If (oSearchRec.Name <> '.') And (oSearchRec.Name <> '..') And (oSearchRec.Name <> '') Then
      Begin
        // Remove all files and directories in this directory:
        sFilename := sSourceFolder + oSearchRec.Name;
        // Remove read-only file attribute so we can delete it:
        If (oSearchRec.Attr And faReadOnly) > 0 Then
          FileSetAttrUTF8(sFilename, oSearchRec.Attr - faReadOnly);
        If (oSearchRec.Attr And faDirectory) > 0 Then
        Begin
          // Directory; exit with failure on error
          If Not DeleteDirectoryEx(sFilename, AFilemask, True) Then
            exit;
        End
        Else
        If Not DeleteFileUTF8(sFilename) Then
          exit;
      End;
    Until FindNextUTF8(oSearchRec) <> 0;
  FindCloseUTF8(oSearchRec);

  // By searching with a Mask above, we preclude any subfolders...
  If AFilemask <> '' Then
  Begin
    If FindFirstUTF8(sSourceFolder + GetAllFilesMask, faDirectory, oSearchRec) = 0 Then
      Repeat
        // Ignore directories and files without name:
        If (oSearchRec.Name <> '.') And (oSearchRec.Name <> '..') And (oSearchRec.Name <> '') Then
        Begin
          sFilename := sSourceFolder + oSearchRec.Name;

          If (oSearchRec.Attr And faDirectory) > 0 Then
            If Not DeleteDirectoryEx(sFilename, AFilemask, True) Then
              exit// Directory; exit with failure on error
          ;
        End;
      Until FindNextUTF8(oSearchRec) <> 0;
    FindCloseUTF8(oSearchRec);
  End;



  // Remove "root" directory; exit with failure on error:
  If ARemoveEmptyRoot Then
    If (Not RemoveDirUTF8(AFolder)) Then
      exit;
  Result := True;
End;

End.
