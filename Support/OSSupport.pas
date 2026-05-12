Unit OSSupport;

{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : OSSupport.pas
  Description
    Helper Routines when interacting with the OS

    All the Run/Runex variations from Lazarus Wiki and Forum
    ShellDirectory code from a Delphi StackOverflow
    HTML Clipboard was just hard work - poured through MS documentation
      and StackOverflow, then when I got basis working, spent ages tweaking
      HTML_STYLE_SHEET
      This code was originally in StringSupport, refactored here in 2023

  Source
    Copyright (c) 2025
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    ~2008: Creation date unknown, original local SVN repository lost
    2014-07-05: Uploaded to SourceForge/Package "Shared"
    2023-10-14: Last commit in SourceForge (moved HTML Clipboard from StringSupport to here)
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
  Classes, SysUtils, UTF8Process, Controls, Forms, LCLType
  {$IFDEF WINDOWS}, Windows, ShellAPI{$ENDIF}
  {$IFDEF UNIX}, BaseUnix {$ENDIF}
  ;

Procedure LaunchFile(sFilename: String; sParameters: String = '');
Procedure LaunchDocument(sFilename: String);

// Launch an external application and immediately return control to the caller.
// This is intended for GUI applications such as VLC, editors, viewers, etc.
// No output is captured and the launched process is not monitored or waited on.
//
// AExecutable  - Full path to the executable to launch
// AParams      - Command line parameters passed to the executable
Procedure LaunchExternalTool(Const AExecutable, AParams: String);

// Execute a command line application and capture anything written to stdout.
// This call blocks until the process exits.
//
// IMPORTANT:
// This routine uses pipes instead of poWaitOnExit to avoid deadlocks when
// large amounts of output are generated.
//
// AExecutable     - Full path to the executable to launch
// AParameters     - Optional parameter list passed to the executable
// ARedirectErr    - If True, stderr is redirected to stdout
// ARunExCallback  - Optional callback triggered while output is being received
//
// Returns the captured stdout/stderr text.
Function RunAndCapture(AExecutable: String; AParamaters: TStrings = nil;
  ARedirectErr: Boolean = False; ARunExCallback: TNotifyEvent = nil): String;

// Convenience overload of RunAndCapture() that accepts a simple array of
// command line parameters instead of a TStrings instance.
//
// ACommandLine    - Full path to the executable to launch
// AParamArray     - Array of command line parameters
// ARedirectErr    - If True, stderr is redirected to stdout
// ARunExCallback  - Optional callback triggered while output is being received
//
// Returns the captured stdout/stderr text.
Function RunAndCapture(ACommandLine: String; AParamArray: Array Of String;
  ARedirectErr: Boolean; ARunExCallback: TNotifyEvent): String;

Procedure SetBusy;
Procedure ClearBusy;

// Windows routines to register and unregister a program in Explorer Right Click
Function ShellDirectoryCommand(AAppName: String): String;
Function ShellDirectoryRegister(AAppName: String; ACommand: String; ACaption: String): Boolean;
// Return True if success
Function ShellDirectoryUnRegister(AAppName: String): Boolean; // Return True if success

Procedure CopyHTMLToClipboard(AHTML: TStringList; ABaseFolder: String = '';
  BAlsoAsText: Boolean = False);

Procedure SetEnvVar(Const Name, Value: String);
Procedure AddToEnvironmentPath(AFolder: String);

Const
  HTML_STYLE_SHEET =
    '<style type="text/css">' + LineEnding + '  .unknown{' + LineEnding +
    '    font-size: 10pt;' + LineEnding + '    font-family: Arial, Helvetica, sans-serif;' +
    LineEnding + '  }' + LineEnding + '  .number{' + LineEnding +
    '    font-size: 10pt;' + LineEnding + '    font-family: Arial, Helvetica, sans-serif;' +
    LineEnding + '    text-align: right;' + LineEnding + '  }' + LineEnding +
    '  .text{' + LineEnding + '    mso-number-format:"\@";' + LineEnding +
    '    font-size: 10pt;' + LineEnding + '    font-family: Arial, Helvetica, sans-serif;' +
    LineEnding + '  }' + LineEnding + '  table.StyleTable {' + LineEnding +
    '    width: 100%;' + LineEnding + '    border: 1px solid black;' +
    LineEnding + '    border-collapse: collapse;' + LineEnding +
    '    vertical-align: top;' + LineEnding + '  }' + LineEnding +
    '  table.StyleTable th {' + LineEnding + '    border: 1px solid black;' +
    LineEnding + '    border-collapse: collapse;' + LineEnding +
    '    background-color: rgb(192, 192, 192);' + LineEnding + '  }' +
    LineEnding + '  table.StyleTable td {' + LineEnding + '    border: 1px solid black;' +
    LineEnding + '  }' + LineEnding + '</style>';

Implementation

Uses
  Registry, Process, Clipbrd, StringSupport, FileSupport, SyncObjs;

Var
  LBusy: Longint = 0;

Procedure LaunchFile(sFilename: String; sParameters: String);
Var
  oProcess: TProcessUTF8;
Begin
  oProcess := TProcessUTF8.Create(nil);
  Try
    //oProcess.Executable := sFilename;
    //oProcess.Parameters.Add(sParameters);
    oProcess.CommandLine := sFilename + ' ' + sParameters;

    oProcess.Execute;
  Finally
    oProcess.Free;
  End;
End;

Procedure LaunchDocument(sFilename: String);
Begin
  {$IFDEF WINDOWS}
  ShellExecute(0, 'open', Pansichar(sFilename), nil, nil, SW_SHOWNORMAL);
  {$ELSE}
  Run(sFilename);
  {$ENDIF}
End;

Function RunAndCapture(AExecutable: String; AParamaters: TStrings = nil;
  ARedirectErr: Boolean = False; ARunExCallback: TNotifyEvent = nil): String;
Const
  READ_BYTES = 2048;
Var
  oStrings: TStringList;
  oStream: TMemoryStream;
  oProcess: TProcess;
  iNumBytes: Longint;
  iBytesRead: Longint;
  i: Integer;
Begin
  // A temp Memorystream is used to buffer the output
  oStream := TMemoryStream.Create;
  iBytesRead := 0;
  Try
    oProcess := TProcess.Create(nil);
    Try
      If Assigned(AParamaters) Then
      Begin
        oProcess.Executable := AExecutable;

        For i := 0 To AParamaters.Count - 1 Do
          oProcess.Parameters.Add(AParamaters[i]);
      End
      Else
        oProcess.CommandLine := AExecutable;

      // We cannot use poWaitOnExit here since we don't
      // know the size of the output. On Linux the size of the
      // output pipe is 2 kB; if the output data is more, we
      // need to read the data. This isn't possible since we are
      // waiting. So we get a deadlock here if we use poWaitOnExit.
      If ARedirectErr Then
        oProcess.Options := [poNoConsole, poUsePipes, poStderrToOutPut]
      Else
        oProcess.Options := [poNoConsole, poUsePipes];

      oProcess.Execute;
      While oProcess.Running Do
      Begin
        // make sure we have room
        oStream.SetSize(iBytesRead + READ_BYTES);

        // try reading it
        iNumBytes := oProcess.Output.Read((oStream.Memory + iBytesRead)^, READ_BYTES);
        If iNumBytes > 0 Then
        Begin
          Inc(iBytesRead, iNumBytes);

          If Assigned(ARunExCallback) Then
            ARunExCallback(nil);
        End
        Else
          Sleep(100)// no data, wait 100 ms
        ;
      End;

      // read last part
      Repeat
        // make sure we have room
        oStream.SetSize(iBytesRead + READ_BYTES);

        // try reading it
        iNumBytes := oProcess.Output.Read((oStream.Memory + iBytesRead)^, READ_BYTES);

        If iNumBytes > 0 Then
          Inc(iBytesRead, iNumBytes);
      Until iNumBytes <= 0;

      oStream.SetSize(iBytesRead);

      oStrings := TStringList.Create;
      Try
        oStrings.LoadFromStream(oStream);
        Result := oStrings.Text;
      Finally
        oStrings.Free;
      End;
    Finally
      oProcess.Free;
    End;
  Finally
    oStream.Free;
  End;
End;

Function RunAndCapture(ACommandLine: String; AParamArray: Array Of String;
  ARedirectErr: Boolean; ARunExCallback: TNotifyEvent): String;
Var
  slParameters: TStringList;
  s: String;
Begin
  slParameters := TStringList.Create;
  Try
    For s In AParamArray Do
      slParameters.Add(s);

    Result := RunAndCapture(ACommandLine, slParameters, ARedirectErr, ARunExCallback);
  Finally
    slParameters.Free;
  End;
End;

Procedure LaunchExternalTool(Const AExecutable, AParams: String);
Var
  oProcess: TProcess;
Begin
  oProcess := TProcess.Create(nil);
  Try
    oProcess.Executable := AExecutable;

    // Simple version, preserving your current command-line style
    oProcess.CommandLine := Format('"%s" %s', [AExecutable, AParams]);

    oProcess.Options := [];
    oProcess.Execute;
  Finally
    oProcess.Free;
  End;
End;

Procedure SetBusy;
Begin
  InterlockedIncrement(LBusy);
  Application.MainForm.Cursor := crHourglass;
  Screen.Cursor := crHourglass;
End;

Procedure ClearBusy;
Begin
  InterlockedDecrement(LBusy);
  If LBusy <= 0 Then
  Begin
    LBusy := 0;

    Application.MainForm.Cursor := crDefault;
    Screen.Cursor := crDefault;
  End;
End;

Function ShellDirectoryCommand(AAppName: String): String;
Var
  oReg: TRegistry;
Begin
  Result := '';
  oReg := TRegistry.Create;
  Try
    oReg.RootKey := HKEY_CLASSES_ROOT;
    If oReg.OpenKeyReadOnly(Format('\Directory\Shell\%s\Command', [AAppName])) Then
      Result := oReg.ReadString('');
  Finally
    oReg.Free;
  End;
End;

Function ShellDirectoryRegister(AAppName: String; ACommand: String; ACaption: String): Boolean;
Var
  oReg: TRegistry;
Begin
  Result := False;
  oReg := TRegistry.Create;
  Try
    oReg.RootKey := HKEY_CLASSES_ROOT;
    If oReg.OpenKey(Format('\Directory\Shell\%s', [AAppName]), True) Then
    Begin
      oReg.WriteString('', ACaption);

      If oReg.OpenKey(Format('\Directory\Shell\%s\Command', [AAppName]), True) Then
      Begin
        oReg.WriteString('', ACommand);

        Result := True;
      End;
    End;

  Finally
    oReg.Free;
  End;
End;

Function ShellDirectoryUnRegister(AAppName: String): Boolean;
Var
  oReg: TRegistry;
Begin
  oReg := TRegistry.Create(KEY_WRITE);
  Try
    oReg.RootKey := HKEY_CLASSES_ROOT;
    oReg.Access := KEY_ALL_ACCESS;
    oReg.DeleteKey(Format('\Directory\Shell\%s\Command', [AAppName]));
    oReg.DeleteKey(Format('\Directory\Shell\%s', [AAppName]));
    Result := Not oReg.OpenKeyReadOnly(Format('\Directory\Shell\%s', [AAppName]));
  Finally
    oReg.Free;
  End;
End;

Procedure CopyHTMLToClipboard(AHTML: TStringList; ABaseFolder: String = '';
  BAlsoAsText: Boolean = False);
Var
  oHTML: TStringList;
  cfHTMLFormat: TClipboardFormat;
  oHTMLStream: TMemoryStream;
  iStartHTML, iEndHTML, iStartFragment, iEndFragment: Integer;
Const
  HTML_MIME = 'HTML Format';
  HEADER = '<html><head>' + HTML_STYLE_SHEET + '</head><body><!--StartFragment-->';
  FOOTER1 = '<!--EndFragment-->';
  FOOTER2 = '</body></html>';
  NATIVEHEADER = 'Version:0.9'#13#10 + 'StartHTML:%.10d'#13#10 + 'EndHTML:%.10d'#13#10 +
    'StartFragment:%.10d'#13#10 + 'EndFragment:%.10d';
Begin
  // Reponsibility of caller to clear Clipboard - they may add additional formats

  oHTML := TStringList.Create;
  Try
    // HTML may contain image references that are relative.
    If ABaseFolder <> '' Then
      AHTML.Text := FindReplace(AHTML.Text, 'src="', Format('src="%s',
        [IncludeSlash(ABaseFolder)]));

    If BAlsoAsText Then
      Clipboard.AsText := AHTML.Text;

    // Load the HTML into the string list
    oHTML.Assign(AHTML);

    iStartHTML := 105;
    // Big thanks to the SynEdit team for their solution to the variable header length problem
    iStartFragment := iStartHTML + Length(HEADER) + 2;
    iEndFragment := iStartFragment + Length(oHTML.Text) + Length(FOOTER1) + 2;
    iEndHTML := iEndFragment + LENGTH(FOOTER2) + 2;

    // insert the native header and opening html tags at the start of the string
    oHTML.Insert(0, Format(NATIVEHEADER, [iStartHTML, iEndHTML, iStartFragment,
      iEndFragment]));
    oHTML.Insert(1, HEADER);

    // append the closing tags to the end of the string list
    oHTML.Add(FOOTER1);
    oHTML.Add(FOOTER2);

    // Ensure the HTML mime type is registered
    cfHTMLFormat := Clipboard.FindFormatID(HTML_MIME);
    If cfHTMLFormat = 0 Then
      cfHTMLFormat := RegisterClipboardFormat(HTML_MIME);

    // Save the HTML to the clipboard
    oHTMLStream := TMemoryStream.Create;
    Try
      oHTML.SaveToStream(oHTMLStream);
      oHTMLStream.Position := 0;

      Clipboard.AddFormat(cfHTMLFormat, oHTMLStream);
    Finally
      oHTMLStream.Free;
    End;
  Finally
    oHTML.Free;
  End;
End;

Procedure SetEnvVar(Const Name, Value: String);
Begin
  {$IFDEF UNIX}
  // Use FpSetEnv, expects 'NAME=VALUE'
  FpSetEnv(PChar(Name + '=' + Value));
  {$ENDIF}

  {$IFDEF WINDOWS}
  // SetEnvironmentVariableA (ANSI version)
  SetEnvironmentVariable(PChar(Name), PChar(Value));
  {$ENDIF}
End;

Procedure AddToEnvironmentPath(AFolder: String);
Var
  sCurrentPath: String;
Begin
  sCurrentPath := SysUtils.GetEnvironmentVariable('PATH');
  If sCurrentPath = '' Then
    SetEnvVar('PATH', AFolder)
  Else
    SetEnvVar('PATH', AFolder + PathSeparator + sCurrentPath);
End;

End.
