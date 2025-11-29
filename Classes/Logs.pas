Unit Logs;

{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : Logs.pas
  Description
    CLI support for xpdf

  Source
    Copyright (c) 2025
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  TODO
    Deprecate this unit

    //TODO: Merge the Threadsafe and stability Log code with this

    I now have several logging units.  This one uses AssignFile,
    I've noticed that if the app crashes, recent data is not logged

    I'm much happier with the Log file created as part of SerialProcessor

  History
    ~2008: Creation date unknown, original local SVN repository lost
           Reference in SourceForge also lost?
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


{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

Type
  TLogCallback = Procedure(Const AMessage: String) Of Object;

  { TLog }

  TLog = Class
  Private
    FLogFile: TextFile;
    FLogFileName: String;
    FIndentLevel: Integer;
    FLogCallback: TLogCallback;
    Function GetIndentString: String;
    Procedure SetLogCallback(AValue: TLogCallback);
  Public
    Constructor Create(Const ALogFileName: String);
    Destructor Destroy; Override;

    Procedure IncIndent;
    Procedure DecIndent;
    Procedure Log(Const AMessage: String);

    Function Filename: String;

    Property OnLog: TLogCallback read FLogCallback write SetLogCallback;
  End;

// Simple helpers to simplify calling code
Function Log: TLog;
Procedure Debug(AMessage: String);
Procedure Debug(AMessage: String; arrParams: Array Of Const);
Procedure Debug(AMessage: TStringList);

Implementation

Var
  FLog: TLog;

Procedure Debug(AMessage: String);
Begin
  Log.Log(AMessage);
End;

Procedure Debug(AMessage: String; arrParams: Array Of Const);
Begin
  Debug(Format(AMessage, arrParams));
End;

Procedure Debug(AMessage: TStringList);
Var
  s: String;
Begin
  If Assigned(AMessage) Then
    For s In AMessage Do
      Debug(s);
End;

Function Log: TLog;
Var
  sFilename: String;
Begin
  sFilename := IncludeTrailingBackslash(GetAppConfigDir(False)) + FormatDateTime(
    'yyyy-mm-dd', now()) + ' debug.log';

  If Not Assigned(FLog) Then
    FLog := TLog.Create(sFilename);

  Result := FLog;
End;

{ TLog }

Constructor TLog.Create(Const ALogFileName: String);
Begin
  FLogFileName := ALogFileName;
  FIndentLevel := 0;

  FLog := Self;
End;

Destructor TLog.Destroy;
Begin
  Inherited Destroy;
End;

Function TLog.GetIndentString: String;
Begin
  Result := StringOfChar(' ', FIndentLevel * 2);
End;

Procedure TLog.SetLogCallback(AValue: TLogCallback);
Begin
  If FLogCallback = AValue Then Exit;
  FLogCallback := AValue;
End;

Procedure TLog.IncIndent;
Begin
  Inc(FIndentLevel);
End;

Procedure TLog.DecIndent;
Begin
  If FIndentLevel > 0 Then
    Dec(FIndentLevel);
End;

Procedure TLog.Log(Const AMessage: String);
Var
  sMessage: String;
Begin
  sMessage := Format('%s: %s%s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now),
    GetIndentString, AMessage]);

    // Send to the main app if it wants
  If assigned(FLogCallback) Then
    FLogCallback(sMessage);

  // Output to file
  AssignFile(FLogFile, FLogFileName);
  Try
    // Create or overwrite the file
    If FileExists(FLogFileName) Then
      Append(FLogFile)
    Else
      Rewrite(FLogFile);

    Writeln(FLogFile, sMessage);
  Finally
    CloseFile(FLogFile);
  End;
End;

Function TLog.Filename: String;
Begin
  Result := FLogFileName;
End;

Initialization
  FLog := nil;

End.
