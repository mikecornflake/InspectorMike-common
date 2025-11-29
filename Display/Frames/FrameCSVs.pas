Unit FrameCSVs;

{-------------------------------------------------------------------------------
  Package   : IM_application
  Unit      : FrameCSVs.pas
  Description
    Frame for previewing CSV files through an SdfDataSet.

  Source
    Copyright (c) 2025
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    ~2008: Creation date unknown, original local SVN repository lost
    2014-07-05: Uploaded to SourceForge/Package "Shared"
    2024-01-22: Migrated to Github.  Refactored package to "IM_application"
    2025-11-29: Added LGPL-3.0-or-later license header

  License
    This file is part of IM_application.lpk.

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
  Classes, SysUtils, DB, SdfData, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, DBCtrls, DBGrids, StdCtrls, FrameBase;

Type

  { TFrameCSV }

  TFrameCSV = Class(TfmeBase)
    dsCSV: TDatasource;
    grdCSV: TDBGrid;
    DBNavigator1: TDBNavigator;
    dsFile: TSdfDataSet;
    lblStatus: TLabel;
  Private
    FDelimiter: Char;
    FFilename: String;
    Procedure SetFilename(Const AValue: String);

    Procedure ClearCSV;
  Public
    Property Filename: String read FFilename write SetFilename;
    Property Delimiter: Char read FDelimiter write FDelimiter;
  End;

Implementation

Uses
  DBSupport;

{$R *.lfm}

{ TFrameCSV }

Procedure TFrameCSV.SetFilename(Const AValue: String);
Begin
  If FFilename = AValue Then
    exit;

  FFilename := AValue;
  If FileExists(FFilename) Then
  Begin
    If dsFile.Active Then
      dsFile.Close;

    dsFile.FileName := FFilename;
    dsFile.Delimiter := FDelimiter;
    dsFile.FirstLineAsSchema := True;

    dsFile.Open;

    InitialiseDBGrid(grdCSV, dsFile, False);

    lblStatus.Caption := Format('%d lines loaded from "%s"', [dsFile.RecordCount, FFilename]);

  End
  Else
    ClearCSV;
End;

Procedure TFrameCSV.ClearCSV;
Begin
  If dsFile.Active Then
    dsFile.Close;

  FFilename := '';
  dsFile.FileName := '';

  lblStatus.Caption := 'No file loaded';
End;

Initialization

End.
