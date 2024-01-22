Unit FrameCSVs;

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
