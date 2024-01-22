Unit ExportersFPVectorial;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, DB, Exporters, fpvectorial;

Type

    { TDocumentExporter }

  TDocumentExporter = Class(TExporter)
  Private
    FDoc: TvVectorialDocument;
    FPage: TvTextPageSequence;
    FStyleRight: TvStyle;
  Public
    Class Function Extensions: String; Override;
    Class Function Description(AExt: String): String; Override;

    Constructor Create(sFilename: String); Override;
    Destructor Destroy; Override;

    Procedure Heading(sHeading: String; iLevel: Integer = 1); Override;
    Procedure Paragraph(sText: String = ''); Override;
    Procedure Dataset(oDataset: TDataset;
      ATableExportOptions: TTableExportOptions = teActiveAndHeader); Override;
    Procedure Images(oDataset: TDataset; bOnlyActiveRow: Boolean = False); Override;

    Procedure PageBreak; Override;

    Function Save: Boolean; Override;
  End;

Implementation

Uses
  fpvutils, odtvectorialwriter, docxvectorialwriter;

{ TDocumentExporter }

Class Function TDocumentExporter.Extensions: String;
Begin
  Result := '.docx;.odt';
End;

Class Function TDocumentExporter.Description(AExt: String): String;
Begin
  Case AExt Of
    '.docx': Result := 'Microsoft Word 2007 & above';
    '.odt': Result := 'LibreWriter Document (LibreOffice)';
    Else
      Result := Inherited Description(AExt);
  End;
End;

Constructor TDocumentExporter.Create(sFilename: String);
Begin
  Inherited Create(sFilename);

  FDoc := TvVectorialDocument.Create;
  FDoc.SetDefaultPageFormat(vpA4);

  FDoc.AddStandardTextDocumentStyles(vfODT);

  // Change the FP Vectorial defaults to make Mike happy...
  FDoc.StyleTextBody.Font.Name := 'Calibri';
  FDoc.StyleTextBody.Font.Size := 9;
  FDoc.StyleTextBody.MarginTop := 0.5;
  FDoc.StyleTextBody.MarginBottom := 0.5;
  FDoc.StyleTextBody.MarginRight := 0.5;
  FDoc.StyleTextBody.MarginLeft := 0.5;
  FDoc.StyleTextBody.SetElements += [sseMarginLeft, sseMarginRight];

(*  FDoc.StyleTextBodyCentralized.MarginTop := 1;
  FDoc.StyleTextBodyCentralized.MarginBottom := 1;
  FDoc.StyleTextBodyCentralized.MarginRight := 1;
  FDoc.StyleTextBodyCentralized.MarginLeft := 1;
  FDoc.StyleTextBodyCentralized.SetElements :=
  FDoc.StyleTextBody.SetElements + [sseMarginLeft, sseMarginRight];
*)
  FDoc.StyleTextBodyCentralized.ApplyOver(FDoc.StyleTextBody);
  FDoc.StyleTextBodyCentralized.Parent := FDoc.StyleTextBody;

  // Add some custom styles
  FStyleRight := FDoc.AddStyle();
  FStyleRight.ApplyOver(FDoc.StyleTextBody);
  FStyleRight.Name := 'Text Body Right';
  FStyleRight.Alignment := vsaRight;
  FStyleRight.SetElements += [spbfAlignment];
  FStyleRight.Parent := FDoc.StyleTextBody;

  PageBreak;  // If no other pages exist, this creates the first page
End;

Destructor TDocumentExporter.Destroy;
Begin
  //All other entities owned ultimately by FDoc..
  FreeAndNil(FDoc);

  Inherited Destroy;
End;

Procedure TDocumentExporter.Heading(sHeading: String; iLevel: Integer);
Var
  oPara: TvParagraph;
Begin
  oPara := FPage.AddParagraph();

  Case iLevel Of
    1: oPara.Style := FDoc.StyleHeading1;
    2: oPara.Style := FDoc.StyleHeading2;
    Else
      oPara.Style := FDoc.StyleHeading3;
  End;

  oPara.AddText(sHeading);
End;

Procedure TDocumentExporter.Paragraph(sText: String);
Begin
  With FPage.AddParagraph Do
  Begin
    Style := FDoc.StyleTextBody;
    AddText(sText);
  End;
End;

Procedure TDocumentExporter.Dataset(oDataset: TDataset;
  ATableExportOptions: TTableExportOptions = teActiveAndHeader);
Var
  oTable: TvTable;
  oRow: TvTableRow;
  oField: TField;

  Procedure SetText(oField: TField; AHeader: Boolean);
  Var
    oPara: TvParagraph;
  Begin
    oPara := oRow.AddCell.AddParagraph;
    Case oField.Alignment Of
      taLeftJustify: oPara.Style := FDoc.StyleTextBody;
      taRightJustify: oPara.Style := FStyleRight;
      taCenter: oPara.Style := FDoc.StyleTextBodyCentralized;
    End;

    If AHeader Then
      oPara.AddText(oField.DisplayName).Style := FDoc.StyleTextSpanBold
    Else
      oPara.AddText(oField.DisplayText);
  End;

  Procedure ExportCurrentRow(AHeader: Boolean);
  Var
    i: Integer;
  Begin
    For i := 0 To oDataset.FieldCount - 1 Do
    Begin
      oField := oDataset.Fields[i];

      If oField.Visible Then
        SetText(oField, AHeader);
    End;
  End;

Begin
  // TODO ATableExportOptions: TTableExportOptions = teActiveNoHeader
  // not yet supported

  If Not oDataset.Active Then
    Exit;

  oDataset.DisableControls;
  Try
    oTable := FPage.AddTable;
    oTable.PreferredWidth := Dimension(100, dimPercent);

    // Export the Field names
    oRow := oTable.AddRow;
    oRow.BackgroundColor := RGBToFPColor(192, 192, 192);
    oRow.Header := True;

    // Export the header row
    ExportCurrentRow(True);

    // Work out which parts of the dataset to export
    If ATableExportOptions = teActiveAndHeader Then
    Begin
      oRow := oTable.AddRow;
      ExportCurrentRow(False);
    End
    Else
    Begin
      oDataset.First;

      While Not oDataset.EOF Do
      Begin
        oRow := oTable.AddRow;
        ExportCurrentRow(False);
        oDataset.Next;
      End;
    End;
  Finally
    oDataset.EnableControls;
  End;
End;

Procedure TDocumentExporter.Images(oDataset: TDataset; bOnlyActiveRow: Boolean);
Begin
  // Not yet supported
End;

Procedure TDocumentExporter.PageBreak;
Var
  oPara: TvParagraph;
Begin
  FPage := FDoc.AddTextPageSequence();

    // Set the Header
  oPara := FPage.Header.AddParagraph;
  oPara.Style := FDoc.StyleTextBodyCentralized;
  oPara.AddText(Title).Style := FDoc.StyleTextSpanBold;

  // Set the Footer
  oPara := FPage.Footer.AddParagraph;
  oPara.Style := FDoc.StyleTextBodyCentralized;
  oPara.AddText(ExtractFilename(Filename)).Style := FDoc.StyleTextSpanBold;
  oPara.AddText(#09);
  oPara.AddText(#09);
  oPara.AddText('Page ').Style := FDoc.StyleTextSpanBold;
  oPara.AddField(vfkPage).Style := FDoc.StyleTextSpanBold;
  oPara.AddText(' of ').Style := FDoc.StyleTextSpanBold;
  oPara.AddField(vfkNumPages).Style := FDoc.StyleTextSpanBold;
End;

Function TDocumentExporter.Save: Boolean;
Var
  sExt: String;
Begin
  sExt := Lowercase(ExtractFileExt(Filename));

  Case sExt Of
    '.docx': FDoc.WriteToFile(Filename, vfDOCX);
    '.odt': FDoc.WriteToFile(Filename, vfODT);
    Else
      Raise Exception.Create('Filename extension not recognised');
  End;

  Result := True;
End;

End.
