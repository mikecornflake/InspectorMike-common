Unit Exporters;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, DB, SysUtils, ExtCtrls;

Type
  TExporter = Class;
  TExporterClass = Class Of TExporter;
  TTableExportOptions = (teAll, teActiveAndHeader, teActiveNoHeader);
  TValidateFileEvent = Procedure(AObject: TObject; Var AFilename: String) Of Object;
  { TExporter }

  TExporter = Class(TObject)
  Private
    FImageColCount: Integer;
    FImageWidth: Integer;
  Protected
    FFilename: String;
    FImageDir: String;
    FTitle: String;
    FValidateFile: TValidateFileEvent;
  Public
    Class Function Extensions: String; Virtual; // '.docx;.odt'
    Class Function Description(AExt: String): String; Virtual; // Description of Class for Factory

    Constructor Create(sFilename: String); Virtual;

    Property Filename: String read FFilename;
    Property Title: String read FTitle write FTitle;

    Procedure BeginDocument; Virtual;
    Procedure EndDocument; Virtual;

    Procedure Heading(sHeading: String; iLevel: Integer = 1); Virtual; Abstract;
    Procedure Paragraph(sText: String = ''); Virtual; Abstract;
    Procedure Dataset(oDataset: TDataset;
      ATableExportOptions: TTableExportOptions = teActiveAndHeader);
      Virtual; Abstract;
    Procedure Images(oDataset: TDataset; bOnlyActiveRow: Boolean = False);
      Virtual; Abstract;

    Procedure PageBreak; Virtual;

    // New model that lets these tables be built up in a controlled manner, then
    // exported when ready
    Procedure AppendTableRow(ADataset: TDataset); Virtual; Abstract;
    Procedure WriteTable; Virtual; Abstract;

    Function Save: Boolean; Virtual;
    Procedure CopyToClipboard; Virtual;

    Property ImageDir: String read FImageDir write FImageDir;
    Property ImageColCount: Integer read FImageColCount write FImageColCount;
    Property ImageWidth: Integer read FImageWidth write FImageWidth;

    Property ValidateFile: TValidateFileEvent read FValidateFile write FValidateFile;
  End;

  { TTextExporter }

  TTextExporter = Class(TExporter)
  Protected
    FText, FTable: TStringList;
    Function Validate(AField: TField): String; Virtual; Overload;
    Function Validate(sIn: String): String; Virtual; Overload;
  Public
    Class Function Extensions: String; Override;
    Class Function Description(AExt: String): String; Override;

    Constructor Create(sFilename: String); Override;
    Destructor Destroy; Override;

    Procedure BeginDocument; Override;

    Procedure Heading(sHeading: String; iLevel: Integer = 1); Override;
    Procedure Paragraph(sText: String = ''); Override;
    Procedure WriteTable; Override;

    Function Save: Boolean; Override;
    Procedure CopyToClipboard; Override;
  End;

  { TCSVExporter }
  // TODO - HTML Exporter functionality has changed, bring CSV Exporter up to date...
  TCSVExporter = Class(TTextExporter)
  Protected
    Function Validate(sIn: String): String; Override;
  Public
    Class Function Extensions: String; Override;
    Class Function Description(AExt: String): String; Override;

    Procedure Dataset(oDataset: TDataset;
      ATableExportOptions: TTableExportOptions = teActiveAndHeader); Override;
    Procedure Images(oDataset: TDataset; bOnlyActiveRow: Boolean = False); Override;
  End;

  { THTMLExporter }

  THTMLExporter = Class(TTextExporter)
  Protected
    Function Validate(sIn: String): String; Override;
  Public
    Class Function Extensions: String; Override;
    Class Function Description(AExt: String): String; Override;

    Procedure BeginDocument; Override;
    Procedure EndDocument; Override;

    Procedure Heading(sHeading: String; iLevel: Integer = 1); Override;
    Procedure Paragraph(sText: String = ''); Override;
    Procedure Dataset(oDataset: TDataset;
      ATableExportOptions: TTableExportOptions = teActiveAndHeader); Override;
    Procedure Images(oDataset: TDataset; bOnlyActiveRow: Boolean = False); Override;

    Procedure PageBreak; Override;

    Procedure AppendTableRow(ADataset: TDataset); Override;
  End;

  { TExporterFactory }

  TExporterFactory = Class(TList)
  Public
    Procedure RegisterExporter(AClass: TExporterClass);

    Function ExtensionCount: Integer;

    Function ExporterClass(AIndex: Integer): TExporterClass;
    Function ExporterClass(AExt: String): TExporterClass;
    Function CreateExporter(AExt: String; AFilename: String): TExporter;
  End;

Var
  ExporterFactory: TExporterFactory;

Implementation

Uses
  DBSupport, OSSupport, FileSupport, strutils, FileUtil, Clipbrd;

Function FindReplace(Const sStringToSearch, sFind, sReplace: String): String;
Begin
  Result := StringReplace(sStringToSearch, sFind, sReplace, [rfReplaceAll, rfIgnoreCase]);
End;

Function IsImage(sExt: String): Boolean;
Begin
  sExt := LowerCase(sExt);

  Result := (sExt = '.jpg') Or (sExt = '.bmp') Or (sExt = '.tif') Or
    (sExt = '.png') Or (sExt = '.gif') Or (sExt = '.jpeg');
End;

{ TExporterFactory }

Procedure TExporterFactory.RegisterExporter(AClass: TExporterClass);
Begin
  Add(AClass);
End;

Function TExporterFactory.ExtensionCount: Integer;
Var
  i: Integer;
Begin
  Result := 0;

  For i := 0 To Count - 1 Do
    Result := Result + WordCount(TExporterClass(Items[i]).Extensions, [';']);
End;

Function TExporterFactory.ExporterClass(AIndex: Integer): TExporterClass;
Begin
  If (AIndex >= 0) And (AIndex < Count) Then
    Result := TExporterClass(Items[AIndex])
  Else
    Raise Exception.CreateFmt('Exporter index %d not found', [AIndex]);
End;

Function TExporterFactory.ExporterClass(AExt: String): TExporterClass;
Var
  i, j: Integer;
  sExtensions: String;
Begin
  For i := 0 To Count - 1 Do
  Begin
    sExtensions := TExporterClass(Items[i]).Extensions;
    For j := 1 To WordCount(sExtensions, [';']) Do
      If AExt = ExtractDelimited(j, sExtensions, [';']) Then
      Begin
        Result := TExporterClass(Items[i]);
        Break;
      End;
  End;
End;

Function TExporterFactory.CreateExporter(AExt: String; AFilename: String): TExporter;
Var
  oClass: TExporterClass;
Begin
  oClass := ExporterClass(AExt);
  If Assigned(oClass) Then
    Result := oClass.Create(AFilename)
  Else
    Result := nil;
End;

{ TExporter }

Class Function TExporter.Extensions: String;
Begin
  Result := '';
End;

Class Function TExporter.Description(AExt: String): String;
Begin
  Result := '';
End;

Constructor TExporter.Create(sFilename: String);
Begin
  FImageDir := '';
  FFilename := sFilename;
  FTitle := ExtractFilename(ChangeFileExt(sFilename, ''));

  FValidateFile := nil;
End;

Procedure TExporter.BeginDocument;
Begin
  // Do nothing
End;

Procedure TExporter.EndDocument;
Begin
  // Do nothing
End;

Procedure TExporter.PageBreak;
Begin
  // Not going to do anything for most file formats...
End;

Function TExporter.Save: Boolean;
Begin
  Result := False;
End;

Procedure TExporter.CopyToClipboard;
Begin

End;

{ TTextExporter }

Function TTextExporter.Validate(AField: TField): String;
Begin
  If AField.DataType = ftMemo Then
    Result := Validate(AField.AsString)
  Else
    Result := Validate(AField.AsString);
End;

Function TTextExporter.Validate(sIn: String): String;
Begin
  Result := sIn;
End;

Class Function TTextExporter.Extensions: String;
Begin
  Result := '.txt';
End;

Class Function TTextExporter.Description(AExt: String): String;
Begin
  Result := 'Text File';
End;

Constructor TTextExporter.Create(sFilename: String);
Begin
  Inherited Create(sFilename);

  FText := TStringList.Create;
  FTable := TStringList.Create;
End;

Destructor TTextExporter.Destroy;
Begin
  FText.Free;
  FText := nil;

  FTable.Free;
  FTable := nil;

  Inherited Destroy;
End;

Procedure TTextExporter.BeginDocument;
Begin
  Inherited BeginDocument;

  FText.Clear;
  //FText.Add(Chr($EF) + Chr($BB) + Chr($BF)); // Add a UTF-8 Byte Order Mark
End;

Procedure TTextExporter.Paragraph(sText: String);
Begin
  FText.Add(sText);
End;

Procedure TTextExporter.Heading(sHeading: String; iLevel: Integer);
Begin
  FText.Add(sHeading);
End;

Function TTextExporter.Save: Boolean;
Begin
  Result := False;

  If FText.Count > 0 Then
  Begin
    FText[0] := Chr($EF) + Chr($BB) + Chr($BF) + FText[0]; // Insert a UTF-8 Byte Order Mark;
    FText.SaveToFile(FFilename);
    FText.Delete(0);
    Result := True;
  End;
End;

Procedure TTextExporter.CopyToClipboard;
Begin
  Inherited CopyToClipboard;

  Clipboard.Clear;
  CopyHTMLToClipboard(FText, ExtractFilePath(FFilename), True);
End;

Procedure TTextExporter.WriteTable;
Begin
  If FTable.Count > 0 Then
  Begin
    FTable.Add('</table>');
    FText.AddStrings(FTable);
    FTable.Clear;
  End;
End;

{ TCSVExporter }

Function TCSVExporter.Validate(sIn: String): String;
Var
  bHasQuote: Boolean;
  bHasComma: Boolean;
  bHasSpace: Boolean;
  bHasCR: Boolean;

Begin
  bHasQuote := Pos('"', sIn) > 0;
  bHasComma := Pos(',', sIn) > 0;
  bHasSpace := Pos(' ', sIn) > 0;
  bHasCR := (Pos(#13, sIn) > 0) Or (Pos(#10, sIn) > 0);

  sIn := FindReplace(sIn, '"', '""');

  If bHasQuote Or bHasComma Or bHasSpace Or bHasCR Then
    Result := '"' + sIn + '"'
  Else
    Result := sIn;
End;

Class Function TCSVExporter.Extensions: String;
Begin
  Result := '.csv';
End;

Class Function TCSVExporter.Description(AExt: String): String;
Begin
  Result := 'Comma Separated Text File';
End;

Procedure TCSVExporter.Dataset(oDataset: TDataset; ATableExportOptions: TTableExportOptions);
Var
  sTemp: String;
  iCol: Integer;
  oField: TField;
  oBookmark: TBookmark;
Begin
  oBookmark := oDataset.GetBookmark;
  oDataset.DisableControls;
  Try
    sTemp := '';
    For iCol := 0 To oDataset.FieldCount - 1 Do
    Begin
      oField := oDataset.Fields[iCol];
      If oField.Visible Then
        sTemp := sTemp + Validate(oField.DisplayName) + ',';
    End;

    If Trim(sTemp) <> '' Then
    Begin
      // Add the header
      FText.Add(Copy(sTemp, 1, Length(sTemp) - 1));
      sTemp := '';

      oDataset.First;

      While Not oDataset.EOF Do
      Begin
        For iCol := 0 To oDataset.FieldCount - 1 Do
        Begin
          oField := oDataset.Fields[iCol];
          If oField.Visible Then
            sTemp := sTemp + Validate(oField) + ',';
        End;

        // Add the row
        FText.Add(Copy(sTemp, 1, Length(sTemp) - 1));
        sTemp := '';

        oDataset.Next;
      End;
    End;
  Finally
    oDataset.GotoBookmark(oBookmark);
    oDataset.FreeBookmark(oBookmark);
    oDataset.EnableControls;
  End;
End;

Procedure TCSVExporter.Images(oDataset: TDataset; bOnlyActiveRow: Boolean);
Var
  sTemp: String;
  oField: TField;
  oBookmark: TBookmark;
  bHasCaption: Boolean;
  sFilenameField: String;
Begin
  oBookmark := oDataset.GetBookmark;
  oDataset.DisableControls;
  Try
    // Simple one or two column table
    bHasCaption := oDataset.FindField('Caption') <> nil;
    If bHasCaption Then
      sTemp := 'Caption,'
    Else
      sTemp := '';

    sFilenameField := '';
    If Assigned(oDataset.FindField('Filename')) Then
    Begin
      sFilenameField := 'Filename';
      sTemp := sTemp + 'Filename';
    End
    Else If Assigned(oDataset.FindField('Filename_ID')) Then
    Begin
      sFilenameField := 'Filename_ID';
      sTemp := sTemp + 'Filename';
    End;

    If Trim(sTemp) <> '' Then
    Begin
      // Add the header
      FText.Add(sTemp);

      oDataset.First;

      While Not oDataset.EOF Do
      Begin
        sTemp := '';
        If bHasCaption Then
        Begin
          oField := oDataset.FieldByName('Caption');
          sTemp := sTemp + Validate(oField) + ',';
        End;

        oField := oDataset.FieldByName(sFilenameField);
        sTemp := sTemp + Validate(oField);

        // Add the row
        FText.Add(sTemp);

        oDataset.Next;
      End;
    End;
  Finally
    oDataset.GotoBookmark(oBookmark);
    oDataset.FreeBookmark(oBookmark);
    oDataset.EnableControls;
  End;
End;

{ THTMLExporter }

Function THTMLExporter.Validate(sIn: String): String;
Begin
  sIn := FindReplace(sIn, '<', '&lt;');
  sIn := FindReplace(sIn, '>', '&gt;');
  sIn := FindReplace(sIn, #13, '');
  sIn := FindReplace(sIn, #10, '<br style="mso-data-placement:same-cell;" />');
  If Trim(sIn) = '' Then
    Result := '&nbsp;'
  Else
    Result := sIn;
End;

Class Function THTMLExporter.Extensions: String;
Begin
  Result := '.html;.doc;.xls';
End;

Class Function THTMLExporter.Description(AExt: String): String;
Begin
  Case AExt Of
    '.doc': Result := 'Microsoft Word (webpage)';
    '.xls': Result := 'Microsoft Excel (webpage)';
    '.html': Result := 'HTML Web Page';
    Else
      Result := Inherited Description(AExt);
  End;
End;

Procedure THTMLExporter.BeginDocument;
Begin
  Inherited BeginDocument;

  FText.Add('<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">');
  FText.Add('<html>');
  FText.Add('  <head>');
  FText.Add('    <meta content="text/html; charset=ISO-8859-1"  http-equiv="content-type">');
  FText.Add('    <meta content="Data exported via Third Party Utilties">');
  FText.Add('    <title>' + FTitle + '</title>');
  FText.Add('  </head>');
  FText.Add(HTML_STYLE_SHEET); // Use the same Style Sheet as the OSSupport routines
  FText.Add('');
  FText.Add('<body>');
End;

Procedure THTMLExporter.EndDocument;
Begin
  FText.Add('</body>');
  FText.Add('</html>');

  Inherited EndDocument;
End;

Procedure THTMLExporter.Paragraph(sText: String);
Begin
  If sText <> '' Then
    Inherited Paragraph(Format('<p class="text" style=''mso-special-character:line-break''>%s</p>',
      [Validate(sText)]))
  Else
    FText.Add('<p class="text" style=''mso-special-character:line-break''>&nbsp;</p>');
End;

Procedure THTMLExporter.Heading(sHeading: String; iLevel: Integer);
Begin
  Inherited Heading(Format('<h%d>%s</h%d>', [iLevel, Validate(sHeading), iLevel]));
End;

Procedure THTMLExporter.Dataset(oDataset: TDataset; ATableExportOptions: TTableExportOptions);
Var
  oSummaryField: TField;
  bOnlyActiveRow: Boolean;

Begin
  bOnlyActiveRow := ATableExportOptions <> teAll;

  If oDataset.RecordCount <> 0 Then
  Begin
    oSummaryField := oDataset.FindField('Summary');
    If Assigned(oSummaryField) And (bOnlyActiveRow Or (oDataset.RecordCount = 1)) Then
      oSummaryField.Visible := False;

    AppendDatasetAsHTML(FText, oDataset, bOnlyActiveRow, (ATableExportOptions In
      [teAll, teActiveAndHeader]));

    If Assigned(oSummaryField) And (bOnlyActiveRow Or (oDataset.RecordCount = 1)) Then
    Begin
      // Overwrite the </table> tag
      FText[FText.Count - 1] := '    <tr valign="top">';

      // Now, add our additional cells
      FText.Add('      <th class="text" colspan="' + IntToStr(oDataset.FieldCount) +
        '">' + Validate(oSummaryField.DisplayName) + '</th>');
      FText.Add('    </tr>');
      FText.Add('    <tr valign="top">');
      FText.Add('      <td class="text" colspan="' + IntToStr(oDataset.FieldCount) +
        '">' + Validate(oSummaryField) + '</td>');
      FText.Add('    </tr>');
      FText.Add('  </table>');

      oSummaryField.Visible := True;
    End;
  End;

  //Paragraph;
End;

Procedure THTMLExporter.Images(oDataset: TDataset; bOnlyActiveRow: Boolean);
Var
  bHasCaption: Boolean;
  i, iCol: Integer;
  iNumFiles, iFile: Integer;
  oBookmark: TBookmark;
  oFilenameField: TField;
  sCaption: String;
  oCaptions: TStringList;
  sCaptionField: String;
  sExt: String;
  sFilename: String;
  sFolder: String;
  sFullFilename: String;
  sNewFile: String;
  sTemp: String;
  sStartTable: String;

  Function NewHeight(sFilename: String; iWidth: Integer): Integer;
  Var
    oImage: TImage;
  Begin
    oImage := TImage.Create(nil);
    Try
      oImage.Picture.LoadFromFile(sFilename);

      Result := Trunc(oImage.Picture.Height * (iWidth / oImage.Picture.Width));
    Finally
      oImage.Free;
    End;
  End;

  Procedure AddImageCell;
  Var
    i2: Integer;
  Begin
    If iCol = 0 Then
      FText.Add('  <tr>');

    oCaptions.Add(sCaption);

    // Add the image
    FText.Add('    <td width=' + IntToStr(Trunc(100 / FImageColCount)) + '%>' +
      '<div style="text-align: center;">' + '<img width=' + IntToStr(FImageWidth) +
      ' height=' + IntToStr(NewHeight(sNewFile, FImageWidth)) + ' src="' +
      FindReplace(sFilename, sFolder + '\', '') + '"></br></div></td>');

    iCol := (iCol + 1) Mod FImageColCount;

    If iCol = 0 Then
    Begin
      FText.Add('  </tr>');

      If bHasCaption Then
      Begin
        FText.Add('  <tr>');

        For i2 := 0 To oCaptions.Count - 1 Do
          FText.Add('      <td width=' + IntToStr(Trunc(100 / FImageColCount)) +
            '%>' + '<div style="text-align:center;font-size:9.0pt;font-family:Arial;">' +
            '<i><b>' + Validate(oCaptions[i2]) + '</b></i></div>' + '</td>');

        FText.Add('  </tr>');
      End;

      oCaptions.Clear;
    End;
  End;

  Procedure ExportImageFromCurrentRecord;
  Begin
    sFullFilename := oFilenameField.AsString;
    sCaption := Value(oDataset, sCaptionField, '');

    If (Trim(sFullFilename) <> '') Then
    Begin
      iNumFiles := WordCount(sFullFilename, [';']) + 1;
      iFile := 0;

      Repeat
        If (iNumFiles = 1) Then
          sTemp := sFullFilename
        Else
          sTemp := ExtractDelimited(iFile + 1, sFullFilename, [';']);

        // Some databases embed the images as BLOBS.  The routines here
        // are all file based
        // ValidateFile takes a Filename and checks with the host database
        // If files are loaded as Blobs, then Validate will download a Temp copy
        // and change the contents of the passed filename to include the filepath to the
        // Temp file
        If Assigned(FValidateFile) Then
          FValidateFile(Self, sTemp);

        sFilename := ChangeFileExt(ExtractFilename(FFilename), '') +
          '_files\' + ExtractFilename(sTemp);
        sNewFile := sFolder + '\' + ExtractFilename(sTemp);

        sExt := ExtractFileExt(sNewFile);

        If IsImage(sExt) Then
        Begin
          If FileExists(sTemp) Then
            CopyFile(sTemp, sNewFile)
          Else If FileExists(FImageDir + sTemp) Then
            CopyFile(FImageDir + sTemp, sNewFile);

          If FileExists(sNewFile) Then
            AddImageCell;
        End;

        Inc(iFile);
      Until iFile >= iNumFiles;
    End;
  End;

Begin
  oCaptions := TStringList.Create;
  Try
    oFilenameField := oDataset.FindField('Filename');
    If Not assigned(oFilenameField) Then
      oFilenameField := oDataset.FindField('Filename_ID');

    If Assigned(oFilenameField) Then
    Begin
      sFolder := ChangeFileExt(FFilename, '') + '_files';
      ForceDirectories(sFolder);

      sStartTable := '<table class="StyleTable">';
      FText.Add(sStartTable);
      iCol := 0;

      sCaptionField := '';
      bHasCaption := (oDataset.FindField('Caption') <> nil);
      If bHasCaption Then
        sCaptionField := 'Caption'
      Else
      Begin
        bHasCaption := (oDataset.FindField('Description') <> nil);
        If bHasCaption Then
          sCaptionField := 'Description';
      End;

      If bOnlyActiveRow Then
        ExportImageFromCurrentRecord
      Else
      Begin
        oBookmark := oDataset.GetBookmark;
        oDataset.DisableControls;
        Try
          oDataset.First;

          While Not oDataset.EOF Do
          Begin
            ExportImageFromCurrentRecord;

            oDataset.Next;
          End;
        Finally
          oDataset.GotoBookmark(oBookmark);
          oDataset.FreeBookmark(oBookmark);
          oDataset.EnableControls;
        End;
      End;

      // Insert the last row, which will be a mix of populated and non-populated cells
      If iCol <> 0 Then
      Begin
        For i := iCol To FImageColCount - 1 Do
          FText.Add('    <td>&nbsp;</td>');

        FText.Add('  </tr>');

        If bHasCaption Then
        Begin
          FText.Add('  <tr>');

          For i := 0 To oCaptions.Count - 1 Do
            FText.Add('      <td width=' + IntToStr(Trunc(100 / FImageColCount)) +
              '%>' + '<div style="text-align:center;font-size:9.0pt;font-family:Arial;">' +
              '<i><b>' + Validate(oCaptions[i]) + '</b></i></div>' + '</td>');

          For i := iCol To FImageColCount - 1 Do
            FText.Add('      <td>&nbsp;</td>');

          FText.Add('  </tr>');
        End;
      End;

      If FText[FText.Count - 1] = sStartTable Then
        FText.Delete(FText.Count - 1)
      Else
        FText.Add('</table>');
    End;
  Finally
    oCaptions.Free;
  End;
End;

Procedure THTMLExporter.PageBreak;
Begin
  Inherited PageBreak;

  // FText.Add('<br clear=all style=''mso-special-character:line-break;page-break-before:always''>');
End;

Procedure THTMLExporter.AppendTableRow(ADataset: TDataset);
Var
  oTemp: TStringList;
  i: Integer;
Begin
  // Ug.  Pointer hack to ensure Dataset operates on the correct StringList
  oTemp := FText;
  FText := FTable;
  Try
    // Take care of writing the header the lazy way :-)
    If FTable.Count = 0 Then
      Dataset(ADataset, teActiveAndHeader)
    Else
    Begin
      // In this section we're appending to the existing table
      // Means we don't any new <table tags>
      i := FTable.Count - 1;

      Dataset(ADataset, teActiveNoHeader);

      // Delete the extra <table> tag
      If i <> FText.Count Then
        FTable.Delete(i + 1);
    End;

    // Remove the trailing </table>
    FTable.Delete(FTable.Count - 1);
  Finally
    FText := oTemp;
  End;
End;

Initialization
  ExporterFactory := TExporterFactory.Create;

  ExporterFactory.RegisterExporter(TTextExporter);
  ExporterFactory.RegisterExporter(TCSVExporter);
  ExporterFactory.RegisterExporter(THTMLExporter);
  //TODO - Move to ExportersFPVectorial.pas
  //ExporterFactory.RegisterExporter(TDocumentExporter);

Finalization;
  ExporterFactory.Free;
End.
