Unit FrameHTMLs;

{$mode objfpc}{$H+}

(*
  Portions of this code were derived from the example code found in
     lazarus\examples\turbopower_ipro\mainform.pas

  Modifications were either to allow for FOnHyperlink or to fit
  my personal coding conventions
*)
Interface

Uses
  Classes, SysUtils, FileUtil, IpHtml, Ipfilebroker, LResources, Forms, Controls,
  Graphics, Dialogs, FrameBase;

Type
  THyperlinkEvent = Procedure(Sender: TObject; AHREF: String) Of Object;

  { TFrameHTML }

  TFrameHTML = Class(TfmeBase)
    htmlPanel: TIpHtmlPanel;
    Procedure htmlPanelHotClick(Sender: TObject);
  Private
    FOnHyperlink: THyperlinkEvent;

    // Used by LoadFromStream
    Procedure HTMLGetImageX(Sender: TIpHtmlNode; Const URL: String; Var Picture: TPicture);

    Procedure LoadFromStream(AStream: TStream);
  Public
    Constructor Create(TheOwner: TComponent); Override;

    // Simple pass-throughs
    Procedure PrintPreview;
    Procedure SelectAll;
    Procedure CopyToClipboard;

    // Simple use functions
    Procedure OpenHTMLFile(Const AFilename: String);
    Procedure SetHTMLAsString(AHTML: String);

    // If you don't want hyperlinks to be active, then assign
    // a handler here, and do nothing in your handler
    Property OnHyperlink: THyperlinkEvent read FOnHyperlink write FOnHyperlink;
  End;

Implementation

{$R *.lfm}

//Uses IpHtmlNodes;

Constructor TFrameHTML.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  FOnHyperlink := nil;
End;

Procedure TFrameHTML.PrintPreview;
Begin
  //htmlPanel.PrintPreview;
End;

Procedure TFrameHTML.SelectAll;
Begin
  htmlPanel.SelectAll;
End;

Procedure TFrameHTML.CopyToClipboard;
Begin
  htmlPanel.CopyToClipboard;
End;

Procedure TFrameHTML.htmlPanelHotClick(Sender: TObject);
Var
  oNode: TIpHtmlNodeA;
Begin
  If htmlPanel.HotNode Is TIpHtmlNodeA Then
  Begin
    oNode := TIpHtmlNodeA(htmlPanel.HotNode);

    If Assigned(FOnHyperlink) Then
      FOnHyperlink(Self, oNode.HREF)
    Else
      OpenHTMLFile(oNode.HREF);
  End;
End;

Procedure TFrameHTML.HTMLGetImageX(Sender: TIpHtmlNode; Const URL: String; Var Picture: TPicture);
Var
  bCreated: Boolean;
Begin
  Try
    If FileExists(URL) Then
    Begin
      bCreated := False;
      If Picture = nil Then
      Begin
        Picture := TPicture.Create;
        bCreated := True;
      End;
      Picture.LoadFromFile(URL);
    End;
  Except
    If bCreated Then
      Picture.Free;
    Picture := nil;
  End;
End;

Type
  // Expose the OnGetImageX property
  THackIpHtml = Class(TIpHtml);

Procedure TFrameHTML.LoadFromStream(AStream: TStream);
Var
  oHTML: THackIpHtml;
Begin
  Try
    oHTML := THackIpHtml.Create; // Beware: Will be freed automatically by htmlPanel
    oHTML.OnGetImageX := @HTMLGetImageX;

    AStream.Position := 0;
    oHTML.LoadFromStream(AStream);

    AStream.Position := 0;

    htmlPanel.SetHtml(oHTML);
  Except
    On E: Exception Do
      MessageDlg('Unable to open HTML' + #13 + 'Error: ' + E.Message,
        mtError, [mbCancel], 0);
  End;
End;

Procedure TFrameHTML.SetHTMLAsString(AHTML: String);
Var
  oStream: TStringStream;
Begin
  oStream := TStringStream.Create(AHTML);
  Try
    LoadFromStream(oStream);
  Finally
    oStream.Free;
  End;
End;

Procedure TFrameHTML.OpenHTMLFile(Const AFilename: String);
Var
  oStream: TFileStream;
Begin
  oStream := TFileStream.Create(AFilename, fmOpenRead);
  Try
    LoadFromStream(oStream);
  Finally
    oStream.Free;
  End;
End;

Initialization

End.
