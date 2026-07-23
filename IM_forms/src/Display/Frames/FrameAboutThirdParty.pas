Unit FrameAboutThirdParty;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, ComCtrls, StdCtrls,
  SynHighlighterMarkdown, SynEdit, ThirdPartySupport;

Type

  { TFrameThirdParty }

  TFrameThirdParty = Class(TFrame)
    edtFolder: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lblCodeURL: TLabel;
    lblSummary: TLabel;
    lblProjectURL: TLabel;
    memReadme: TSynEdit;
    memLicense: TSynEdit;
    pcMain: TPageControl;
    synMarkdown: TSynMarkdownSyn;
    tsReadme: TTabSheet;
    tsLicense: TTabSheet;
    tsLinks: TTabSheet;

    Procedure URLLabelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure URLLabelMouseEnter(Sender: TObject);
    Procedure URLLabelMouseLeave(Sender: TObject);
  Public
    Procedure Populate(AThirdParty: TThirdParty);
  End;

Implementation

Uses
  LCLIntf, Graphics;

  {$R *.lfm}

Procedure TFrameThirdParty.URLLabelMouseLeave(Sender: TObject);
Begin
  TLabel(Sender).Font.Style := [];
  TLabel(Sender).Font.Color := clBlue;
  TLabel(Sender).Cursor := crDefault;
End;

Procedure TFrameThirdParty.Populate(AThirdParty: TThirdParty);
Begin
  lblProjectURL.Caption := AThirdParty.ProjectURL;
  lblCodeURL.Caption := AThirdParty.CodeURL;
  lblSummary.Caption := AThirdParty.Summary;
  edtFolder.Text := AThirdParty.Folder;

  If FileExists(AThirdParty.Readme) Then
  Begin
    If SameText(ExtractFileExt(AThirdParty.Readme), '.md') Then
      memReadme.Highlighter := synMarkdown
    Else
      memReadme.Highlighter := nil;

    memReadme.Lines.LoadFromFile(AThirdParty.Readme);
  End
  Else
    tsReadme.TabVisible := False;

  If FileExists(AThirdParty.License) Then
  Begin
    If SameText(ExtractFileExt(AThirdParty.License), '.md') Then
      memLicense.Highlighter := synMarkdown
    Else
      memLicense.Highlighter := nil;

    memLicense.Lines.LoadFromFile(AThirdParty.License);
  End
  Else
    tsLicense.TabVisible := False;
End;

Procedure TFrameThirdParty.URLLabelMouseEnter(Sender: TObject);
Begin
  TLabel(Sender).Font.Style := [fsUnderLine];
  TLabel(Sender).Font.Color := clRed;
  TLabel(Sender).Cursor := crHandPoint;
End;

Procedure TFrameThirdParty.URLLabelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  OpenURL(TLabel(Sender).Caption);
End;


End.
