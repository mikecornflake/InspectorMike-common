Unit FormAbout;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, ComCtrls,
  Controls, Dialogs, ExtCtrls, FileUtil, Forms, Graphics, StdCtrls, SysUtils,
  FrameHTMLs;

Type

  { TfrmAbout }

  TfrmAbout = Class(TForm)
    Bevel1: TBevel;
    btnOK: TButton;
    Label3: TLabel;
    Label4: TLabel;
    lblSDKs: TLabel;
    lblImageMagick: TLabel;
    lblXPDF1: TLabel;
    lblXPDF2: TLabel;
    lblHTMLLabel6: TLabel;
    lblHTMLLabel7: TLabel;
    lblXPDF3: TLabel;
    memImageMagick: TMemo;
    memXPDF: TMemo;
    memReadme: TMemo;
    pcAbout: TPageControl;
    imgAbout: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lblApplicationTitle: TLabel;
    lblHTMLLabel: TLabel;
    lblHTMLLabel1: TLabel;
    memLicence: TMemo;
    memAbout: TMemo;
    tsFFMPEG: TTabSheet;
    tsImageMagick: TTabSheet;
    tsXPDF: TTabSheet;
    tsCredits: TTabSheet;
    tsAbout: TTabSheet;
    tsReadme: TTabSheet;
    tsLicence: TTabSheet;
    Procedure btnOKClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure URLLabelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure URLLabelMouseEnter(Sender: TObject);
    Procedure URLLabelMouseLeave(Sender: TObject);
  Protected
    fmeFFmpeg: TFrameHTML;

    Procedure HTMLHyperlink(Sender: TObject; AHREF: String);
  Public
  End;

Procedure ShowAbout;

Implementation

Uses
  LCLIntf, VersionSupport, XPDFSupport, ImageMagickSupport, ffmpegSupport, OSSupport;

{$R *.lfm}

Procedure ShowAbout;
Var
  oAbout: TfrmAbout;
Begin
  oAbout := TfrmAbout.Create(Application.MainForm);
  Try
    oAbout.ShowModal;
  Finally
    oAbout.Free;
  End;
End;

Procedure TfrmAbout.FormCreate(Sender: TObject);
Var
  oResourceStrings: TStringList;
  sFolder: String;
Begin
  Inherited;

  sFolder := IncludeTrailingBackslash(ExtractFilePath(Application.ExeName));
  pcAbout.ActivePage := tsAbout;

  If FileExists(sFolder + 'licence.txt') Then
  Begin
    tsLicence.TabVisible := True;
    memLicence.Lines.LoadFromFile(sFolder + 'licence.txt');
  End;

  If FileExists(sFolder + 'readme.txt') Then
  Begin
    tsReadme.TabVisible := True;
    memReadme.Lines.LoadFromFile(sFolder + 'readme.txt');
  End;

  If (ImageMagickAvailable) And (FileExists(ImageMagickPath + '\License.txt')) Then
  Begin
    tsImageMagick.TabVisible := True;
    memImageMagick.Lines.LoadFromFile(ImageMagickPath + '\License.txt');
    lblImageMagick.Visible := True;
  End
  Else
    tsImageMagick.TabVisible := False;

  If (XPDFAvailable) And (FileExists(XPDFPath + '\..\README')) Then
  Begin
    tsXPDF.TabVisible := True;
    memXPDF.Lines.LoadFromFile(XPDFPath + '\..\README');
    lblXPDF1.Visible := True;
    lblXPDF2.Visible := True;
  End
  Else
    tsXPDF.TabVisible := False;

  If (XPDFAvailable) And (FileExists(XPDFPath + '\..\README')) Then
  Begin
    tsXPDF.TabVisible := True;
    memXPDF.Lines.LoadFromFile(XPDFPath + '\..\README');
    lblXPDF1.Visible := True;
    lblXPDF2.Visible := True;
  End
  Else
    tsXPDF.TabVisible := False;

  If (FFmpegAvailable) Then
  Begin
    tsFFMPEG.TabVisible := True;
    fmeFFmpeg := TFrameHTML.Create(Self);
    fmeFFmpeg.Parent := tsFFMPEG;
    fmeFFmpeg.Name := 'fmeFFmpeg';
    fmeFFmpeg.Align := alClient;
    fmeFFmpeg.Visible := True;

    fmeFFmpeg.OnHyperlink := @HTMLHyperlink;

    fmeFFmpeg.SetHTMLAsString(FFmpegHelpAboutBlurb);
  End
  Else
  Begin
    tsFFMPEG.TabVisible := False;
    fmeFFmpeg := nil;
  End;

  lblSDKs.Visible := lblImageMagick.Visible Or lblXPDF1.Visible;

  memAbout.Lines.Clear;
  memAbout.Lines.Add(Application.exename);
  memAbout.Lines.Add('');
  oResourceStrings := TStringList.Create;
  Try
    GetResourceStrings(oResourceStrings);

    memAbout.Lines.AddStrings(oResourceStrings);

    If oResourceStrings.Count > 0 Then
      memAbout.Lines.Add('');
  Finally
    oResourceStrings.Free;
  End;

  memAbout.Lines.Add('Built for ' + GetTargetInfo);
  memAbout.Lines.Add(' with ' + GetCompilerInfo + ' on ' + GetCompiledDate);
  memAbout.Lines.Add(' and using ' + GetLCLVersion + ' and ' + GetWidgetset);

  lblApplicationTitle.Caption := Application.Title;
End;

Procedure TfrmAbout.FormDestroy(Sender: TObject);
Begin
  If Assigned(fmeFFmpeg) Then
    FreeAndNil(fmeFFmpeg);
End;

Procedure TfrmAbout.btnOKClick(Sender: TObject);
Begin
  Close;
End;

Procedure TfrmAbout.URLLabelMouseLeave(Sender: TObject);
Begin
  TLabel(Sender).Font.Style := [];
  TLabel(Sender).Font.Color := clBlue;
  TLabel(Sender).Cursor := crDefault;
End;

Procedure TfrmAbout.HTMLHyperlink(Sender: TObject; AHREF: String);
Begin
  If AHREF = 'FFMPEGLibraryLicenses' Then
    LaunchFile('explorer.exe', Format('/e,"%s"', [FFmpegPath + '\..\licenses']))
  Else
    OpenURL(AHREF);
End;

Procedure TfrmAbout.URLLabelMouseEnter(Sender: TObject);
Begin
  TLabel(Sender).Font.Style := [fsUnderLine];
  TLabel(Sender).Font.Color := clRed;
  TLabel(Sender).Cursor := crHandPoint;
End;

Procedure TfrmAbout.URLLabelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  OpenURL(TLabel(Sender).Caption);
End;

End.
