Unit FormAbout;

{-------------------------------------------------------------------------------
  Package   : IM_forms
  Unit      : FormAbout.pas
  Description
    About dialog presenting application details.

  Source
    Copyright (c) 2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    ~2008: Creation date unknown, original local SVN repository lost
    2014-07-05: Uploaded to SourceForge/Package "Shared"
    2024-01-22: Migrated to Github.  Refactored package to "IM_application"
    2025-11-29: Added LGPL-3.0-or-later license header
    2026-06-19: Refactored into split InspectorMike package structure

  License
    This file is part of IM_forms.lpk.

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
  Classes, ComCtrls,
  Controls, Dialogs, ExtCtrls, FileUtil, Forms, Graphics, StdCtrls, SysUtils,
  FrameHTMLs;

Type

  { TfrmAbout }

  TfrmAbout = Class(TForm)
    Bevel1: TBevel;
    btnOK: TButton;
    edtqpdfDir: TEdit;
    edtmpvDLL: TEdit;
    edtImageMagickDir: TEdit;
    edtXPDFDir: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    lblHTMLLabel2: TLabel;
    lblHTMLLabel3: TLabel;
    lblImageMagickURL: TLabel;
    lblqpdfURL: TLabel;
    lblXPDF2: TLabel;
    lblXPDFURL: TLabel;
    lblSDKs: TLabel;
    lblHTMLLabel6: TLabel;
    lblHTMLLabel7: TLabel;
    lblXPDF3: TLabel;
    memImageMagick: TMemo;
    memqdfp: TMemo;
    memMPV: TMemo;
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
    tsQPDF: TTabSheet;
    tsMPV: TTabSheet;
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
    procedure lblImageMagickClick(Sender: TObject);
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
  LCLIntf, VersionSupport,
  XPDFSupport, ImageMagickSupport, ffmpegSupport, OSSupport, LibmpvSupport, qpdfSupport;

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

  If FileExists(sFolder + 'AboutGraphic.png') Then
  Begin
    imgAbout.Picture.LoadFromFile(sFolder + 'AboutGraphic.png');
  End;

  If FileExists(sFolder + 'LICENSE.txt') Then
  Begin
    tsLicence.TabVisible := True;
    memLicence.Lines.LoadFromFile(sFolder + 'LICENSE.txt');
  End
  Else If FileExists(sFolder + 'LICENSE') Then
  Begin
    tsLicence.TabVisible := True;
    memLicence.Lines.LoadFromFile(sFolder + 'LICENSE');
  End;

  If FileExists(sFolder + 'readme.txt') Then
  Begin
    tsReadme.TabVisible := True;
    memReadme.Lines.LoadFromFile(sFolder + 'readme.txt');
  End
  Else If FileExists(sFolder + 'readme.md') Then
  Begin
    tsReadme.TabVisible := True;
    memReadme.Lines.LoadFromFile(sFolder + 'readme.md');
  End;

  If (ImageMagickAvailable) And (FileExists(ImageMagickPath + '\License.txt')) Then
  Begin
    tsImageMagick.TabVisible := True;
    memImageMagick.Lines.LoadFromFile(ImageMagickPath + '\License.txt');
    edtImageMagickDir.Text := ImageMagickPath;
  End
  Else
    tsImageMagick.TabVisible := False;

  If (XPDFAvailable) And (FileExists(XPDFPath + '\..\README')) Then
  Begin
    tsXPDF.TabVisible := True;
    memXPDF.Lines.LoadFromFile(XPDFPath + '\..\README');
    edtXPDFDir.Text := XPDFPath;
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

  If qpdfAvailable Then
  Begin
    tsqPDF.TabVisible := True;

    If FileExists(qpdfExe) Then
      edtqpdfDir.Text := qpdfExe;
  end
  Else
      tsqPDF.TabVisible := False;

  tsMPV.TabVisible := LibmpvAvailable;
  If tsMPV.TabVisible Then
  Begin
    edtmpvDLL.Text := LibmpvDLL;
  End;

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

procedure TfrmAbout.lblImageMagickClick(Sender: TObject);
begin

end;

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
