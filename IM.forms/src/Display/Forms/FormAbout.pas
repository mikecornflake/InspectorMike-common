Unit FormAbout;

{-------------------------------------------------------------------------------
  Package   : IM.forms
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
    2024-01-22: Migrated to Github.  Refactored package to "IM.application"
    2025-11-29: Added LGPL-3.0-or-later license header
    2026-06-19: Refactored into split InspectorMike package structure

  License
    This file is part of IM.forms.lpk.

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
  Classes, ComCtrls, Controls, Dialogs, ExtCtrls, FileUtil, SynEdit,
  SynHighlighterMarkdown, Forms, Graphics, StdCtrls, SysUtils, FrameAboutThirdParty;

Type

  { TfrmAbout }

  TfrmAbout = Class(TForm)
    Bevel1: TBevel;
    btnOK: TButton;
    edtAppExe: TEdit;
    ilTabs: TImageList;
    lblHTMLLabel2: TLabel;
    memLicence: TSynEdit;
    pnlTreeView: TPanel;
    pnlThirdParty: TPanel;
    pcAbout: TPageControl;
    imgAbout: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lblApplicationTitle: TLabel;
    lblHTMLLabel: TLabel;
    lblHTMLLabel1: TLabel;
    memAbout: TMemo;
    memReadme: TSynEdit;
    Splitter1: TSplitter;
    synMarkdown: TSynMarkdownSyn;
    tsThirdParty: TTabSheet;
    tsAbout: TTabSheet;
    tsReadme: TTabSheet;
    tsLicence: TTabSheet;
    tvThirdParty: TTreeView;
    Procedure btnOKClick(Sender: TObject);
    Procedure FormActivate(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure tvThirdPartySelectionChanged(Sender: TObject);
    Procedure URLLabelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure URLLabelMouseEnter(Sender: TObject);
    Procedure URLLabelMouseLeave(Sender: TObject);
  Protected
    FThirdPartyFrame: TFrameThirdParty;
    FActivated: Boolean;
  End;

Procedure ShowAbout;

Implementation

Uses
  LCLIntf,
  VersionSupport, OSSupport,
  ThirdPartySupport;

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

  Procedure BuildThirdPartyTree(ATree: TTreeView);
  Var
    arrKinds: Array[TThirdPartyKind] Of TTreeNode;
    oThirdParty: TThirdParty;
    oRootNode, oNode: TTreeNode;
    oKind: TThirdPartyKind;
  Begin
    FillChar(arrKinds, SizeOf(arrKinds), 0);

    ATree.Items.BeginUpdate;
    Try
      // First pass: create root nodes in sorted enum order
      For oKind := Low(TThirdPartyKind) To High(TThirdPartyKind) Do
      Begin
        For oThirdParty In ThirdParties Do
        Begin
          If oThirdParty.Used And (oThirdParty.Kind = oKind) Then
          Begin
            If arrKinds[oKind] = nil Then
            Begin
              oRootNode := ATree.Items.Add(nil, ThirdPartyKindNames[oKind]);
              arrKinds[oKind] := oRootNode;

              oRootNode.ImageIndex := 4 + Integer(oKind);
              oRootNode.SelectedIndex := 4 + Integer(oKind);
            End;
          End;
        End;
      End;

      // Second pass: add children under the already sorted roots
      For oThirdParty In ThirdParties Do
      Begin
        If oThirdParty.Used Then
        Begin
          oRootNode := arrKinds[oThirdParty.Kind];
          If oRootNode <> nil Then
          Begin
            oNode := ATree.Items.AddChild(oRootNode, oThirdParty.Name);
            oNode.Data := oThirdParty;
          End;
        End;
      End;

    Finally
      ATree.Items.EndUpdate;
    End;
  End;

Var
  oResourceStrings: TStringList;
  sFolder: String;
Begin
  Inherited;

  FActivated := False;

  // Register the attribuions for this TForm
  ThirdParties.Include([THIRDPARTY_FATCOW_ICONS]);

  sFolder := IncludeTrailingBackslash(ExtractFilePath(Application.ExeName));

  If FileExists(sFolder + 'AboutGraphic.png') Then
  Begin
    imgAbout.Picture.LoadFromFile(sFolder + 'AboutGraphic.png');
  End;

  edtAppExe.Text := Application.ExeName;

  BuildThirdPartyTree(tvThirdParty);
  tsThirdParty.TabVisible := (tvThirdParty.Items.Count > 0);

  FThirdPartyFrame := TFrameThirdParty.Create(pnlThirdParty);
  FThirdPartyFrame.Parent := pnlThirdParty;
  FThirdPartyFrame.Align := alClient;
  FThirdPartyFrame.Visible := False;

  If FileExists(sFolder + 'LICENSE.md') Then
  Begin
    tsLicence.TabVisible := True;
    memLicence.Lines.LoadFromFile(sFolder + 'LICENSE.md');
    memLicence.Highlighter := synMarkdown;
  End
  Else If FileExists(sFolder + 'LICENSE.txt') Then
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
    memReadme.Highlighter := nil;
    memReadme.Lines.LoadFromFile(sFolder + 'readme.txt');
  End
  Else If FileExists(sFolder + 'readme.md') Then
  Begin
    tsReadme.TabVisible := True;
    memReadme.Highlighter := synMarkdown;
    memReadme.Lines.LoadFromFile(sFolder + 'readme.md');
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

  {$IFDEF DEFAULT}
  memAbout.Lines.Add(' Build mode: Limited Debug');
  {$ENDIF}
  {$IFDEF DEBUG}
  memAbout.Lines.Add(' Build mode: Full Debug');
  {$ENDIF}
  {$IFDEF RELEASE}
  memAbout.Lines.Add(' Build mode: Release');
  {$ENDIF}


  If tsReadme.TabVisible Then
    pcAbout.ActivePage := tsReadme
  Else
    pcAbout.ActivePage := tsAbout;

  Caption := 'About:  ' + Application.Title;
  lblApplicationTitle.Caption := Application.Title;
End;

Procedure TfrmAbout.FormDestroy(Sender: TObject);
Begin
End;

Procedure TfrmAbout.tvThirdPartySelectionChanged(Sender: TObject);
Begin
  If Assigned(tvThirdParty.Selected) And Assigned(tvThirdParty.Selected.Data) Then
  Begin
    FThirdPartyFrame.Visible := True;
    FThirdPartyFrame.Populate(TThirdParty(tvThirdParty.Selected.Data));
  End
  Else
  Begin
    FThirdPartyFrame.Visible := False;

    If Assigned(tvThirdParty.Selected) Then
      tvThirdParty.Selected.Expand(True);
  End;
End;

Procedure TfrmAbout.btnOKClick(Sender: TObject);
Begin
  Close;
End;

Procedure TfrmAbout.FormActivate(Sender: TObject);
Var
  oNode: TTreeNode;
Begin
  If Not FActivated Then
  Begin
    tvThirdParty.FullExpand;

    oNode := tvThirdParty.Items.GetFirstNode;
    If Assigned(oNode) Then
    Begin
      tvThirdParty.Selected := oNode;
      oNode.MakeVisible;
    End;

    FActivated := True;
  End;
End;

Procedure TfrmAbout.URLLabelMouseLeave(Sender: TObject);
Begin
  TLabel(Sender).Font.Style := [];
  TLabel(Sender).Font.Color := clBlue;
  TLabel(Sender).Cursor := crDefault;
End;

Procedure TfrmAbout.URLLabelMouseEnter(Sender: TObject);
Begin
  TLabel(Sender).Font.Style := [fsUnderLine];
  TLabel(Sender).Font.Color := clRed;
  TLabel(Sender).Cursor := crHandPoint;
  TLabel(Sender).Hint := TLabel(Sender).Caption;
  TLabel(Sender).ShowHint := True;
End;

Procedure TfrmAbout.URLLabelMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  OpenURL(TLabel(Sender).Caption);
End;

End.
