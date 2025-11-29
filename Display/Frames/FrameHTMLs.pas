Unit FrameHTMLs;

{-------------------------------------------------------------------------------
  Package   : IM_application
  Unit      : FrameHTMLs.pas
  Description
    Frame for rendering HTML content with hyperlink notifications.

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

Uses IpHtmlNodes;  // leave in, even if compiler tells you this isn't used

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
