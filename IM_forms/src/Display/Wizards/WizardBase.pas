Unit WizardBase;

{$mode objfpc}{$H+}

{-------------------------------------------------------------------------------
  Package   : IM_forms
  Unit      : WizardBase.pas
  Description
    Base form for Wizard dialogs

  Source
    Copyright (c) 2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    ~2019: Creation date unknown, original local SVN repository lost
    2019:  Uploaded to SourceForge as part of InspectionSQLReporters
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

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons, ComCtrls,
  StdCtrls;

Type

  { TwizBase }

  TwizBase = Class(TForm)
    btnClose: TBitBtn;
    btnNext: TBitBtn;
    btnPrevious: TBitBtn;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    memDescription: TMemo;
    memSummary: TMemo;
    pcMain: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    tsFirst: TTabSheet;
    tsLast: TTabSheet;
    Procedure btnNextClick(Sender: TObject);
    Procedure btnPreviousClick(Sender: TObject);
    Procedure pcMainChange(Sender: TObject);
  Protected
    Procedure RefreshUI; Virtual;
    Procedure OnChangePage; Virtual;
  Public
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;
  End;

Implementation

{$R *.lfm}

{ TwizBase }

Constructor TwizBase.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  pcMain.ShowTabs := False;
  pcMain.ActivePageIndex := 0;

  btnNext.OnClick := @btnNextClick;
  btnPrevious.OnClick := @btnPreviousClick;

  RefreshUI;

  // When adding pages in the descendants, please leave First and last pages
  // as PageIndex 0 & 1.
  // INstead we will simply move the Last Page to the end here
  tsLast.PageIndex := pcMain.PageCount - 1;
End;

Destructor TwizBase.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TwizBase.pcMainChange(Sender: TObject);
Begin
  RefreshUI;
End;

Procedure TwizBase.btnPreviousClick(Sender: TObject);
Begin
  If pcMain.ActivePageIndex <> 0 Then
    pcMain.ActivePageIndex := pcMain.ActivePageIndex - 1;

  OnChangePage;

  RefreshUI;
End;

Procedure TwizBase.btnNextClick(Sender: TObject);
Begin
  If pcMain.ActivePageIndex <> pcMain.PageCount - 1 Then
    pcMain.ActivePageIndex := pcMain.ActivePageIndex + 1;

  If (pcMain.ActivePageIndex = pcMain.PageCount - 1) Then
  Begin
    btnPrevious.Visible := False;
    btnNext.Visible := False;

    btnClose.Kind := bkClose;
  End;

  OnChangePage;

  RefreshUI;
End;

Procedure TwizBase.RefreshUI;
Begin
  btnPrevious.Enabled := pcMain.ActivePageIndex <> 0;
  btnNext.Enabled := True;
End;

Procedure TwizBase.OnChangePage;
Begin

End;

End.

