Unit FormMultiDock;

{-------------------------------------------------------------------------------
  Package   : IM_application
  Unit      : FormMultiDock.pas
  Description
    Main dock host form managing tabbed dock pages.

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

Interface

Uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, Menus, ExtCtrls, Buttons, StdCtrls, FormMain, DockBase, DockManagers, Inifiles;

Type

  { TfrmMultiDock }

  TfrmMultiDock = Class(TfrmMain)
    ilDocks: TImageList;
    memDebug2: TMemo;
    pcMain: TPageControl;
    tbCategories: TToolBar;
    tsDebug: TTabSheet;
    Procedure pcMainChange(Sender: TObject);
    Procedure pcMainChanging(Sender: TObject; Var AllowChange: Boolean);
    Procedure ChangeCategoryClick(Sender: TObject);
  Private
    Function DockIndex(oDock: TdckBase): Integer;
    Function GetActiveDockIndex: Integer;
    Function GetActiveDock: TdckBase;
    Function GetActivePageControl: TPageControl;
    Function GetDock(iIndex: Integer): TdckBase;
  Protected
    FDocks: TList;  // TODO - think about moving this to private
    FCategories: TCategoryItems;

    // TODO Remove This (need to have dynamic PageControl functionality first)
    Function PageControl(iCategory: Integer): TPageControl;

    Procedure SetStatus(AValue: String); Override;

    Procedure DoDockChanged(oDock: TdckBase); Virtual;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure SetDebug(AValue: String);

    Procedure LoadGlobalSettings(oInifile: TIniFile); Override;
    Procedure SaveGlobalSettings(oInifile: TIniFile); Override;

    Procedure ShowDockManager;
    Procedure ShowDebug;
    Procedure RefreshCategories;

    Procedure FocusDock(oDock: TdckBase);
    Procedure AddDock(oDock: TdckBase; iCategory: Integer; iImageIndex: Integer = -1);
    Procedure MoveDock(oDockForm: TdckBase; iCategory: Integer);
    Procedure DeleteDock(oDock: TdckBase);

    Property Dock[iIndex: Integer]: TdckBase read GetDock;
    Property ActiveDock: TdckBase read GetActiveDock;
    Property ActiveDockIndex: Integer read GetActiveDockIndex;
    Property ActivePageControl: TPageControl read GetActivePageControl;
    Property Categories: TCategoryItems read FCategories;
  End;

Function MultiDockForm: TfrmMultiDock;

Implementation

Uses
  Math, DialogDockManager, LCLProc;

{$R *.lfm}

Function MultiDockForm: TfrmMultiDock;
Begin
  If Assigned(Application.MainForm) And (Application.MainForm Is TfrmMultiDock) Then
    Result := TfrmMultiDock(Application.MainForm)
  Else
    Raise Exception.Create(
      'FormMainBase.MainForm is only for apps where MainForm is a TfrmMain descendent');
End;

Constructor TfrmMultiDock.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);

  FDocks := TList.Create;

  FCategories := TCategoryItems.Create;

  pcMain.ActivePage := pcMain.Pages[0];
  pcMain.TabIndex := 0;
  pcMain.Visible := False;
End;

Destructor TfrmMultiDock.Destroy;
Begin
  While FDocks.Count > 0 Do
  Begin
    Dock[FDocks.Count - 1].Visible := False;

    Dock[FDocks.Count - 1].Free;
    FDocks.Delete(FDocks.Count - 1);
  End;

  FreeAndNil(FDocks);
  FreeAndNil(FCategories);

  Inherited Destroy;
End;

Procedure TfrmMultiDock.ChangeCategoryClick(Sender: TObject);
Var
  iCategory: Integer;
Begin
  Busy := True;
  Try
    iCategory := TControl(Sender).Tag;
    pcMain.ActivePage := TTabSheet(PageControl(iCategory).Parent);
  Finally
    Busy := False;
  End;
End;

Procedure TfrmMultiDock.pcMainChanging(Sender: TObject; Var AllowChange: Boolean);
Var
  oDock: TdckBase;
Begin
  If csDestroying In ComponentState Then
    Exit;

  If Not Assigned(FDocks) Then
    Exit;

  If Not FormIsUpdating Then
    BeginFormUpdate;

  oDock := ActiveDock;

  If pcMain.Visible And Assigned(oDock) Then
    oDock.Leave;
End;

Procedure TfrmMultiDock.pcMainChange(Sender: TObject);
Var
  i: Integer;
  bFound: Boolean;
  oPageControl: TPageControl;
Begin
  If csDestroying In ComponentState Then
    Exit;

  Status := '';
  Busy := True;
  Try
    i := 0;
    oPageControl := ActivePageControl;

    bFound := False;

    If Assigned(oPageControl) Then
    Begin
      //FocusButton(oPageControl.Tag);

      While Not bFound And (i < FDocks.Count) Do
      Begin
        bFound := (Dock[i].TabSheet = oPageControl.ActivePage);

        If Not bFound Then
          Inc(i);
      End;

      If bFound And pcMain.Visible Then
      Begin
        Dock[i].Enter;

        DoDockChanged(Dock[i]);
      End;
    End;
  Finally
    If FormIsUpdating Then
      EndFormUpdate;

    Busy := False;
  End;
End;

Procedure TfrmMultiDock.FocusDock(oDock: TdckBase);
Var
  oPageControl: TPageControl;
Begin
  oPageControl := PageControl(oDock.Category);

  //FocusButton(oDock.Category);

  oPageControl.ActivePage := oDock.Tabsheet;
  pcMain.ActivePage := TTabSheet(oPageControl.Parent);
End;

Function TfrmMultiDock.DockIndex(oDock: TdckBase): Integer;
Var
  i: Integer;
  bFound: Boolean;
Begin
  i := 0;
  bFound := False;

  Result := -1;

  While Not bFound And (i < FDocks.Count) Do
  Begin
    bFound := Dock[i] = oDock;

    If bFound Then
      Result := i
    Else
      Inc(i);
  End;
End;

Function TfrmMultiDock.GetActiveDockIndex: Integer;
Var
  i: Integer;
  bFound: Boolean;
Begin
  i := 0;
  bFound := False;

  Result := -1;

  While Not bFound And (i < FDocks.Count) Do
  Begin
    bFound := Dock[i].Active;

    If bFound Then
      Result := i
    Else
      Inc(i);
  End;
End;

Function TfrmMultiDock.GetActiveDock: TdckBase;
Var
  i: Integer;
Begin
  i := ActiveDockIndex;

  If i = -1 Then
    Result := nil
  Else
    Result := Dock[i];
End;

Function TfrmMultiDock.GetActivePageControl: TPageControl;
Begin
  Result := nil;

  If Assigned(pcMain) And Assigned(pcMain.ActivePage) Then
    If InRange(pcMain.ActivePage.Tag, 0, pcMain.PageCount - 1) Then
      Result := PageControl(pcMain.ActivePage.Tag);
End;

Function TfrmMultiDock.GetDock(iIndex: Integer): TdckBase;
Begin
  If (iIndex >= 0) And (iIndex < FDocks.Count) Then
    Result := TdckBase(FDocks[iIndex])
  Else
    Raise Exception.Create('Dock index out of bounds');
End;

Procedure TfrmMultiDock.AddDock(oDock: TdckBase; iCategory: Integer; iImageIndex: Integer);
Var
  oPageControl: TPageControl;
  oCategory: TCategoryItem;
Begin
  If (iCategory < 0) Or (iCategory > FCategories.Count - 1) Then
    Exit;

  oCategory := FCategories[iCategory];
  oPageControl := oCategory.PageControl;

  oDock.Category := iCategory;
  oDock.ManualDock(oPageControl);

  oDock.TabSheet := TTabSheet(oDock.Parent);
  oDock.TabSheet.Caption := oDock.Caption;
  oDock.TabSheet.TabVisible := True;
  oDock.TabSheet.PageIndex := oPageControl.PageCount - 1;
  oDock.TabSheet.ImageIndex := iImageIndex;
  oDock.TabSheet.Tag := iCategory;

  oDock.Show;

  FDocks.Add(oDock);

  With oCategory.Docks.Add Do
  Begin
    ImageIndex := iImageIndex;
    dckBase := oDock;
  End;

  //SetStatus(Format('FDock.Count=%d, FCategories.Count=%d, oCategory.Docks.Count=%d', [FDocks.Count, FCategories.Count, oCategory.Docks.Count]));
End;

Procedure TfrmMultiDock.DeleteDock(oDock: TdckBase);
Var
  oPageControl: TPageControl;
  iDock: Integer;
  oCategory: TCategoryItem;
  iDockItem: Integer;
Begin
  // Remove the dock from the FDocks list
  iDock := DockIndex(oDock);
  FDocks.Delete(iDock);

  // Remove the dock from the Categories list
  oCategory := FCategories[oDock.Category];
  iDockItem := oCategory.Docks.IndexOf(oDock);
  oCategory.Docks.Delete(iDockItem);

  // Ensure the correct TabPages are visible;

  // First ensure the correct Category PageControl is visible
  oPageControl := PageControl(oDock.Category);
  pcMain.ActivePage := TTabSheet(oPageControl.Parent);

  // Now ensure the active tab page ISN'T selected
  oPageControl.TabIndex := Max(0, oDock.TabSheet.TabIndex - 1);

  // Hide first to stop some flicker
  oDock.Hide;

  // Now free the dock and clear the reference..
  oDock.Tabsheet.Free;
  oDock.Free;
End;

Procedure TfrmMultiDock.SetDebug(AValue: String);
Begin
  DebugLn(FormatDateTime('HH:mm:ss', Now) + FIndent + AValue);
  memDebug2.Lines.Add(FormatDateTime('HH:mm:ss', Now) + FIndent + AValue);
End;

Procedure TfrmMultiDock.LoadGlobalSettings(oInifile: TIniFile);
Begin
  Inherited LoadGlobalSettings(oInifile);

  FCategories.LoadSettings(oInifile);
End;

Procedure TfrmMultiDock.SaveGlobalSettings(oInifile: TIniFile);
Begin
  Inherited SaveGlobalSettings(oInifile);

  FCategories.SaveSettings(oInifile);
End;

Procedure TfrmMultiDock.SetStatus(AValue: String);
Begin
  Inherited SetStatus(AValue);

  If (Trim(AValue) <> '') Then
    SetDebug(AValue);
End;

Procedure TfrmMultiDock.DoDockChanged(oDock: TdckBase);
Begin
  // Virtual...
End;

Function TfrmMultiDock.PageControl(iCategory: Integer): TPageControl;
Begin
  Result := FCategories[iCategory].PageControl;
End;

Procedure TfrmMultiDock.ShowDockManager;
Var
  oDlgDockManager: TdlgDockManager;

Begin
  oDlgDockManager := TdlgDockManager.Create(Self);
  oDlgDockManager.Categories.Assign(FCategories);
  Try
    If oDlgDockManager.ShowModal = mrOk Then
    Begin
      FCategories.Assign(oDlgDockManager.Categories);

      // Update Toolbars and assorted PageControls
      RefreshCategories;
    End;
  Finally
    oDlgDockManager.Free;
  End;
End;

Procedure TfrmMultiDock.ShowDebug;
Var
  i: Integer;
Begin
  For i := 0 To tbCategories.ButtonCount - 1 Do
    tbCategories.Buttons[i].Down := False;

  pcMain.ActivePage := tsDebug;
End;

Procedure TfrmMultiDock.RefreshCategories;
Var
  iCat: Integer;
  iDock: Integer;
  iPrevButton: Integer;
  oButton: TToolButton;
  oCategory: TCategoryItem;
  oDockItem: TDockItem;
  oPageControl: TPageControl;
  oTabSheet: TTabSheet;
  bInitial, bHasConfigure: Boolean;
  iPageControl: Integer;

  Function FindPageControl(ATabSheet: TTabSheet): TPageControl;
  Var
    i: Integer;
  Begin
    Result := nil;

    For i := 0 To FCategories.Count - 1 Do
      If TTabSheet(FCategories[i].PageControl.Parent) = ATabSheet Then
      Begin
        Result := FCategories[i].PageControl;
        Exit;
      End;
  End;

Begin
  // This code is called both from LoadSettings and after DialogDockManager is used
  bHasConfigure := Application.HasOption('configure');

  BeginFormUpdate;
  Try
    bInitial := tbCategories.ButtonCount = 0;

    // Clear all buttons from the toolbar..
    For iCat := tbCategories.ButtonCount - 1 Downto 0 Do
      tbCategories.Buttons[iCat].Free;

    For iCat := 0 To FCategories.Count - 1 Do
    Begin
      oCategory := FCategories[iCat];

      // Add the appropriate Category Button to the Toolbar
      oButton := TToolButton.Create(tbCategories);
      oButton.Caption := oCategory.Name;
      oButton.ImageIndex := oCategory.ImageIndex;
      oButton.Style := tbsCheck;
      oButton.Grouped := True;
      oButton.AllowAllUp := True;
      oButton.Tag := iCat;
      oButton.OnClick := @ChangeCategoryClick;
      oButton.Enabled := tbCategories.Enabled;

      // Currently no way to edit this in the UI - hand hack ini only
      If oCategory.Administrator Then
        oButton.Visible := bHasConfigure;

      iPrevButton := tbCategories.ButtonCount - 1;
      If iPrevButton > -1 Then
        oButton.Left := tbCategories.Buttons[iPrevButton].Left +
          tbCategories.Buttons[iPrevButton].Width
      Else
        oButton.Left := 0;
      oButton.Parent := tbCategories;

      // Ensure the PageControls are created, one per category
      oPageControl := oCategory.PageControl;

      If Not Assigned(oPageControl) Then
      Begin
        oPageControl := TPageControl.Create(Self);
        oPageControl.Options := [nboDoChangeOnSetIndex];
        oPageControl.ManualDock(pcMain);
        oPageControl.Images := ilImages;
        oPageControl.OnChange := @pcMainChange;
        oPageControl.OnChanging := @pcMainChanging;

        oCategory.PageControl := oPageControl;
      End;

      oPageControl.Tag := iCat;

      oTabSheet := TTabSheet(oPageControl.Parent);
      oTabSheet.Caption := oCategory.Name + IntToStr(iCat);
      oTabSheet.PageIndex := iCat;
      oTabSheet.Tag := iCat;

      // Now ensure all the correct Docks are in the correct Page Control (note: during LoadSettings,
      // none of the docks will have been created yet, so this code is ONLY for the scenario of the
      // user using DialogDockManager
      For iDock := 0 To oCategory.Docks.Count - 1 Do
      Begin
        oDockItem := oCategory.Docks[iDock];
        If assigned(oDockItem.dckBase) Then
        Begin
          If oDockItem.dckBase.Tabsheet.PageControl <> oPageControl Then
          Begin
            oDockItem.dckBase.ManualDock(oPageControl);
            oDockItem.dckBase.TabSheet := TTabSheet(oDockItem.dckBase.Parent);
          End;

          oDockItem.dckBase.TabSheet.Tag := iDock;
          oDockItem.dckBase.TabSheet.PageIndex := iDock;
          oDockItem.dckBase.TabSheet.ImageIndex := oDockItem.ImageIndex;
          oDockItem.dckBase.Category := iCat;
          oDockItem.dckBase.ImageIndex := oDockItem.ImageIndex;
        End;
      End;
    End;

    // Now ensure that any old PageControls are deleted
    For iPageControl := 0 To pcMain.PageCount - 1 Do
    Begin
      oPageControl := FindPageControl(pcMain.Pages[iPageControl]);
      If Not assigned(oPageControl) Then
        If pcMain.Pages[iPageControl] <> tsDebug Then
          pcMain.Pages[iPageControl].Free;
    End;

    If (tbCategories.ButtonCount > 0) Then
      If bInitial Or Not Assigned(pcMain.ActivePage) Then
      Begin
        tbCategories.Buttons[0].Down := True;
        tbCategories.Buttons[0].Click;
      End
      Else
      If Assigned(pcMain.ActivePage) And (pcMain.ActivePage <> tsDebug) Then
      Begin
        tbCategories.Buttons[pcMain.ActivePage.TabIndex].Down := True;
        tbCategories.Buttons[pcMain.ActivePage.TabIndex].Click;
      End;
  Finally
    EndFormUpdate;
  End;
End;

Procedure TfrmMultiDock.MoveDock(oDockForm: TdckBase; iCategory: Integer);
Var
  oPageControl: TPageControl;
Begin
  // TODO - If iCategory changed, update FCategories...
  oPageControl := PageControl(iCategory);

  oDockForm.Category := iCategory;
  oDockForm.ManualDock(oPageControl);
  oDockForm.TabSheet := TTabSheet(oDockForm.Parent);

  FocusDock(oDockForm);
End;

Initialization

End.
