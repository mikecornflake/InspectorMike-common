Unit DialogDockManager;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ButtonPanel, StdCtrls, Buttons, types, DockManagers;

Type

  { TdlgDockManager }

  TdlgDockManager = Class(TForm)
    btnCatDown: TSpeedButton;
    btnCatDelete: TButton;
    btnDockDown: TSpeedButton;
    btnCatUp: TSpeedButton;
    btnCatNew: TButton;
    btnDockUp: TSpeedButton;
    ButtonPanel1: TButtonPanel;
    cboCatImages: TComboBox;
    cboDockImages: TComboBox;
    cboCategory: TComboBox;
    edtName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lbCategories: TListBox;
    lbDocks: TListBox;
    pnlDocks: TPanel;
    pnlLeft: TPanel;
    pnlRight: TPanel;
    Splitter1: TSplitter;
    Procedure btnCatDeleteClick(Sender: TObject);
    Procedure btnCatDownClick(Sender: TObject);
    Procedure btnCatNewClick(Sender: TObject);
    Procedure btnCatUpClick(Sender: TObject);
    Procedure btnDockDownClick(Sender: TObject);
    Procedure btnDockUpClick(Sender: TObject);
    Procedure cboCategoryChange(Sender: TObject);
    Procedure cboCatImagesChange(Sender: TObject);
    Procedure cboCatImagesDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    Procedure cboDockImagesChange(Sender: TObject);

    Procedure cboDockImagesDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    Procedure edtNameChange(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure lbCategoriesSelectionChange(Sender: TObject; User: Boolean);
    Procedure lbDocksSelectionChange(Sender: TObject; User: Boolean);
  Private
    FCategories: TCategoryItems;
    Procedure RefreshCategoryButtons;
    Procedure RefreshDockButtons;
  Public
    Property Categories: TCategoryItems read FCategories;
  End;

Var
  dlgDockManager: TdlgDockManager;

Implementation

{$R *.lfm}

Uses
  FormMultiDock;

{ TdlgDockManager }

Procedure TdlgDockManager.FormCreate(Sender: TObject);
Var
  i: Integer;
Begin
  // Our own local copy of the Categories (so we ignore choices if user cancels)
  FCategories := TCategoryItems.Create;

  // Configure the Category ComboBox for ImageList choice...
  cboCatImages.Style := csOwnerDrawFixed;
  cboCatImages.ItemHeight := TfrmMultiDock(Application.MainForm).ilDocks.Height + 2;
  cboCatImages.ReadOnly := True;

  cboCatImages.Items.Clear;
  For i := 0 To TfrmMultiDock(Application.MainForm).ilDocks.Count - 1 Do
    cboCatImages.Items.Add(IntToStr(i));

  cboCatImages.ItemIndex := -1;

  // Configure the Dock ComboBox for ImageList choice...
  cboDockImages.Style := csOwnerDrawFixed;
  cboDockImages.ItemHeight := TfrmMultiDock(Application.MainForm).ilImages.Height + 2;
  cboDockImages.ReadOnly := True;

  cboDockImages.Items.Clear;
  For i := 0 To TfrmMultiDock(Application.MainForm).ilImages.Count - 1 Do
    cboDockImages.Items.Add(IntToStr(i));

  cboDockImages.ItemIndex := -1;
End;

Procedure TdlgDockManager.FormDestroy(Sender: TObject);
Begin
  FreeAndNil(FCategories);
End;

Procedure TdlgDockManager.FormShow(Sender: TObject);
Var
  i: Integer;
Begin
  For i := 0 To FCategories.Count - 1 Do
  Begin
    lbCategories.Items.Add(FCategories[i].Name);
    cboCategory.Items.Add(FCategories[i].Name);
  End;
End;

Procedure TdlgDockManager.cboCatImagesDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
Begin
  cboCatImages.Canvas.FillRect(ARect);

  // Draw the Text for each item
  //cboCatImages.Canvas.TextOut(ARect.Left + 3, ARect.Top + 5, cboCatImages.Items[Index]);

  // Draw the Image for each item
  If Index >= 0 Then
    TfrmMultiDock(Application.MainForm).ilDocks.Draw(cboCatImages.Canvas,
      ARect.Left + 5, ARect.Top + 1, Index);
End;

Procedure TdlgDockManager.cboDockImagesChange(Sender: TObject);
Begin
  If (lbCategories.ItemIndex <> -1) And (lbDocks.ItemIndex <> -1) And (cboDockImages.Focused) Then
    FCategories[lbCategories.ItemIndex].Docks[lbDocks.ItemIndex].ImageIndex :=
      cboDockImages.ItemIndex;
End;

Procedure TdlgDockManager.cboCategoryChange(Sender: TObject);
Var
  oDockItem: TDockItem;
  oNewDockItem: TDockItem;
Begin
  If (lbCategories.ItemIndex <> -1) And (lbDocks.ItemIndex <> -1) And
    (lbCategories.ItemIndex <> cboCategory.ItemIndex) And (cboCategory.Focused) Then
  Begin
    // Move the current TDock to the new Category
    oDockItem := FCategories[lbCategories.ItemIndex].Docks[lbDocks.ItemIndex];
    oNewDockItem := FCategories[cboCategory.ItemIndex].Docks.Add;
    oNewDockItem.Assign(oDockItem);
    FCategories[lbCategories.ItemIndex].Docks.Delete(lbDocks.ItemIndex);

    // Now refresh the UI
    lbCategories.ItemIndex := cboCategory.ItemIndex;
    lbDocks.ItemIndex := lbDocks.Items.Count - 1;
  End;
End;

Procedure TdlgDockManager.cboDockImagesDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
Begin
  cboDockImages.Canvas.FillRect(ARect);

  // Draw the Text for each item
  //cboDockImages.Canvas.TextOut(ARect.Left + 3, ARect.Top + 5, cboDockImages.Items[Index]);

  // Draw the Image for each item
  If Index >= 0 Then
    TfrmMultiDock(Application.MainForm).ilImages.Draw(cboDockImages.Canvas,
      ARect.Left + 5, ARect.Top + 1, Index);
End;

Procedure TdlgDockManager.edtNameChange(Sender: TObject);
Var
  i: Integer;
Begin
  If (lbCategories.ItemIndex <> -1) And edtName.Focused Then
  Begin
    i := lbCategories.ItemIndex;
    lbCategories.Items[i] := edtName.Text;
    FCategories[i].Name := edtName.Text;
  End;
End;

Procedure TdlgDockManager.cboCatImagesChange(Sender: TObject);
Begin
  If (lbCategories.ItemIndex <> -1) And cboCatImages.Focused Then
    FCategories[lbCategories.ItemIndex].ImageIndex := cboCatImages.ItemIndex;
End;

Procedure TdlgDockManager.btnCatUpClick(Sender: TObject);
Var
  i: Integer;
Begin
  i := lbCategories.ItemIndex;
  If (i > 0) Then
  Begin
    FCategories.Exchange(i, i - 1);
    lbCategories.Items.Exchange(i, i - 1);

    lbCategories.ItemIndex := i - 1;

    RefreshCategoryButtons;
  End;
End;

Procedure TdlgDockManager.btnCatDownClick(Sender: TObject);
Var
  i: Integer;
Begin
  i := lbCategories.ItemIndex;
  If (i >= 0) And (i < lbCategories.Items.Count - 1) Then
  Begin
    i := lbCategories.ItemIndex;
    FCategories.Exchange(i, i + 1);
    lbCategories.Items.Exchange(i, i + 1);

    lbCategories.ItemIndex := i + 1;

    RefreshCategoryButtons;
  End;
End;

Procedure TdlgDockManager.btnCatDeleteClick(Sender: TObject);
Var
  i: Integer;
Begin
  If lbCategories.ItemIndex >= 0 Then
  Begin
    i := lbCategories.ItemIndex;

    FCategories.Delete(i);
    lbCategories.Items.Delete(i);
  End;
End;

Procedure TdlgDockManager.btnCatNewClick(Sender: TObject);
Begin
  With FCategories.Add Do
  Begin
    Name := 'New';
    ImageIndex := 1;
  End;

  lbCategories.Items.Add('New');
End;

Procedure TdlgDockManager.lbCategoriesSelectionChange(Sender: TObject; User: Boolean);
Var
  i: Integer;
  oCategory: TCategoryItem;
Begin
  lbDocks.Items.Clear;

  If lbCategories.ItemIndex = -1 Then
  Begin
    edtName.Text := '';
    cboCatImages.ItemIndex := -1;
  End
  Else
  Begin
    oCategory := FCategories[lbCategories.ItemIndex];
    edtName.Text := oCategory.Name;
    cboCatImages.ItemIndex := oCategory.ImageIndex;

    For i := 0 To oCategory.Docks.Count - 1 Do
      lbDocks.Items.Add(oCategory.Docks[i].dckBase.Tabsheet.Caption);
  End;

  RefreshCategoryButtons;
End;

Procedure TdlgDockManager.RefreshCategoryButtons;
Var
  i: Integer;
Begin
  i := lbCategories.ItemIndex;

  btnCatNew.Enabled := True;
  btnCatDelete.Enabled := (i >= 0) And (FCategories[i].Docks.Count = 0);

  btnCatUp.Enabled := (i > 0);
  btnCatDown.Enabled := (i >= 0) And (i < lbCategories.Items.Count - 1);
  edtName.Enabled := (i >= 0);
  cboCatImages.Enabled := (i >= 0);

  RefreshDockButtons;
End;

Procedure TdlgDockManager.RefreshDockButtons;
Var
  i: Integer;
Begin
  i := lbDocks.ItemIndex;

  If i = -1 Then
  Begin
    cboCategory.ItemIndex := -1;
    cboDockImages.ItemIndex := -1;
  End
  Else
  Begin
    cboCategory.Items.Assign(lbCategories.Items);
    cboCategory.ItemIndex := lbCategories.ItemIndex;

    cboDockImages.ItemIndex := FCategories[lbCategories.ItemIndex].Docks[i].ImageIndex;
  End;

  btnDockUp.Enabled := (i > 0);
  btnDockDown.Enabled := (i >= 0) And (i < lbDocks.Items.Count - 1);
  cboCategory.Enabled := (i >= 0);
  cboDockImages.Enabled := (i >= 0);
End;

Procedure TdlgDockManager.lbDocksSelectionChange(Sender: TObject; User: Boolean);
Begin
  RefreshDockButtons;
End;

Procedure TdlgDockManager.btnDockDownClick(Sender: TObject);
Var
  i: Integer;
Begin
  i := lbDocks.ItemIndex;
  If (i >= 0) And (i < lbDocks.Items.Count - 1) Then
  Begin
    i := lbDocks.ItemIndex;
    FCategories[lbCategories.ItemIndex].Docks.Exchange(i, i + 1);
    lbDocks.Items.Exchange(i, i + 1);

    lbDocks.ItemIndex := i + 1;

    RefreshDockButtons;
  End;
End;

Procedure TdlgDockManager.btnDockUpClick(Sender: TObject);
Var
  i: Integer;
Begin
  i := lbDocks.ItemIndex;
  If (i > 0) Then
  Begin
    FCategories[lbCategories.ItemIndex].Docks.Exchange(i, i - 1);
    lbDocks.Items.Exchange(i, i - 1);

    lbDocks.ItemIndex := i - 1;

    RefreshDockButtons;
  End;
End;

End.
