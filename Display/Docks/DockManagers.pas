Unit DockManagers;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, ExtCtrls, ComCtrls, Controls, Inifiles, DockBase;

Type
  { TDockItems }

  TDockItem = Class(TCollectionItem)
  Private
    FdckBase: TdckBase;
    FImageIndex: Integer;
  Public
    Constructor Create(ACollection: TCollection); Override;

    Procedure Assign(Source: TPersistent); Override;

    Property dckBase: TdckBase read FdckBase write FdckBase;
    Property ImageIndex: Integer read FImageIndex write FImageIndex;
  End;

  { TDockItems }

  TDockItems = Class(TCollection)
  Private
    Function GetItem(AIndex: Integer): TDockItem;
  Public
    Constructor Create;

    Procedure Assign(Source: TPersistent); Override;

    Function IndexOf(ADock: TdckBase): Integer;

    Function Add: TDockItem;
    Property Items[AIndex: Integer]: TDockItem read GetItem; Default;
  End;

  { TCategoryItem }

  TCategoryItem = Class(TCollectionItem)
  Private
    FAdministrator: Boolean;
    FDocks: TDockItems;
    FImageIndex: Integer;
    FName: String;
    FPageControl: TPageControl;
  Public
    Constructor Create(ACollection: TCollection); Override;
    Destructor Destroy; Override;

    Procedure Assign(Source: TPersistent); Override;

    Property Name: String read FName write FName;
    Property ImageIndex: Integer read FImageIndex write FImageIndex;
    Property PageControl: TPageControl read FPageControl write FPageControl;
    Property Administrator: Boolean read FAdministrator write FAdministrator;

    Property Docks: TDockItems read FDocks;
  End;

  { TCategoryItems }

  TCategoryItems = Class(TCollection)
  Private
    Function GetItem(AIndex: Integer): TCategoryItem;
  Public
    Constructor Create;

    Procedure LoadSettings(oInifile: TIniFile);
    Procedure SaveSettings(oInifile: TIniFile);

    Procedure Assign(Source: TPersistent); Override;

    Function IndexOf(ACategory: TCategoryItem): Integer;
    Function CategoryItem(AName: String): TCategoryItem;

    Function Add: TCategoryItem;
    Property Items[AIndex: Integer]: TCategoryItem read GetItem; Default;
  End;

Implementation

Uses
  FormMultiDock;

{ TCategoryItems }

Function TCategoryItems.GetItem(AIndex: Integer): TCategoryItem;
Begin
  Result := TCategoryItem(Inherited GetItem(AIndex));
End;

Constructor TCategoryItems.Create;
Begin
  Inherited Create(TCategoryItem);
End;

Procedure TCategoryItems.LoadSettings(oInifile: TIniFile);
Var
  iCount, i: Integer;
  oCategory: TCategoryItem;
Begin
  Clear;

  iCount := oInifile.ReadInteger('Category', 'Category Count', 0);

  If iCount = 0 Then
  Begin
    oCategory := Add;
    oCategory.Name := 'QA';
    oCategory.ImageIndex := 1;

    oCategory := Add;
    oCategory.Name := 'Events';
    oCategory.ImageIndex := 31;

    oCategory := Add;
    oCategory.Name := 'Workpacks';
    oCategory.ImageIndex := 11;

    oCategory := Add;
    oCategory.Name := 'Other';
    oCategory.ImageIndex := 14;

    oCategory := Add;
    oCategory.Name := 'Admin';
    oCategory.ImageIndex := 19;
  End
  Else
    For i := 0 To iCount - 1 Do
    Begin
      oCategory := Add;
      oCategory.Name := oInifile.ReadString('Category', Format('Category %d.Name', [i]),
        'Not assigned');
      oCategory.ImageIndex := oInifile.ReadInteger('Category',
        Format('Category %d.ImageIndex', [i]), 1);
      oCategory.Administrator := oInifile.ReadBool('Category',
        Format('Category %d.Administrator', [i]), False);
    End;

  MultiDockForm.RefreshCategories;
End;

Procedure TCategoryItems.SaveSettings(oInifile: TIniFile);
Var
  i: Integer;
Begin
  oInifile.WriteInteger('Category', 'Category Count', Count);

  For i := 0 To Count - 1 Do
  Begin
    oInifile.WriteString('Category', Format('Category %d.Name', [i]), GetItem(i).Name);
    oInifile.WriteInteger('Category', Format('Category %d.ImageIndex', [i]),
      GetItem(i).ImageIndex);
    oInifile.WriteBool('Category', Format('Category %d.Administrator', [i]),
      GetItem(i).Administrator);
  End;
End;

Procedure TCategoryItems.Assign(Source: TPersistent);
Var
  i: Integer;
Begin
  If Source Is TCategoryItems Then
  Begin
    Clear;
    For i := 0 To TCategoryItems(Source).Count - 1 Do
      Add.Assign(TCategoryItems(Source).Items[i]);
  End
  Else
    Inherited Assign(Source);
End;

Function TCategoryItems.IndexOf(ACategory: TCategoryItem): Integer;
Var
  i: Integer;
Begin
  Result := -1;

  For i := 0 To Count - 1 Do
    If (GetItem(i) = ACategory) Then
    Begin
      Result := i;
      Exit;
    End;
End;

Function TCategoryItems.CategoryItem(AName: String): TCategoryItem;
Var
  i: Integer;
Begin
  Result := nil;

  AName := Uppercase(AName);

  For i := 0 To Count - 1 Do
    If (Uppercase(GetItem(i).Name) = AName) Then
    Begin
      Result := GetItem(i);
      Exit;
    End;
End;

Function TCategoryItems.Add: TCategoryItem;
Begin
  Result := TCategoryItem(Inherited Add);
End;

{ TCategoryItem }

Constructor TCategoryItem.Create(ACollection: TCollection);
Begin
  Inherited Create(ACollection);

  FDocks := TDockItems.Create;
  FPageControl := nil;

  FAdministrator := False;
End;

Destructor TCategoryItem.Destroy;
Begin
  FreeAndNil(FDocks);

  Inherited Destroy;
End;

Procedure TCategoryItem.Assign(Source: TPersistent);
Begin
  If Source Is TCategoryItem Then
  Begin
    FDocks.Assign(TCategoryItem(Source).Docks);

    FName := TCategoryItem(Source).Name;
    FImageIndex := TCategoryItem(Source).ImageIndex;
    FPageControl := TCategoryItem(Source).PageControl;
    FAdministrator := TCategoryItem(Source).Administrator;
  End
  Else
    Inherited Assign(Source);
End;

{ TDockItem }

Constructor TDockItem.Create(ACollection: TCollection);
Begin
  Inherited Create(ACollection);
End;

Procedure TDockItem.Assign(Source: TPersistent);
Begin
  If Source Is TDockItem Then
  Begin
    FdckBase := TDockItem(Source).dckBase;
    FImageIndex := TDockItem(Source).ImageIndex;
  End
  Else
    Inherited Assign(Source);
End;

{ TDockItems }

Function TDockItems.GetItem(AIndex: Integer): TDockItem;
Begin
  Result := TDockItem(Inherited GetItem(AIndex));
End;

Constructor TDockItems.Create;
Begin
  Inherited Create(TDockItem);
End;

Procedure TDockItems.Assign(Source: TPersistent);
Var
  i: Integer;
Begin
  If Source Is TDockItems Then
  Begin
    Clear;
    For i := 0 To TDockItems(Source).Count - 1 Do
      Add.Assign(TDockItems(Source).Items[i]);
  End
  Else
    Inherited Assign(Source);
End;

Function TDockItems.IndexOf(ADock: TdckBase): Integer;
Var
  i: Integer;
Begin
  Result := -1;

  For i := 0 To Count - 1 Do
    If (GetItem(i).dckBase = ADock) Then
    Begin
      Result := i;
      Exit;
    End;
End;

Function TDockItems.Add: TDockItem;
Begin
  Result := TDockItem(Inherited Add);
End;

End.
