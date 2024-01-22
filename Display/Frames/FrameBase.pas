Unit FrameBase;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Inifiles,
  FormPersistent;

Type

  { TfmeBase }

  TfmeBase = Class(TFrame)
  Protected
    FParentForm: TfrmPersistent;
    FFullIdentKey: String;
    Function GetFullIdentKey: String; Virtual;
    Function GetSettingsKey: String; Virtual;
  Public
    Constructor Create(TheOwner: TComponent); Override;

    Procedure RefreshUI; Virtual;

    Procedure LoadSettings(oInifile: TIniFile); Virtual;
    Procedure SaveSettings(oInifile: TIniFile); Virtual;

    // Called on database open/close
    Procedure Open; Virtual;
    Procedure Close; Virtual;

    Property ParentForm: TfrmPersistent read FParentForm write FParentForm;
    Property SettingsKey: String read GetSettingsKey;
    Property FullIdentKey: String read GetFullIdentKey;
  End;

Implementation

{$R *.lfm}

{ TfmeBase }

Function TfmeBase.GetSettingsKey: String;
Begin
  If Assigned(FParentForm) Then
    Result := FParentForm.SettingsKey
  Else
    Result := Name;
End;

Function TfmeBase.GetFullIdentKey: String;

  Function ParentFullIdentKey(oComponent: TComponent): String;
  Var
    oParent: TComponent;
    s: String;
  Begin
    oParent := oComponent.Owner;

    If (oParent = nil) Or (oParent Is TfrmPersistent) Then
      Result := ''
    Else If oParent Is TfmeBase Then
    Begin
      s := ParentFullIdentKey(oParent);
      If (s = '') Then
        Result := oParent.Name
      Else
        Result := Format('%s.%s', [s, oParent.Name]);
    End
    Else
      Result := ParentFullIdentKey(oParent);
  End;

Var
  s: String;
Begin
  If FFullIdentKey = '' Then
  Begin
    s := ParentFullIdentKey(Self);

    If (s = '') Then
      FFullIdentKey := Name
    Else
      FFullIdentKey := Format('%s.%s', [s, Name]);
  End;

  Result := FFullIdentKey;
End;

Constructor TfmeBase.Create(TheOwner: TComponent);

  Function FindParentForm(oComponent: TComponent): TfrmPersistent;
  Begin
    If Assigned(oComponent) Then
    Begin
      If oComponent Is TfrmPersistent Then
        Result := TfrmPersistent(oComponent)
      Else
        Result := FindParentForm(oComponent.Owner);
    End
    Else
      Result := nil;
  End;

Begin
  Inherited Create(TheOwner);

  FParentForm := FindParentForm(TheOwner);
End;

Procedure TfmeBase.RefreshUI;
Begin

End;

Procedure TfmeBase.LoadSettings(oInifile: TIniFile);
Begin

End;

Procedure TfmeBase.SaveSettings(oInifile: TIniFile);
Begin

End;

Procedure TfmeBase.Open;
Begin

End;

Procedure TfmeBase.Close;
Begin

End;

Initialization

End.
