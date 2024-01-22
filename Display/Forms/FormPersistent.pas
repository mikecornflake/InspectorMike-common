Unit FormPersistent;

(*
 Forms that descend from here will be able to have their settings saved
 in a TIniFile...

 Automatic management for Frames descended from FrameBase (just need
 to override LaodSettings/SaveSettings in Frame)

 This assumes Application.MainForm is descended from TfrmMain in FormMain.pas
 *)

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Inifiles;

Type

  { TfrmPersistent }

  TfrmPersistent = Class(TForm)
  Private
    FRefreshUIing: Boolean;
  Protected
    FKey: String;
    Function GetSettingsKey: String; Virtual;
  Public
    Constructor Create(TheOwner: TComponent); Override;

    Procedure RefreshUI; Virtual;

    Procedure LoadSettings(oInifile: TIniFile); Virtual;
    Procedure SaveSettings(oInifile: TIniFile); Virtual;

    Property SettingsKey: String read GetSettingsKey;
  End;

Implementation

Uses
  FrameBase;

{$R *.lfm}

{ TfrmPersistent }

Constructor TfrmPersistent.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  FKey := Caption;
  FRefreshUIing := False;
End;

Procedure TfrmPersistent.RefreshUI;

  Procedure RefreshUIChildren(AControl: TWinControl);
  Var
    i: Integer;
    oControl: TWinControl;
  Begin
    For i := 0 To AControl.ControlCount - 1 Do
      If AControl.Controls[i] Is TWinControl Then
      Begin
        oControl := TWinControl(AControl.Controls[i]);

        If oControl Is TfmeBase Then
          TfmeBase(oControl).RefreshUI;

        If Assigned(oControl) Then
          RefreshUIChildren(oControl);
      End;
  End;

Begin
  If Not FRefreshUIing Then
  Begin
    FRefreshUIing := True;
    Try
      RefreshUIChildren(Self);
    Finally
      FRefreshUIing := False;
    End;
  End;
End;

Function TfrmPersistent.GetSettingsKey: String;
Begin
  Result := FKey;
End;

Procedure TfrmPersistent.LoadSettings(oInifile: TIniFile);

  Procedure LoadSettingsChildren(AControl: TWinControl);
  Var
    i: Integer;
    oControl: TWinControl;
  Begin
    For i := 0 To AControl.ControlCount - 1 Do
      If AControl.Controls[i] Is TWinControl Then
      Begin
        oControl := TWinControl(AControl.Controls[i]);

        If oControl Is TfmeBase Then
          TfmeBase(oControl).LoadSettings(oInifile);

        If Assigned(oControl) Then
          LoadSettingsChildren(oControl);
      End;
  End;

Begin
  LoadSettingsChildren(Self);
End;

Procedure TfrmPersistent.SaveSettings(oInifile: TIniFile);

  Procedure SaveSettingsChildren(AControl: TWinControl);
  Var
    i: Integer;
    oControl: TWinControl;
  Begin
    For i := 0 To AControl.ControlCount - 1 Do
      If AControl.Controls[i] Is TWinControl Then
      Begin
        oControl := TWinControl(AControl.Controls[i]);

        If oControl Is TfmeBase Then
          TfmeBase(oControl).SaveSettings(oInifile);

        If Assigned(oControl) Then
          SaveSettingsChildren(oControl);
      End;
  End;

Begin
  SaveSettingsChildren(Self);
End;

Initialization

End.
