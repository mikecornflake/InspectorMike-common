Unit FormVolumePopup;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls;

Type
  TOnVolumeChanged = Procedure(AOwner: TObject; AVolume: Integer; AMuted: Boolean) Of Object;

  { TfrmVolumePopup }

  TfrmVolumePopup = Class(TForm)
    cbMuted: TCheckBox;
    tbVolume: TTrackBar;
    Procedure StateChange(Sender: TObject);
  Private
    FOnVolumeChanged: TOnVolumeChanged;
  Protected
    Procedure Deactivate; Override;
  Public
    Property OnVolumeChanged: TOnVolumeChanged Read FOnVolumeChanged Write FOnVolumeChanged;
  End;

Implementation

{$R *.lfm}

{ TfrmVolumePopup }

Procedure TfrmVolumePopup.StateChange(Sender: TObject);
Begin
  If Assigned(FOnVolumeChanged) Then
    FOnVolumeChanged(Self, tbVolume.Position, cbMuted.Checked);
End;

Procedure TfrmVolumePopup.Deactivate;
Begin
  Inherited Deactivate;
  Hide;
End;

End.
