Unit DialogFrameHost;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ComCtrls,
  FrameBase;

Type

  { TdlgFrameHost }

  { TDialogFrameHost }

  TDialogFrameHost = Class(TForm)
    ButtonPanel: TButtonPanel;
    PageControl: TPageControl;
    Procedure CancelButtonClick(Sender: TObject);
    Procedure OKButtonClick(Sender: TObject);
  Public
    Procedure RegisterFrame(AFrame: TFrameBase; Const ACaption: String);
  End;

Implementation

{$R *.lfm}

{ TdlgFrameHost }

Procedure TDialogFrameHost.OKButtonClick(Sender: TObject);
Begin
  ModalResult := mrOk;
End;

Procedure TDialogFrameHost.CancelButtonClick(Sender: TObject);
Begin
  ModalResult := mrCancel;
End;

Procedure TDialogFrameHost.RegisterFrame(AFrame: TFrameBase; Const ACaption: String);
Var
  oTab: TTabSheet;
Begin
  oTab := TTabSheet.Create(PageControl);
  oTab.PageControl := PageControl;
  oTab.Caption := ACaption;

  If PageControl.ClientWidth < AFrame.Width Then
    Width := AFrame.Width + (Width - PageControl.ClientWidth);

  If PageControl.ClientHeight < AFrame.Height Then
    Height := AFrame.Height + (Height - PageControl.ClientHeight);

  AFrame.Parent := oTab;
  AFrame.Align := alClient;
End;

End.
