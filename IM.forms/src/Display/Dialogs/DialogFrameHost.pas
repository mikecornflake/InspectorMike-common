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

Type
  THackPageControl = Class(TPageControl);

Procedure TDialogFrameHost.RegisterFrame(AFrame: TFrameBase; Const ACaption: String);
Var
  oTab: TTabSheet;
  rDisplay: TRect;
  iFrameWidth: Integer;
  iFrameHeight: Integer;
  iDisplayWidth: Integer;
  iDisplayHeight: Integer;
Begin
  // Capture design size before parenting/alignment
  iFrameWidth := AFrame.Width;
  iFrameHeight := AFrame.Height;

  oTab := TTabSheet.Create(PageControl);
  oTab.PageControl := PageControl;
  oTab.Caption := ACaption;

  AFrame.Parent := oTab;

  rDisplay := THackPageControl(PageControl).DisplayRect;

  iDisplayWidth := rDisplay.Right - rDisplay.Left;
  iDisplayHeight := rDisplay.Bottom - rDisplay.Top;

  If iDisplayWidth < iFrameWidth Then
    Width := Width + (iFrameWidth - iDisplayWidth);

  If iDisplayHeight < iFrameHeight Then
    Height := Height + (iFrameHeight - iDisplayHeight);

  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;

  AFrame.Align := alClient;
End;

End.
