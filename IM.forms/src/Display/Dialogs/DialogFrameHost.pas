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
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure OKButtonClick(Sender: TObject);
  Private
    FActivated: Boolean;
    FFrames: TStringList;
  Public
    Procedure RegisterFrame(AFrame: TFrameBase; Const ACaption: String);
  End;

Implementation

Uses
  Math;

  {$R *.lfm}

  { TdlgFrameHost }

Procedure TDialogFrameHost.FormCreate(Sender: TObject);
Begin
  FActivated := False;

  FFrames := TStringList.Create(False);
End;

Procedure TDialogFrameHost.FormDestroy(Sender: TObject);
Begin
  FreeAndNil(FFrames);
End;

Procedure TDialogFrameHost.OKButtonClick(Sender: TObject);
Begin
  ModalResult := mrOk;
End;

Procedure TDialogFrameHost.CancelButtonClick(Sender: TObject);
Begin
  ModalResult := mrCancel;
End;

Procedure TDialogFrameHost.RegisterFrame(AFrame: TFrameBase; Const ACaption: String);
Begin
  FFrames.AddObject(ACaption, AFrame);
End;

Procedure TDialogFrameHost.FormShow(Sender: TObject);
Var
  oTab: TTabSheet;
  iMaxFrameWidth, iMaxFrameHeight: Integer;
  i, iDlgWidth, iDlgHeight, iDisplayWidth, iDisplayHeight, iDlgLeft, iDlgTop: Integer;

  oFrame: TFrameBase;
  sCaption: String;
Begin
  If FActivated Then
    Exit;

  FActivated := True;

  iDisplayWidth := PageControl.ClientWidth;
  iDisplayHeight := PageControl.ClientHeight;

  iMaxFrameWidth := 0;
  iMaxFrameHeight := 0;

  // Delay loading until Form created
  For i := 0 To FFrames.Count - 1 Do
  Begin
    sCaption := FFrames[i];
    oFrame := TFrameBase(FFrames.Objects[i]);

    // Capture design size before parenting/alignment
    iMaxFrameWidth := Max(iMaxFrameWidth, oFrame.Width);
    iMaxFrameHeight := Max(iMaxFrameHeight, oFrame.Height);

    oTab := TTabSheet.Create(PageControl);
    oTab.PageControl := PageControl;
    oTab.Caption := sCaption;

    oFrame.Parent := oTab;
    oFrame.Align := alClient;
  End;

  If iDisplayWidth < iMaxFrameWidth Then
    iDlgWidth := Width + (iMaxFrameWidth - iDisplayWidth)
  Else
    iDlgWidth := Width;

  If iDisplayHeight < iMaxFrameHeight Then
    iDlgHeight := Height + (iMaxFrameHeight - iDisplayHeight)
  Else
    iDlgHeight := Height;

  If Assigned(Application.MainForm) Then
  Begin
    iDlgLeft := Application.MainForm.Left + (Application.MainForm.Width - iDlgWidth) Div 2;
    iDlgTop := Application.MainForm.Top + (Application.MainForm.Height - iDlgHeight) Div 2;
  End
  Else
  Begin
    iDlgLeft := Left;
    iDlgTop := Top;
  End;

  SetBounds(iDlgLeft, iDlgTop, iDlgWidth, iDlgHeight);

  Constraints.MinWidth := iDlgWidth;
  Constraints.MinHeight := iDlgHeight;
End;

End.
