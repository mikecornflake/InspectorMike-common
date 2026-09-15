Unit DialogImageSelection;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, FrameImageViewer;

Type

  { TdlgImageSelection }

  TdlgImageSelection = Class(TForm)
    ButtonPanel1: TButtonPanel;
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
  Private
    fmeImageViewer: TFrameImageViewer;
    Function GetCaptionNotSelected: String;
    Function GetCaptionSelected: String;
    Function GetImage(AIndex: Integer): TViewerImage;
    Procedure SetCaptionNotSelected(Const AValue: String);
    Procedure SetCaptionSelected(Const AValue: String);
  Public
    Procedure AddImage(Const AFilename: String; Const ACaption: String; Const ASelected: Boolean);

    Property CaptionSelected: String Read GetCaptionSelected Write SetCaptionSelected;
    Property CaptionNotSelected: String Read GetCaptionNotSelected Write SetCaptionNotSelected;

    Function ImageCount: Integer;
    Property Image[AIndex: Integer]: TViewerImage Read GetImage;
  End;

Var
  dlgImageSelection: TdlgImageSelection;

Implementation

{$R *.lfm}

{ TdlgImageSelection }

Procedure TdlgImageSelection.FormCreate(Sender: TObject);
Begin
  fmeImageViewer := TFrameImageViewer.Create(self);
  fmeImageViewer.Parent := self;
  fmeImageViewer.Align := alClient;
  fmeImageViewer.Name := 'fmeImageViewer';

  fmeImageViewer.SelectionMode := ismMultiple;
End;

Procedure TdlgImageSelection.FormDestroy(Sender: TObject);
Begin
  FreeAndNil(fmeImageViewer);
End;

Function TdlgImageSelection.GetCaptionNotSelected: String;
Begin
  Result := fmeImageViewer.CaptionNotSelected;
End;

Function TdlgImageSelection.GetCaptionSelected: String;
Begin
  Result := fmeImageViewer.CaptionSelected;
End;

Function TdlgImageSelection.GetImage(AIndex: Integer): TViewerImage;
Begin
  Result := fmeImageViewer.Image[AIndex];
End;

Procedure TdlgImageSelection.SetCaptionNotSelected(Const AValue: String);
Begin
  fmeImageViewer.CaptionNotSelected := AValue;
End;

Procedure TdlgImageSelection.SetCaptionSelected(Const AValue: String);
Begin
  fmeImageViewer.CaptionSelected := AValue;
End;

Procedure TdlgImageSelection.AddImage(Const AFilename: String; Const ACaption: String;
  Const ASelected: Boolean);
Var
  oImage: TViewerImage;
Begin
  oImage := fmeImageViewer.AddImage(AFilename, ACaption);
  oImage.Selected := ASelected;
End;

Function TdlgImageSelection.ImageCount: Integer;
Begin
  Result := fmeImageViewer.ImageCount;
End;

End.
