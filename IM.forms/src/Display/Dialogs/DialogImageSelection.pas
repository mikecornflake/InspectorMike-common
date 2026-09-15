Unit DialogImageSelection;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, FrameImageViewer;

Type

  { TDialogImageSelection }

  { TDialogImageSelection }

  TDialogImageSelection = Class(TForm)
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

    Procedure LoadFromFolder(Const AFolder: String);

    Property CaptionSelected: String Read GetCaptionSelected Write SetCaptionSelected;
    Property CaptionNotSelected: String Read GetCaptionNotSelected Write SetCaptionNotSelected;

    Function ImageCount: Integer;
    Property Image[AIndex: Integer]: TViewerImage Read GetImage;
  End;

Implementation

Uses
  FileSupport;

  {$R *.lfm}


  { TDialogImageSelection }

Procedure TDialogImageSelection.FormCreate(Sender: TObject);
Begin
  fmeImageViewer := TFrameImageViewer.Create(self);
  fmeImageViewer.Parent := self;
  fmeImageViewer.Align := alClient;
  fmeImageViewer.Name := 'fmeImageViewer';

  fmeImageViewer.SelectionMode := ismMultiple;
End;

Procedure TDialogImageSelection.FormDestroy(Sender: TObject);
Begin
  FreeAndNil(fmeImageViewer);
End;

Function TDialogImageSelection.GetCaptionNotSelected: String;
Begin
  Result := fmeImageViewer.CaptionNotSelected;
End;

Function TDialogImageSelection.GetCaptionSelected: String;
Begin
  Result := fmeImageViewer.CaptionSelected;
End;

Function TDialogImageSelection.GetImage(AIndex: Integer): TViewerImage;
Begin
  Result := fmeImageViewer.Image[AIndex];
End;

Procedure TDialogImageSelection.SetCaptionNotSelected(Const AValue: String);
Begin
  fmeImageViewer.CaptionNotSelected := AValue;
End;

Procedure TDialogImageSelection.SetCaptionSelected(Const AValue: String);
Begin
  fmeImageViewer.CaptionSelected := AValue;
End;

Procedure TDialogImageSelection.AddImage(Const AFilename: String; Const ACaption: String;
  Const ASelected: Boolean);
Var
  oImage: TViewerImage;
Begin
  oImage := fmeImageViewer.AddImage(AFilename, ACaption);
  oImage.Selected := ASelected;
End;

Procedure TDialogImageSelection.LoadFromFolder(Const AFolder: String);
Var
  SearchRec: TSearchRec;
  sFolder: String;
  sExt, sFile: String;
Begin
  sFolder := IncludeTrailingPathDelimiter(AFolder);

  If FindFirst(sFolder + '*.*', faAnyFile, SearchRec) = 0 Then
  Begin
    Try
      Repeat
        If (SearchRec.Attr And faDirectory) = 0 Then
        Begin
          sExt := ExtractFileExt(SearchRec.Name);
          sFile := SearchRec.Name;
          If IsImage(sExt) Then
            AddImage(sFolder + sFile, sFile, Not sFile.Contains('Aux', True));
        End;
      Until FindNext(SearchRec) <> 0;
    Finally
      FindClose(SearchRec);
    End;
  End;
End;

Function TDialogImageSelection.ImageCount: Integer;
Begin
  Result := fmeImageViewer.ImageCount;
End;

End.
