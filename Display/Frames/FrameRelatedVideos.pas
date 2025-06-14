Unit FrameRelatedVideos;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, FrameBase, FrameVideoPlayers;

Type

  { TFrameRelatedVideo }

  TFrameRelatedVideo = Class(TfmeBase)
    lblRelatedTitle: TLabel;
    lbRelated: TListBox;
    pnlRelated: TPanel;
    pnlVideo: TPanel;
    splRelated: TSplitter;
    Procedure lbRelatedDblClick(Sender: TObject);
    Procedure OnStop(Sender: TObject);
  Private
    FFilename: String;
    FPath: String;
    FPlaylist: String;
    FTempDir: String;

    Procedure SetFilename(AValue: String);

    // Used to determine related video files
    Function ExtractBaseFilename(sFile: String): String;
  Public
    fmeVideoPlayer: TFrameVideoPlayer;

    Constructor Create(TheOwner: TComponent); Override;

    Procedure RefreshUI; Override;

    Property Filename: String read FFilename write SetFilename;
    Property TempDir: String read FTempDir write FTempDir;
    Property Playlist: String read FPlaylist;
  End;

Implementation

{$R *.lfm}

{ TFrameRelatedVideo }

Constructor TFrameRelatedVideo.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  fmeVideoPlayer := TFrameVideoPlayer.Create(Self);
  fmeVideoPlayer.Parent := pnlVideo;
  fmeVideoPlayer.Name := 'fmeVideoPlayer2';
  fmeVideoPlayer.Align := alClient;
  fmeVideoPlayer.Autoplay := False;
  fmeVideoPlayer.OnStop := @OnStop;

  FTempDir := '';
  FPlaylist := '';

  FPath := '';
End;

Procedure TFrameRelatedVideo.lbRelatedDblClick(Sender: TObject);

  Function GetSelected: String;
  Var
    i: Integer;
  Begin
    i := 0;

    While (i < lbRelated.Items.Count) And (Not lbRelated.Selected[i]) Do
      Inc(i);

    If i < lbRelated.Items.Count Then
      Result := lbRelated.Items[i]
    Else
      Result := '';
  End;

Begin
  If lbRelated.Items.Count = 0 Then
    Exit;

  If (lbRelated.SelCount = 0) Then
    fmeVideoPlayer.Filename := FPath + lbRelated.Items[0]
  Else
    fmeVideoPlayer.Filename := FPath + GetSelected;
End;

Procedure TFrameRelatedVideo.OnStop(Sender: TObject);
Var
  i: Integer;
  sFile: String;
Begin
  If fmeVideoPlayer.Filename <> '' Then
  Begin
    sFile := ExtractFileName(fmeVideoPlayer.Filename);
    i := lbRelated.Items.IndexOf(sFile);

    If (i <> -1) And (i < lbRelated.Count - 1) Then
    Begin
      lbRelated.ItemIndex := i + 1;
      fmeVideoPlayer.Filename := FPath + lbRelated.Items[lbRelated.ItemIndex];
    End;
  End;
End;

// Expected filename format is XXXX_DDD
// where XXXX is the BaseFilename
// and DDDD is the FilenameNumber

// Slightly complicated by the fact that XXXX will likely include
// futher underscores

Function TFrameRelatedVideo.ExtractBaseFilename(sFile: String): String;
Var
  i: Integer;
Begin
  i := Length(sFile);

  While (sFile[i] <> '_') And (sFile[i] <> '-') Do
    Dec(i);

  // TODO - add defensive code
  If i>= 1 Then
    Result := Copy(sFile, 1, i - 1)
  Else
    Result := sFile;
End;

Function CustomSort(List: TStringList; Index1, Index2: Integer): Integer;

  Function ExtractFilenameNumber(sFile: String): Integer;
  Var
    i, iLen: Integer;
  Begin
    sFile := ExtractFileNameWithoutExt(sFile);

    iLen := Length(sFile);
    i := iLen;

    While (sFile[i] <> '_') And (sFile[i] <> '-') Do
      Dec(i);

    // TODO - add defensive code
    If i>=1 Then
      Result := StrToIntDef(Copy(sFile, i + 1, iLen - i), -1)
    Else
      Result := 0;
  End;

Var
  i1, i2: Integer;
Begin
  i1 := ExtractFilenameNumber(List[Index1]);
  i2 := ExtractFilenameNumber(List[Index2]);

  Result := i1 - i2;
End;

Procedure TFrameRelatedVideo.SetFilename(AValue: String);
Var
  sFile, sBaseFile, sExt, sPath: String;
  srTemp: TSearchRec;
  oFiles: TStringList;
  FPlaylistFile: TStringList;
  i: Integer;

Begin
  If FFilename <> AValue Then
  Begin
    FFilename := AValue;

    If AValue = '' Then
      fmeVideoPlayer.Filename := ''
    Else
    Begin
      // Find all related video files
      oFiles := TStringList.Create;
      Try
        sExt := ExtractFileExt(AValue);
        sFile := ExtractFileNameOnly(AValue);
        sPath := Copy(AValue, 1, Pos(sFile, AValue) - 1);
        // UNC paths bugger ExtractFilePath

        FPath := sPath;

        // Try and handle linked files.
        If Pos('_', sFile) > 0 Then
        Begin
          sBaseFile := ExtractBaseFilename(sFile);

          // Now we know sBaseFile, we search for all filenames starting with
          // sBaseFile, and ending with a number
          If FindFirst(Format('%s%s_*%s', [sPath, sBaseFile, sExt]),
            faAnyFile, srTemp) = 0 Then
          Begin
            oFiles.Add(srTemp.Name);

            While (FindNext(srTemp) = 0) Do
              oFiles.Add(srTemp.Name);
          End;
          FindClose(srTemp);
        End;

        // But not all files with _ are numbered...
        If oFiles.Count=0 Then
          oFiles.Add(Format('%s%s', [sFile, sExt]));

        // Custom sort the files
        oFiles.CustomSort(@CustomSort);

        lbRelated.Items.Clear;
        lbRelated.Items.AddStrings(oFiles);

        If FTempDir <> '' Then
        Begin
          FPlaylist := IncludeTrailingBackslash(FTempDir) +
            ChangeFileExt(ExtractFileName(FFilename), '.m3u');

          //If Not FileExists(FPlaylist) Then
          Begin
            FPlaylistFile := TStringList.Create;
            Try
              For i := 0 To oFiles.Count - 1 Do
                FPlaylistFile.Add(IncludeTrailingBackslash(FPath) + oFiles[i]);

              FPlaylistFile.SaveToFile(FPlaylist);
            Finally
              FPlaylistFile.Free;
            End;
          End;
        End
        Else
          FPlayList := '';
      Finally
        oFiles.Free;
      End;

      If lbRelated.Items.Count > 0 Then
      Begin
        lbRelated.Selected[0] := True;
        lbRelatedDblClick(lbRelated);
      End
      Else
        fmeVideoPlayer.Filename := '';
    End;

    RefreshUI;
  End;
End;

Procedure TFrameRelatedVideo.RefreshUI;
Begin
  fmeVideoPlayer.RefreshUI;
End;

End.
