Unit FrameVideoPlayers;

{-------------------------------------------------------------------------------
  Package   : IM_application
  Unit      : FrameVideoPlayers.pas
  Description
    Toolbar-driven video player frame built on the DirectShow host.

  Source
    Copyright (c) 2025
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    ~2008: Creation date unknown, original local SVN repository lost
    2014-07-05: Uploaded to SourceForge/Package "Shared"
    2024-01-22: Migrated to Github.  Refactored package to "IM_application"
    2025-11-29: Added LGPL-3.0-or-later license header

  License
    This file is part of IM_application.lpk.

    This library is free software: you can redistribute it and/or modify it
    under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or (at
    your option) any later version.

    This library is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
    General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this library. If not, see <https://www.gnu.org/licenses/>.

    SPDX-License-Identifier: LGPL-3.0-or-later
-------------------------------------------------------------------------------}



{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, LazFileUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, Menus, FrameBase, FrameVideoDirectShow;

Type

  { TFrameVideoPlayer }

  TFrameVideoPlayer = Class(TfmeBase)
    btnGrab: TToolButton;
    btnOpenInExplorer: TToolButton;
    btnPause: TToolButton;
    btnPlay: TToolButton;
    dlgSaveLocation: TSelectDirectoryDialog;
    ilToolbar: TImageList;
    lblStatus: TLabel;
    lblTime: TLabel;
    mnuGrabAlwaysAsk: TMenuItem;
    mnuGrabOnlyAskOnce: TMenuItem;
    mnuGrabVideoFolder: TMenuItem;
    pmnuGrab: TPopupMenu;
    pnlToolbar: TPanel;
    pnlVideo: TPanel;
    tbVideo: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    btnRewind: TToolButton;
    btnFastForward: TToolButton;
    btnStepBack: TToolButton;
    btnStepForward: TToolButton;
    Procedure btnFastForwardClick(Sender: TObject);
    Procedure btnGrabClick(Sender: TObject);
    Procedure btnOpenInExplorerClick(Sender: TObject);
    Procedure btnPauseClick(Sender: TObject);
    Procedure btnPlayClick(Sender: TObject);
    Procedure btnRewindClick(Sender: TObject);
    Procedure btnStepForwardClick(Sender: TObject);
    Procedure mnuGrabClick(Sender: TObject);
    Procedure pnlToolbarResize(Sender: TObject);
    Procedure btnStepBackClick(Sender: TObject);
  Private
    FAutoplay: Boolean;
    FLastImageFolder: String;
    FOnStop: TNotifyEvent;

    Function GetFilename: String;
    Function GetShowLabel: Boolean;
    Procedure OnTimer(Sender: TObject; CurrentPos, StopPos: Cardinal);
    Procedure SetFilename(AValue: String);
    Procedure SetShowLabel(AValue: Boolean);
  Public
    fmeVideo: TFrameDirectShowVideo;

    Constructor Create(TheOwner: TComponent); Override;

    Procedure RefreshUI; Override;

    Property Filename: String read GetFilename write SetFilename;

    Property Autoplay: Boolean read FAutoplay write FAutoplay;
    Property ShowLabel: Boolean read GetShowLabel write SetShowLabel;

    Property OnStop: TNotifyEvent read FOnStop write FOnStop;
  End;

Implementation

Uses
  OSSupport, FormMain, Math, Clipbrd;

{$R *.lfm}

{ TFrameVideoPlayer }

Constructor TFrameVideoPlayer.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  fmeVideo := TFrameDirectShowVideo.Create(Self);
  fmeVideo.Parent := pnlVideo;
  fmeVideo.Name := 'fmeVideo';
  fmeVideo.Align := alClient;
  fmeVideo.OnTimer := @OnTimer;

  lblStatus.Caption := '';
  FAutoplay := False;
  FLastImageFolder := '';

  FOnStop := nil;

  RefreshUI;
End;

Procedure TFrameVideoPlayer.RefreshUI;
Var
  bEnabled: Boolean;
Begin
  Inherited RefreshUI;

  bEnabled := FileExists(fmeVideo.Filename);
  pnlToolbar.Visible := bEnabled;

  If fmeVideo.GraphLoaded Then
  Begin
    If fmeVideo.Rate = 1 Then
      btnFastForward.Hint := 'Play faster'
    Else
      btnFastForward.Hint := Format('Play faster: Current rate %.1f times normal',
        [fmeVideo.Rate]);

    btnRewind.Visible := fmeVideo.CanRewind;
  End;
End;

Procedure TFrameVideoPlayer.btnPlayClick(Sender: TObject);
Begin
  If Not fmeVideo.GraphLoaded Then
    fmeVideo.Play
  Else
  Begin
    If fmeVideo.Rate <> 1 Then
      fmeVideo.Rate := 1;

    If Not fmeVideo.Paused Then
      fmeVideo.Pause;

    fmeVideo.Play;
  End;

  RefreshUI;
End;

Procedure TFrameVideoPlayer.btnFastForwardClick(Sender: TObject);
Begin
  fmeVideo.Rate := fmeVideo.Rate * sqrt(2);

  RefreshUI;
End;

Procedure TFrameVideoPlayer.btnRewindClick(Sender: TObject);
Begin
  fmeVideo.Rate := -1 * Abs(fmeVideo.Rate * sqrt(2));

  RefreshUI;
End;

Procedure TFrameVideoPlayer.mnuGrabClick(Sender: TObject);
Begin
  If Sender Is TMenuItem Then
    TMenuItem(Sender).Checked := True;
End;

Procedure TFrameVideoPlayer.pnlToolbarResize(Sender: TObject);
Begin
  tbVideo.Top := 0;
  tbVideo.Left := Max(0, (pnlToolbar.Width - tbVideo.Width) Div 2);
End;

Procedure TFrameVideoPlayer.btnStepBackClick(Sender: TObject);
Begin
  If Not fmeVideo.Paused Then
    fmeVideo.Pause;

  fmeVideo.Position := fmeVideo.Position - 1000;

  RefreshUI;
End;

Procedure TFrameVideoPlayer.btnStepForwardClick(Sender: TObject);
Begin
  fmeVideo.Position := fmeVideo.Position + 5000;

  RefreshUI;
End;

Procedure TFrameVideoPlayer.btnPauseClick(Sender: TObject);
Begin
  If fmeVideo.Paused Then
    fmeVideo.Play
  Else
    fmeVideo.Pause;

  RefreshUI;
End;

Procedure TFrameVideoPlayer.btnGrabClick(Sender: TObject);
Var
  oBitmap: TBitmap;
  oJPEG: TJPEGImage;
  sPath, sFile: String;
  i: Integer;
Begin
  oBitmap := TBitmap.Create;
  Try
    If fmeVideo.GetBitmap(oBitmap) Then
    Begin
      If (dlgSaveLocation.InitialDir = '') Then
        dlgSaveLocation.InitialDir :=
          IncludeTrailingBackslash(ExtractFilePath(fmeVideo.Filename));

      // THERE has to be a way to rationalise this into more sane code...
      If (mnuGrabOnlyAskOnce.Checked) Then
      Begin
        If (FLastImageFolder = '') Then
        Begin
          If dlgSaveLocation.Execute Then
          Begin
            sPath := IncludeTrailingBackslash(dlgSaveLocation.FileName);
            FLastImageFolder := sPath;
          End
          Else
            sPath := '';
        End
        Else
          sPath := FLastImageFolder;
      End
      Else If (mnuGrabAlwaysAsk.Checked) Then
      Begin
        If dlgSaveLocation.Execute Then
        Begin
          sPath := IncludeTrailingBackslash(dlgSaveLocation.FileName);
          FLastImageFolder := sPath;
        End
        Else
          sPath := '';
      End
      Else //  Default case is to save the image to the video folder
        sPath := IncludeTrailingBackslash(ExtractFilePath(fmeVideo.Filename));

      // If the user clicked cancel to the Select Folder dialog, then sPath will = ''
      If sPath <> '' Then
      Begin
        sFile := ExtractFileNameWithoutExt(ExtractFileNameOnly(fmeVideo.Filename));

        i := 0;
        While FileExists(Format('%s%s-%d.jpg', [sPath, sFile, i])) Do
          Inc(i);

        Clipboard.Assign(oBitmap);

        oJPEG := TJPEGImage.Create;
        Try
          oJPEG.CompressionQuality := 100;
          oJPEG.Assign(oBitmap);
          oJPEG.SaveToFile(Format('%s%s-%d.jpg', [sPath, sFile, i]));
        Finally
          oJPEG.Free;
        End;

        lblStatus.Caption := 'Saved ' + Format('%s%s-%d.jpg', [sPath, sFile, i]);
        MainForm.Status := 'Saved ' + Format('%s%s-%d.jpg', [sPath, sFile, i]);
      End;
    End
    Else
    Begin
      lblStatus.Caption := 'Failed to grab image using SampleGrabber.Getbitmap';
      MainForm.Status := 'Failed to grab image using SampleGrabber.Getbitmap';
    End;
  Finally
    oBitmap.Free;
  End;
End;

Procedure TFrameVideoPlayer.btnOpenInExplorerClick(Sender: TObject);
Begin
  If fmeVideo.Filename <> '' Then
    LaunchFile('explorer.exe', Format('/e,/select,"%s"', [fmeVideo.Filename]));
End;

Function TFrameVideoPlayer.GetFilename: String;
Begin
  Result := fmeVideo.Filename;
End;

Procedure TFrameVideoPlayer.SetFilename(AValue: String);
Begin
  FLastImageFolder := '';
  fmeVideo.Filename := AValue;

  RefreshUI;

  lblStatus.Caption := Format('File: %s', [fmeVideo.Filename]);
  MainForm.Status := Format('File: %s', [fmeVideo.Filename]);

  If FileExists(fmeVideo.Filename) And FAutoplay Then
    fmeVideo.Play;
End;

Function TFrameVideoPlayer.GetShowLabel: Boolean;
Begin
  Result := lblStatus.Visible;
End;


Procedure TFrameVideoPlayer.SetShowLabel(AValue: Boolean);
Begin
  lblStatus.Visible := AValue;
End;

Procedure TFrameVideoPlayer.OnTimer(Sender: TObject; CurrentPos, StopPos: Cardinal);

  Function ToTime(iTemp: Integer): String;
  Begin
    Result := FormatDateTime('HH:mm:ss', (iTemp / 1000) / (24 * 60 * 60));
  End;

Begin
  lblTime.Caption := ToTime(CurrentPos) + #13#10 + ToTime(StopPos);

  If (CurrentPos = StopPos) Then
    If Not (fmeVideo.Paused) Then
    Begin
      fmeVideo.Pause;

      If Assigned(FOnStop) Then
        FOnStop(Self);
    End;
End;

End.
