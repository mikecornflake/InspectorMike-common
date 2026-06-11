Unit FrameSyncedVideo;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, FGL,
  FrameVideoBase, VideoGridLayout;

Type
  { TfmeSyncedVideo }

  TfmeSyncedVideo = Class(TfmeVideoBase)
    Procedure FrameResize(Sender: TObject);
  Private
    FPlaybackClass: TfmeVideoBaseClass;
    FVideos: TfmeVideoBaseList;
    FMaster: TfmeVideoBase;
    FState: TVideoState;
    FSyncTimer: TTimer;
    FSyncSeekThresholdMS: TVideoTime;
    FStartTime: TDateTime;
    FReadyToPlay: Boolean;
    FAutoPlayWhenLoaded: Boolean;
    FLayout: TVideoGridLayout;

    Procedure SetState(AValue: TVideoState);
    Procedure SetMaster(AValue: TfmeVideoBase);

    Function GetCurrentTime: TDateTime;
    Procedure SetCurrentTime(AValue: TDateTime);
    Function GetEndTime: TDateTime;

    Procedure MasterPosition(Sender: TObject; PositionMS, DurationMS: TVideoTime);
    Procedure VideoStateChanged(Sender: TObject; AState: TVideoState);

    Function LoadedVideoCount: Integer;
    Function AllVideosLoaded: Boolean;
    Procedure CheckAllVideosLoaded;

    Procedure SyncTimerTimer(Sender: TObject);
    Procedure SyncVideos;

  Protected
    Function GetPosition: TVideoTime; Override;
    Procedure SetPosition(AValue: TVideoTime); Override;
    Function GetDuration: TVideoTime; Override;
    Function GetRate: Double; Override;
    Procedure SetRate(AValue: Double); Override;
    Function GetState: TVideoState; Override;

  Public
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure ClearVideos;

    Procedure Layout(ARows, ACols: Integer; ASequence: TVideoLayoutSequence);

    Function Load(Const AFilename: String): Boolean; Override;
    Function Play: Boolean; Override;
    Function Pause: Boolean; Override;
    Function Resume: Boolean; Override;
    Function Stop: Boolean; Override;

    Function CanSeek: Boolean; Override;
    Function CanSetRate: Boolean; Override;
    Function CanGrabBitmap: Boolean; Override;
    Function GetBitmap(Bitmap: TBitmap): Boolean; Override;

    Property Videos: TfmeVideoBaseList Read FVideos;
    Property Master: TfmeVideoBase Read FMaster Write SetMaster;

    Property StartTime: TDateTime Read FStartTime Write FStartTime;
    Property CurrentTime: TDateTime Read GetCurrentTime Write SetCurrentTime;
    Property EndTime: TDateTime Read GetEndTime;

    Property AutoPlayWhenLoaded: Boolean Read FAutoPlayWhenLoaded Write FAutoPlayWhenLoaded;

    Property SyncSeekThresholdMS: TVideoTime Read FSyncSeekThresholdMS Write FSyncSeekThresholdMS;

    Property PlaybackClass: TfmeVideoBaseClass Read FPlaybackClass Write FPlaybackClass;
  End;

Implementation

Uses
  DateUtils, Math;

  {$R *.lfm}

  { TfmeSyncedVideo }

Constructor TfmeSyncedVideo.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  FVideos := TfmeVideoBaseList.Create(True); // Does own videos.
  FMaster := nil;
  FState := vsEmpty;

  // End user will have to provide an active class
  FPlaybackClass := nil;

  FStartTime := 0;
  FSyncSeekThresholdMS := 1000;
  FReadyToPlay := False;
  FAutoPlayWhenLoaded := True;

  FSyncTimer := TTimer.Create(Self);
  FSyncTimer.Enabled := False;
  FSyncTimer.Interval := 500;
  FSyncTimer.OnTimer := @SyncTimerTimer;

  FLayout := TVideoGridLayout.Create(Self);
  FLayout.RowCount := 2;
  FLayout.ColCount := 1;
  FLayout.Sequence := vlsLeftToRightThenDown;
  FLayout.PanelMargin := 0;
  FLayout.CellSpacing := 0;
End;

Destructor TfmeSyncedVideo.Destroy;
Begin
  FSyncTimer.Enabled := False;
  ClearVideos;
  FreeAndNil(FVideos);
  FreeAndNil(FLayout);
  FreeAndNil(FSyncTimer);

  Inherited Destroy;
End;

Procedure TfmeSyncedVideo.FrameResize(Sender: TObject);
Begin
  If Assigned(FLayout) Then
    FLayout.LayoutVideos(FVideos);
End;

Procedure TfmeSyncedVideo.SetState(AValue: TVideoState);
Begin
  If FState = AValue Then
    Exit;

  FState := AValue;
  DoStateChanged;
End;

Procedure TfmeSyncedVideo.SetMaster(AValue: TfmeVideoBase);
Var
  i: Integer;
Begin
  If FMaster = AValue Then
    Exit;

  If Assigned(FMaster) Then
    FMaster.OnPosition := nil;

  FMaster := AValue;

  If Assigned(FMaster) Then
  Begin
    FMaster.OnPosition := @MasterPosition;

    For i := 0 To FVideos.Count - 1 Do
      FVideos[i].Muted := FVideos[i] <> FMaster;

    SetState(FMaster.State);
  End
  Else
    SetState(vsEmpty);

  DoPosition;
End;

Procedure TfmeSyncedVideo.Layout(ARows, ACols: Integer; ASequence: TVideoLayoutSequence);
Begin
  FLayout.RowCount := ARows;
  FLayout.ColCount := ACols;
  FLayout.Sequence := ASequence;
  FLayout.LayoutVideos(FVideos);
End;

Procedure TfmeSyncedVideo.ClearVideos;
Var
  i: Integer;
Begin
  For i := 0 To FVideos.Count - 1 Do
  Begin
    FVideos[i].OnPosition := nil;
    FVideos[i].OnStateChanged := nil;
  End;

  Master := nil;
  FVideos.Clear;
  SetState(vsEmpty);
End;

Function TfmeSyncedVideo.GetPosition: TVideoTime;
Begin
  If Assigned(FMaster) Then
    Result := FMaster.Position
  Else
    Result := 0;
End;

Procedure TfmeSyncedVideo.SetPosition(AValue: TVideoTime);
Var
  i: Integer;
Begin
  For i := 0 To FVideos.Count - 1 Do
    If FVideos[i].CanSeek Then
      FVideos[i].Position := AValue;

  DoPosition;
End;

Function TfmeSyncedVideo.GetDuration: TVideoTime;
Begin
  If Assigned(FMaster) Then
    Result := FMaster.Duration
  Else
    Result := -1;
End;

Function TfmeSyncedVideo.GetRate: Double;
Begin
  If Assigned(FMaster) Then
    Result := FMaster.Rate
  Else
    Result := 1.0;
End;

Procedure TfmeSyncedVideo.SetRate(AValue: Double);
Var
  i: Integer;
Begin
  For i := 0 To FVideos.Count - 1 Do
    If FVideos[i].CanSetRate Then
      FVideos[i].Rate := AValue;
End;

Function TfmeSyncedVideo.GetState: TVideoState;
Begin
  Result := FState;
End;

Function TfmeSyncedVideo.GetCurrentTime: TDateTime;
Begin
  If FStartTime = 0 Then
    Result := 0
  Else
    Result := FStartTime + (Position / MSecsPerDay);
End;

Procedure TfmeSyncedVideo.SetCurrentTime(AValue: TDateTime);
Var
  OffsetMS: TVideoTime;
Begin
  If FStartTime = 0 Then
    Exit;

  OffsetMS := MilliSecondsBetween(AValue, FStartTime);

  If AValue < FStartTime Then
    OffsetMS := -OffsetMS;

  Position := OffsetMS;
End;

Function TfmeSyncedVideo.GetEndTime: TDateTime;
Begin
  If (FStartTime = 0) Or (Duration < 0) Then
    Result := 0
  Else
    Result := FStartTime + (Duration / MSecsPerDay);
End;

Function TfmeSyncedVideo.Load(Const AFilename: String): Boolean;
Var
  oVideo: TfmeVideoBase;
Begin
  Result := False;

  If Not Assigned(FPlaybackClass) Then
    Exit;

  oVideo := FPlaybackClass.Create(Nil);
  oVideo.Parent := Self;
  oVideo.OnStateChanged := @VideoStateChanged;

  FVideos.Add(oVideo);

  If FVideos.Count > (FLayout.RowCount * FLayout.ColCount) Then
    FLayout.ColCount := FLayout.ColCount + 1;

  If Not Assigned(FMaster) Then
  Begin
    FFilename := AFilename;
    Master := oVideo;
  End;

  oVideo.Muted := oVideo <> FMaster;

  Result := oVideo.Load(AFilename);


  FLayout.LayoutVideos(FVideos);
End;
Function TfmeSyncedVideo.Play: Boolean;
Var
  i: Integer;
Begin
  Result := FVideos.Count > 0;

  FReadyToPlay := False;

  For i := 0 To FVideos.Count - 1 Do
  Begin
    If FVideos[i].State = vsEmpty Then
      Continue;

    Result := FVideos[i].Play And Result;
  End;

  If Result Then
  Begin
    SetState(vsLoading);
    FSyncTimer.Enabled := False;
  End;
End;

Function TfmeSyncedVideo.Pause: Boolean;
Var
  i: Integer;
Begin
  Result := FVideos.Count > 0;

  For i := 0 To FVideos.Count - 1 Do
    Result := FVideos[i].Pause And Result;

  If Result Then
  Begin
    FSyncTimer.Enabled := False;
    SetState(vsPaused);
  End;
End;

Function TfmeSyncedVideo.Resume: Boolean;
Var
  i: Integer;
Begin
  Result := FVideos.Count > 0;

  For i := 0 To FVideos.Count - 1 Do
    Result := FVideos[i].Resume And Result;

  If Result Then
  Begin
    SetState(vsPlaying);
    FSyncTimer.Enabled := True;
  End;
End;

Function TfmeSyncedVideo.Stop: Boolean;
Var
  i: Integer;
Begin
  Result := FVideos.Count > 0;

  FSyncTimer.Enabled := False;

  For i := 0 To FVideos.Count - 1 Do
    Result := FVideos[i].Stop And Result;

  If Result Then
    SetState(vsStopped);
End;

Function TfmeSyncedVideo.CanSeek: Boolean;
Begin
  Result := Assigned(FMaster) And FMaster.CanSeek;
End;

Function TfmeSyncedVideo.CanSetRate: Boolean;
Begin
  Result := Assigned(FMaster) And FMaster.CanSetRate;
End;

Function TfmeSyncedVideo.CanGrabBitmap: Boolean;
Begin
  Result := Assigned(FMaster) And FMaster.CanGrabBitmap;
End;

Function TfmeSyncedVideo.GetBitmap(Bitmap: TBitmap): Boolean;
Begin
  If Assigned(FMaster) Then
    Result := FMaster.GetBitmap(Bitmap)
  Else
    Result := False;
End;

Procedure TfmeSyncedVideo.MasterPosition(Sender: TObject; PositionMS, DurationMS: TVideoTime);
Begin
  DoPosition;
End;

Function TfmeSyncedVideo.LoadedVideoCount: Integer;
Var
  i: Integer;
Begin
  Result := 0;

  For i := 0 To FVideos.Count - 1 Do
    If FVideos[i].State = vsPaused Then
      Inc(Result);
End;

Function TfmeSyncedVideo.AllVideosLoaded: Boolean;
Begin
  Result :=
    (FVideos.Count > 0) And (LoadedVideoCount = FVideos.Count);
End;

Procedure TfmeSyncedVideo.CheckAllVideosLoaded;
Begin
  If AllVideosLoaded Then
  Begin
    FReadyToPlay := True;
    FLayout.LayoutVideos(FVideos);
    DoPosition;

    If FAutoPlayWhenLoaded Then
      Resume
    Else
    Begin
      FSyncTimer.Enabled := False;
      SetState(vsPaused);
    End;
  End
  Else
    SetState(vsLoading);
End;

Procedure TfmeSyncedVideo.VideoStateChanged(Sender: TObject; AState: TVideoState);
Begin
  Case AState Of
    vsLoading:
      SetState(vsLoading);

    vsError:
    Begin
      FSyncTimer.Enabled := False;
      SetState(vsError);
    End;

    vsPlaying:
    Begin
      If FReadyToPlay Then
      Begin
        SetState(vsPlaying);
        FSyncTimer.Enabled := True;
      End
      Else
      Begin
        SetState(vsLoading);
        FSyncTimer.Enabled := False;
      End;
    End;

    vsPaused, vsStopped:
      CheckAllVideosLoaded;

    vsEnded:
    Begin
      FSyncTimer.Enabled := False;
      SetState(vsEnded);
    End;

    vsEmpty:
    Begin
      FSyncTimer.Enabled := False;
      SetState(vsEmpty);
    End;
  End;
End;

Procedure TfmeSyncedVideo.SyncTimerTimer(Sender: TObject);
Begin
  SyncVideos;
End;

Procedure TfmeSyncedVideo.SyncVideos;
Var
  i: Integer;
  MasterPos: TVideoTime;
  SlavePos: TVideoTime;
  DriftMS: TVideoTime;
  Slave: TfmeVideoBase;
Begin
  If Not Assigned(FMaster) Then
    Exit;

  If FMaster.State <> vsPlaying Then
    Exit;

  MasterPos := FMaster.Position;

  For i := 0 To FVideos.Count - 1 Do
  Begin
    Slave := FVideos[i];

    If Slave = FMaster Then
      Continue;

    If Not Slave.CanSeek Then
      Continue;

    If Not (Slave.State In [vsPlaying, vsPaused]) Then
      Continue;

    SlavePos := Slave.Position;
    DriftMS := Abs(MasterPos - SlavePos);

    If DriftMS > FSyncSeekThresholdMS Then
      Slave.Position := MasterPos;
  End;
End;

End.
