Unit FrameSyncedVideo;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, ExtCtrls, FGL,
  FrameVideoBase;

Type
  TfmeVideoBaseList = Specialize TFPGObjectList<TfmeVideoBase>;

  { TfmeSyncedVideo }

  TfmeSyncedVideo = Class(TfmeVideoBase)
  Private
    FVideos: TfmeVideoBaseList;
    FMaster: TfmeVideoBase;
    FState: TVideoState;
    FSyncTimer: TTimer;
    FSyncSeekThresholdMS: TVideoTime;
    FStartTime: TDateTime;

    Procedure SetState(AValue: TVideoState);
    Procedure SetMaster(AValue: TfmeVideoBase);

    Function GetCurrentTime: TDateTime;
    Procedure SetCurrentTime(AValue: TDateTime);
    Function GetEndTime: TDateTime;

    Procedure MasterPosition(Sender: TObject; PositionMS, DurationMS: TVideoTime);
    Procedure MasterStateChanged(Sender: TObject; AState: TVideoState);

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

    Function AddVideo(AVideo: TfmeVideoBase): Integer;
    Procedure ClearVideos;

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

    Property SyncSeekThresholdMS: TVideoTime Read FSyncSeekThresholdMS Write FSyncSeekThresholdMS;
  End;

Implementation

Uses
  DateUtils, Math;

  {$R *.lfm}

  { TfmeSyncedVideo }

Constructor TfmeSyncedVideo.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  FVideos := TfmeVideoBaseList.Create(False); // Does NOT own videos.
  FMaster := nil;
  FState := vsEmpty;

  FStartTime := 0;
  FSyncSeekThresholdMS := 1000;

  FSyncTimer := TTimer.Create(Self);
  FSyncTimer.Enabled := False;
  FSyncTimer.Interval := 500;
  FSyncTimer.OnTimer := @SyncTimerTimer;
End;

Destructor TfmeSyncedVideo.Destroy;
Begin
  FSyncTimer.Enabled := False;
  ClearVideos;
  FreeAndNil(FVideos);

  Inherited Destroy;
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
  Begin
    FMaster.OnPosition := nil;
    FMaster.OnStateChanged := nil;
  End;

  FMaster := AValue;

  If Assigned(FMaster) Then
  Begin
    FMaster.OnPosition := @MasterPosition;
    FMaster.OnStateChanged := @MasterStateChanged;
    SetState(FMaster.State);

    For i := 0 To FVideos.Count - 1 Do
      FVideos[i].Muted := FVideos[i] <> FMaster;
  End
  Else
    SetState(vsEmpty);

  DoPosition;
End;
Function TfmeSyncedVideo.AddVideo(AVideo: TfmeVideoBase): Integer;
Begin
  Result := -1;

  If Not Assigned(AVideo) Then
    Exit;

  Result := FVideos.Add(AVideo);

  If Not Assigned(FMaster) Then
    Master := AVideo;

  AVideo.Muted := AVideo <> FMaster;
End;

Procedure TfmeSyncedVideo.ClearVideos;
Begin
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
Begin
  // For synced video, loading individual files should usually be handled
  // by the owner before AddVideo, or by a later LoadSet method.
  FFilename := AFilename;
  Result := False;
End;

Function TfmeSyncedVideo.Play: Boolean;
Var
  i: Integer;
Begin
  Result := FVideos.Count > 0;

  For i := 0 To FVideos.Count - 1 Do
    Result := FVideos[i].Play And Result;

  If Result Then
  Begin
    SetState(vsPlaying);
    FSyncTimer.Enabled := True;
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

Procedure TfmeSyncedVideo.MasterStateChanged(Sender: TObject; AState: TVideoState);
Begin
  SetState(AState);

  FSyncTimer.Enabled := AState = vsPlaying;
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
