Unit FrameVideoLibmpv;

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics,
  FrameVideoBase, MPVPlayer;

Type

  { TfmeVideoLibmpv }

  TfmeVideoLibmpv = Class(TfmeVideoBase)
  Private
    FmpvPlayer: TMPVPlayer;
    FState: TVideoState;

    Procedure SetState(AValue: TVideoState);

    Procedure mpvStartFile(Sender: TObject);
    Procedure mpvFileLoaded(Sender: TObject);
    Procedure mpvPlay(Sender: TObject);
    Procedure mpvPause(Sender: TObject);
    Procedure mpvStop(Sender: TObject);
    Procedure mpvTimeChanged(ASender: TObject; AParam: Integer);

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

    Function Load(Const AFilename: String): Boolean; Override;
    Function Play: Boolean; Override;
    Function Pause: Boolean; Override;
    Function Resume: Boolean; Override;
    Function Stop: Boolean; Override;

    Function CanSeek: Boolean; Override;
    Function CanSetRate: Boolean; Override;
    Function CanGrabBitmap: Boolean; Override;
    Function GetBitmap(Bitmap: TBitmap): Boolean; Override;
  End;

Implementation

Uses
  LibmpvSupport;

{$R *.lfm}

{ TfmeVideoLibmpv }

Constructor TfmeVideoLibmpv.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  FState := vsEmpty;

  FmpvPlayer := TMPVPlayer.Create(Self);
  FmpvPlayer.Parent := Self;
  FmpvPlayer.Align := alClient;

  FmpvPlayer.AutoStartPlayback := False;
  FmpvPlayer.AutoLoadSubtitle := False;
  FmpvPlayer.KeepAspect := True;
  FmpvPlayer.NoAudioDisplay := False;
  FmpvPlayer.RendererMode := rmEmbedding;
  FmpvPlayer.RenderFailAction := rfNone;
  FmpvPlayer.LogLevel := llStatus;

  FmpvPlayer.OnStartFile := @mpvStartFile;
  FmpvPlayer.OnFileLoaded := @mpvFileLoaded;
  FmpvPlayer.OnPlay := @mpvPlay;
  FmpvPlayer.OnPause := @mpvPause;
  FmpvPlayer.OnStop := @mpvStop;
  FmpvPlayer.OnTimeChanged := @mpvTimeChanged;
End;

Destructor TfmeVideoLibmpv.Destroy;
Begin
  If Assigned(FmpvPlayer) Then
  Begin
    If FmpvPlayer.IsMediaLoaded Then
      FmpvPlayer.Close(True);
  End;

  Inherited Destroy;
End;

Procedure TfmeVideoLibmpv.SetState(AValue: TVideoState);
Begin
  If FState = AValue Then
    Exit;

  FState := AValue;
  DoStateChanged;
End;

Function TfmeVideoLibmpv.GetPosition: TVideoTime;
Begin
  If Assigned(FmpvPlayer) And FmpvPlayer.IsMediaLoaded Then
    Result := FmpvPlayer.GetMediaPosInMs
  Else
    Result := 0;
End;

Procedure TfmeVideoLibmpv.SetPosition(AValue: TVideoTime);
Begin
  If CanSeek Then
    FmpvPlayer.SeekInMs(AValue);
End;

Function TfmeVideoLibmpv.GetDuration: TVideoTime;
Begin
  If Assigned(FmpvPlayer) And FmpvPlayer.IsMediaLoaded Then
    Result := FmpvPlayer.GetMediaLenInMs
  Else
    Result := -1;
End;

Function TfmeVideoLibmpv.GetRate: Double;
Begin
  Result := 1.0; // TODO: wire to mpv speed property if needed.
End;

Procedure TfmeVideoLibmpv.SetRate(AValue: Double);
Begin
  // TODO: wire to mpv speed property if needed.
End;

Function TfmeVideoLibmpv.GetState: TVideoState;
Begin
  Result := FState;
End;

Function TfmeVideoLibmpv.Load(Const AFilename: String): Boolean;
Begin
  Result := Inherited Load(AFilename);

  If Not Result Then
  Begin
    SetState(vsError);
    Exit;
  End;

  If FmpvPlayer.IsMediaLoaded Then
    FmpvPlayer.Stop;

  SetState(vsStopped);
End;

Function TfmeVideoLibmpv.Play: Boolean;
Begin
  Result := False;

  If FFilename = '' Then
    Exit;

  If Not FileExists(FFilename) Then
  Begin
    SetState(vsError);
    Exit;
  End;

  SetState(vsLoading);
  FmpvPlayer.Play(FFilename);

  Result := True;
End;

Function TfmeVideoLibmpv.Pause: Boolean;
Begin
  Result := False;

  If FmpvPlayer.IsMediaLoaded And FmpvPlayer.IsPlaying Then
  Begin
    FmpvPlayer.Pause;
    Result := True;
  End;
End;

Function TfmeVideoLibmpv.Resume: Boolean;
Begin
  Result := False;

  If FmpvPlayer.IsMediaLoaded And FmpvPlayer.IsPaused Then
  Begin
    FmpvPlayer.Resume(True);
    Result := True;
  End;
End;

Function TfmeVideoLibmpv.Stop: Boolean;
Begin
  Result := False;

  If FmpvPlayer.IsMediaLoaded Then
  Begin
    FmpvPlayer.Stop;
    Result := True;
  End;
End;

Function TfmeVideoLibmpv.CanSeek: Boolean;
Begin
  Result := Assigned(FmpvPlayer) And FmpvPlayer.IsMediaLoaded;
End;

Function TfmeVideoLibmpv.CanSetRate: Boolean;
Begin
  Result := False;
End;

Function TfmeVideoLibmpv.CanGrabBitmap: Boolean;
Begin
  Result := False;
End;

Function TfmeVideoLibmpv.GetBitmap(Bitmap: TBitmap): Boolean;
Begin
  Result := False;
End;

Procedure TfmeVideoLibmpv.mpvStartFile(Sender: TObject);
Begin
  SetState(vsLoading);
End;

Procedure TfmeVideoLibmpv.mpvFileLoaded(Sender: TObject);
Begin
  DoPosition;
  SetState(vsPaused);
End;

Procedure TfmeVideoLibmpv.mpvPlay(Sender: TObject);
Begin
  SetState(vsPlaying);
End;

Procedure TfmeVideoLibmpv.mpvPause(Sender: TObject);
Begin
  SetState(vsPaused);
End;

Procedure TfmeVideoLibmpv.mpvStop(Sender: TObject);
Begin
  SetState(vsStopped);
End;

Procedure TfmeVideoLibmpv.mpvTimeChanged(ASender: TObject; AParam: Integer);
Begin
  DoPosition;
End;

Initialization
  InitializeLibmpv;

End.
