Unit FrameVideoDirectShow;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  DSPack, FrameVideoBase;

Type

  { TFrameDirectShowVideo }

  TFrameDirectShowVideo = Class(TfmeVideoBase)
    pnlVideo: TPanel;
  Protected
    FDSVideoWindow: TVideoWindow;
    FDSFilterGraph: TFilterGraph;
    //FOnTimer: TTimerEvent;
    FSampleGrabber: TSampleGrabber;
    FDSTrackBar: TDSTrackBar;

    FDuration: TDateTime;

    Function GetPosition: Integer; Override;
    Function GetRate: Double; Override;
    Procedure SetFilename(AValue: String); Override;
    Procedure CreateObjects;
    Procedure SetPosition(AValue: Integer); Override;
    Procedure SetRate(AValue: Double); Override;
  Public
    Constructor Create(TheOwner: TComponent); Override;
    Destructor Destroy; Override;

    Function Play: Boolean; Override;
    Function Pause: Boolean; Override;
    Function Resume: Boolean; Override;
    Function Stop: Boolean; Override;

    Function GraphLoaded: Boolean;
    Function Paused: Boolean; Override;

    Function Duration: TDateTime; Override;

    Function CanRewind: Boolean; Override;
    Function GetBitmap(Bitmap: TBitmap): Boolean; Override;

    Property OnTimer: TTimerEvent read FOnTimer write FOnTimer;
  End;

Implementation

Uses
  ActiveX, DirectShow9;

Constructor TFrameDirectShowVideo.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  FFilename := '';
  FDuration := -1;

  FDSVideoWindow := nil;
  FDSFilterGraph := nil;
  FSampleGrabber := nil;
  FDSTrackBar := nil;
  FOnTimer := nil;
End;

Destructor TFrameDirectShowVideo.Destroy;
Begin
  If FDSFilterGraph <> nil Then
  Begin
    If FDSFilterGraph.Active Then
      FDSFilterGraph.Active := False;

    FDSFilterGraph.ClearGraph;

    FDSTrackBar.FilterGraph := nil;
    FDSTrackBar.Free;

    FDSVideoWindow.FilterGraph := nil;
    FDSVideoWindow.Free;

    FSampleGrabber.FilterGraph := nil;
    FSampleGrabber.Free;

    FDSFilterGraph.Free;
    FDSFilterGraph := nil;
  End;

  Inherited Destroy;
End;

Procedure TFrameDirectShowVideo.SetFilename(AValue: String);
Begin
  If Assigned(FDSFilterGraph) Then
    If FDSFilterGraph.Active Then
      FDSFilterGraph.Active := False;

  If (FFilename <> AValue) Then
    If FileExists(AValue) Then
    Begin
      If FDSFilterGraph = nil Then
        CreateObjects;

      FFilename := AValue;
    End
    Else
      FFilename := '';

End;

Procedure TFrameDirectShowVideo.CreateObjects;
Begin
  FDSFilterGraph := TFilterGraph.Create(Self);

  FDSVideoWindow := TVideoWindow.Create(Self);
  FDSVideoWindow.Parent := pnlVideo;
  FDSVideoWindow.Align := alClient;
  FDSVideoWindow.Mode := vmVMR;
  FDSVideoWindow.VMROptions.KeepAspectRatio := True;
  FDSVideoWindow.FilterGraph := FDSFilterGraph;

  FDSTrackBar := TDSTrackBar.Create(Self);
  FDSTrackBar.Parent := pnlVideo;
  FDSTrackBar.Align := alBottom;
  FDSTrackBar.TimerInterval := 300;
  FDSTrackBar.OnTimer := FOnTimer;
  FDSTrackBar.FilterGraph := FDSFilterGraph;

  FSampleGrabber := TSampleGrabber.Create(Self);
  FSampleGrabber.FilterGraph := FDSFilterGraph;
End;

Function TFrameDirectShowVideo.Play: Boolean;
Const
  CLSID_MOONLIGHT_ELECARD_DECODER_2: TGUID = '{F50B3F13-19C4-11CF-AA9A-02608C9BABA2}';
  CLSID_MOONLIGHT_ELECARD_DECODER_4: TGUID = '{BC4EB321-771F-4E9F-AF67-37C631ECA106}';
Var
  oFilter: IBaseFilter;
  sExt: String;
Begin
  Result := False;

  If Assigned(FDSFilterGraph) And FileExists(FFilename) Then
    If FDSFilterGraph.State = gsPaused Then
      Result := FDSFilterGraph.Play
    Else
    Begin
      FDuration := -1;

      If FDSFilterGraph.Active Then
        FDSFilterGraph.Active := False;

      oFilter := nil;

      FDSFilterGraph.Active := True;

      sExt := Lowercase(ExtractFileExt(FFilename));

      // Having to do this in case Microsoft DVD Decoder is in town...
      If (sExt = '.mpg') Or (sExt = '.pkt') Then
        If SUCCEEDED(CoCreateInstance(CLSID_MOONLIGHT_ELECARD_DECODER_2,
          nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, oFilter)) Then
          FDSFilterGraph.AddFilter(oFilter, 'Elecard MPEG2 Video Decoder')
        Else If SUCCEEDED(CoCreateInstance(CLSID_MOONLIGHT_ELECARD_DECODER_4,
          nil, CLSCTX_INPROC_SERVER, IID_IBaseFilter, oFilter)) Then
          FDSFilterGraph.AddFilter(oFilter, 'Elecard MPEG2 Video Decoder');

      FDSFilterGraph.RenderFile(WideString(FFilename));

      Result := FDSFilterGraph.Play;
    End;
End;

Function TFrameDirectShowVideo.Pause: Boolean;
Begin
  Result := False;

  If Assigned(FDSFilterGraph) Then
    If FDSFilterGraph.Active Then
      Result := FDSFilterGraph.Pause;
End;

Function TFrameDirectShowVideo.Resume: Boolean;
Begin
  Result := False;

  If Assigned(FDSFilterGraph) Then
    If FDSFilterGraph.Active And Paused Then
      Result := FDSFilterGraph.Play;
End;

Function TFrameDirectShowVideo.Stop: Boolean;
Begin
  Result := False;

  If Assigned(FDSFilterGraph) Then
    If FDSFilterGraph.Active Then
      Result := FDSFilterGraph.Stop;
End;

Function TFrameDirectShowVideo.GraphLoaded: Boolean;
Begin
  Result := False;

  If Assigned(FDSFilterGraph) Then
    If FDSFilterGraph.Active Then
      Result := (FDSFilterGraph.State In [gsPaused, gsPlaying]);
End;

Function TFrameDirectShowVideo.Paused: Boolean;
Begin
  Result := False;

  If Assigned(FDSFilterGraph) Then
    If FDSFilterGraph.Active Then
      Result := FDSFilterGraph.State = gsPaused;
End;


Function TFrameDirectShowVideo.Duration: TDateTime;
Begin
  Result := 0;

  If Assigned(FDSFilterGraph) Then
    If Assigned(FDSFilterGraph) Then
      If FDSFilterGraph.Active Then
      Begin
        If FDuration = -1 Then
          Result := (FDSFilterGraph.Duration / 1000) / (24 * 60 * 60)
        Else
          Result := FDuration;

        FDuration := Result;
      End;
End;

Function TFrameDirectShowVideo.CanRewind: Boolean;
Begin
  If GraphLoaded Then
    Result := CanPlayBackwards In FDSFilterGraph.SeekCapabilities
  Else
    Result := False;
End;

Function TFrameDirectShowVideo.GetBitmap(Bitmap: TBitmap): Boolean;
Begin
  If Assigned(FDSFilterGraph) And (FDSFilterGraph.Active) Then
    Result := FSampleGrabber.GetBitmap(Bitmap)
  Else
    Result := False;
End;

Procedure TFrameDirectShowVideo.SetRate(AValue: Double);
Begin
  If GraphLoaded Then
    FDSFilterGraph.Rate := AValue;
End;

Function TFrameDirectShowVideo.GetRate: Double;
Begin
  If GraphLoaded Then
    Result := FDSFilterGraph.Rate
  Else
    Result := 0;
End;

Function TFrameDirectShowVideo.GetPosition: Integer;
Begin
  If GraphLoaded Then
    Result := FDSFilterGraph.Position
  Else
    Result := 0;
End;

Procedure TFrameDirectShowVideo.SetPosition(AValue: Integer);
Begin
  If GraphLoaded Then
    FDSFilterGraph.Position := AValue;
End;


{$R *.lfm}

End.
