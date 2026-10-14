Unit WaveformCache;

{$mode ObjFPC}{$H+}

{ Thinking out loud

  The goal of this unit is to be the mechanism that starts with a video filename
  and ends up delivering a TWaveBuckets

  I want some caching, so we're not constantly regenerating the same data as the
  user skips back and forth investigating someting that starts at the end of one video
  and continues into the next

  I want threading so that the cost of audio extraction and processing is hidden from
  the user

  The TTimeTrackback and TAudioWaveformRenderer plugin has already been developed so
  the UI can deal with the gap while not having the waveform data

  AudioWaveform.TWavInfo (record)
  AudioWaveform.TWaveformBucket (packed record)
  AudioWaveform.TWaveformBuckets = Array Of TWaveformBucket;

  Function AudioWaveform.ReadWaveform(Const AFilename: String; Out AWavInfo: TWavInfo;
  Out ABuckets: TWaveformBuckets; ABucketMilliseconds: Integer = 20): Boolean;

  Function Libmpv_ExtractAudio(AInputVideoFile: String; AOutputWaveFile: String): Boolean;

  FLock.Acquire;
  try
    Request := PopNewestRequest;
  finally
    FLock.Release;
  end;

  // NO LOCK HERE
  Libmpv_ExtractAudio(...);
  ReadWaveform(...);

  // briefly lock again to put result into cache
  FLock.Acquire;
  try
    AddToCache(...);
  finally
    FLock.Release;
  end;

  FLock
    protects FStack/FCache while they're being touched

  FWorkAvailable
      lets Manager wake Worker when FStack has work

  FThread
      does the actual extraction/processing
}

Interface

Uses
  Classes, SysUtils, AudioWaveform, fgl, LibmpvSupport, SyncObjs;

Type
  TOnWaveformReady = Procedure(Sender: TObject; AVideofilename: String;
    AWaveformBuckets: TWaveformBuckets) Of Object;

  // Thread will act on the last item in the stack
  TWaveformRequest = Class
    VideoFilename: String;
    BucketCount: Integer;
  End;

  TStackItemList = Class(Specialize TFPGObjectList<TWaveformRequest>);

  // Manager will hold a cache of processed WaveformBuckets
  TWaveformCacheItem = Class
    VideoFilename: String;
    LastAccess: TDateTime;
    WaveformBuckets: TWaveformBuckets;
  End;

  TCacheItemList = Class(Specialize TFPGObjectList<TWaveformCacheItem>)
  Public
    Function Find(AVideofilename: String): TWaveformCacheItem;
  End;

  TWaveformManager = Class;

  { TWaveformThread }

  TWaveformThread = Class(TThread)
    FManager: TWaveformManager;
    FResults: TCacheItemList;
    FLock: TCriticalSection;
    FWorkAvailable: TEvent;
    FStack: TStackItemList;
    FTempFolder: String;
  Private
    Procedure DeliverResults;
    Function PopRequest(Var AWaveformRequest: TWaveformRequest): Boolean;
    Procedure ProcessRequest(AWaveformRequest: TWaveformRequest);
  Protected
    Procedure Execute; Override;
  Public
    Constructor Create(AManager: TWaveformManager; AStack: TStackItemList;
      ALock: TCriticalSection; AWorkAvailable: TEvent; ATempFolder: String);
    Destructor Destroy; Override;
  End;

  // Main worker class

  { TWaveformManager }

  TWaveformManager = Class
  Private
    FCache: TCacheItemList;
    FStack: TStackItemList;
    FOnWaveformReady: TOnWaveformReady;

    FLock: TCriticalSection;
    FWorkAvailable: TEvent;
    FThread: TWaveformThread;

    FTempFolder: String;

    Procedure PurgeCache;
  Public
    Constructor Create(ATempFolder: String);
    Destructor Destroy; Override;

    Procedure RequestWaveform(AVideofilename: String; ABucketCount: Integer);
    Procedure ReceiveWaveform(AVideofilename: String; AWaveformBuckets: TWaveformBuckets);

    Property OnWaveformReady: TOnWaveformReady Read FOnWaveformReady Write FOnWaveformReady;
  End;


Implementation

Uses
  Math;

  { TWaveformManager }

Constructor TWaveformManager.Create(ATempFolder: String);
Begin
  FTempFolder := ATempFolder;

  FCache := TCacheItemList.Create(True);
  FStack := TStackItemList.Create(True);

  FWorkAvailable := TEvent.Create({EventAttributes=}nil, {ManualReset=}False,
    {InitialState=}False,  {Name=}'FWorkAvailable');

  FLock := TCriticalSection.Create;

  // Conceptual
  FThread := TWaveformThread.Create(Self, FStack, FLock, FWorkAvailable, FTempFolder);
End;

Destructor TWaveformManager.Destroy;
Begin
  If Assigned(FThread) Then
  Begin
    // 1. Ask worker to stop
    FThread.Terminate;

    // 2. Wake it if it's sleeping in WaitFor
    FWorkAvailable.SetEvent;

    // 3. Wait until Execute has actually exited
    FThread.WaitFor;

    // 4. Only now can we destroy things the thread references
    FreeAndNil(FThread);
  End;

  FreeAndNil(FStack);
  FreeAndNil(FCache);

  FreeAndNil(FWorkAvailable);
  FreeAndNil(FLock);

  Inherited Destroy;
End;

Procedure TWaveformManager.RequestWaveform(AVideofilename: String; ABucketCount: Integer);
Var
  oCacheItem: TWaveformCacheItem;
  oStackItem: TWaveformRequest;
Begin
  // Find returns a COPY of the data
  oCacheItem := FCache.Find(AVideofilename);

  If Assigned(oCacheItem) Then
  Begin
    oCacheItem.LastAccess := Now;
    If Assigned(FOnWaveformReady) Then
      FOnWaveformReady(Self, AVideofilename, Copy(oCacheItem.WaveformBuckets));
  End
  Else
  Begin
    // Add to Stack for the Thread
    oStackItem := TWaveformRequest.Create;
    oStackItem.VideoFilename := AVideofilename;
    oStackItem.BucketCount := ABucketCount;

    FLock.Acquire;
    Try
      FStack.Add(oStackItem);
    Finally
      FLock.Release;
    End;

    // Notify the thread there's new data on the stack
    FWorkAvailable.SetEvent;
  End;

  PurgeCache;
End;

Procedure TWaveformManager.ReceiveWaveform(AVideofilename: String;
  AWaveformBuckets: TWaveformBuckets);
Var
  oCacheItem: TWaveformCacheItem;
Begin
  // Find returns a COPY of the data
  oCacheItem := FCache.Find(AVideofilename);
  If Assigned(oCacheItem) Then
    oCacheItem := FCache.Extract(oCacheItem)
  Else
    oCacheItem := TWaveformCacheItem.Create;

  oCacheItem.VideoFilename := AVideofilename;
  oCacheItem.WaveformBuckets := AWaveformBuckets;
  oCacheItem.LastAccess := Now;
  FCache.Add(oCacheItem);

  If Assigned(FOnWaveformReady) Then
    FOnWaveformReady(Self, AVideofilename, Copy(AWaveformBuckets));
End;

Procedure TWaveformManager.PurgeCache;
Const
  FIVE_MINUTES = 5 / MinsPerDay;
Var
  oItem: TWaveformCacheItem;
  i: Integer;
Begin
  For i := FCache.Count - 1 Downto 0 Do
  Begin
    oItem := FCache[i];
    If (Now - oItem.LastAccess) > FIVE_MINUTES Then
      FCache.Delete(i);
  End;
End;

{ TCacheItemList }

Function TCacheItemList.Find(AVideofilename: String): TWaveformCacheItem;
Var
  oItem: TWaveformCacheItem;
Begin
  Result := nil;

  For oItem In Self Do
    If Sametext(oItem.VideoFilename, AVideofilename) Then
      Exit(oItem);
End;

{ TWaveformThread }

Constructor TWaveformThread.Create(AManager: TWaveformManager; AStack: TStackItemList;
  ALock: TCriticalSection; AWorkAvailable: TEvent; ATempFolder: String);
Begin
  Inherited Create(True);

  FManager := AManager;
  FStack := AStack;
  FLock := ALock;
  FWorkAvailable := AWorkAvailable;
  FTempFolder := ATempFolder;

  FResults := TCacheItemList.Create(True);

  Start;
End;

Destructor TWaveformThread.Destroy;
Begin
  FreeAndNil(FResults);

  Inherited Destroy;
End;

// And when Thread somehow returns that is has a result
//    oItem := Default(TCacheItem);
//    oItem.LastAccess := now;
//    oItem.VideoFilename := AVideofilename;
//    oItem.WaveformBuckets := FWaveBuckets;
//    FCache.LastAccess(oItem);
//    If Assigned(FOnWaveformReady) Then
//      FOnWaveformReady(Self, AVideofilename, Copy(FWaveBuckets));

Procedure TWaveformThread.Execute;
Var
  oRequest: TWaveformRequest;
Begin
  While Not Terminated Do
  Begin
    FWorkAvailable.WaitFor(INFINITE);

    If Terminated Then
      Break;

    oRequest := nil;

    While PopRequest(oRequest) Do
    Begin
      Try
        ProcessRequest(oRequest);
      Finally
        oRequest.Free;
      End;

      If Terminated Then
        Break;
    End;
  End;
End;

Function TWaveformThread.PopRequest(Var AWaveformRequest: TWaveformRequest): Boolean;
Begin
  Result := False;
  AWaveformRequest := nil;

  FLock.Acquire;
  Try
    If FStack.Count = 0 Then
      Exit;

    AWaveformRequest := FStack.Extract(FStack.Last);
    Result := True;
  Finally
    FLock.Release;
  End;
End;

Procedure TWaveformThread.ProcessRequest(AWaveformRequest: TWaveformRequest);
Var
  oWavInfo: TWavInfo;
  dDuration_mS: Extended;
  iBucket_mS: Integer;
  oWaveBuckets: TWaveformBuckets;
  oCacheItem: TWaveformCacheItem;
  sAudioFile: String;
Begin
  sAudioFile := IncludeTrailingPathDelimiter(FTempFolder) +
    ChangeFileExt(ExtractFileName(AWaveformRequest.VideoFilename), '.wav');

  If FileExists(sAudioFile) Then
    DeleteFile(sAudioFile);

  oWavInfo := Default(TWavInfo);

  // TODO: Handle failure modes
  If Libmpv_ExtractAudio(AWaveformRequest.VideoFilename, sAudioFile) Then
    If ReadWavHeader(sAudioFile, oWavInfo) Then
    Begin
      dDuration_mS := 1000 * (oWavInfo.DataSize / oWavInfo.ByteRate);
      iBucket_mS := Max(1, Ceil(dDuration_mS / AWaveformRequest.BucketCount));
      ReadWaveform(sAudioFile, oWavInfo, oWaveBuckets, iBucket_mS);
    End;

  If FileExists(sAudioFile) Then
    DeleteFile(sAudioFile);

  // Use FResults to get this back to the main thread
  oCacheItem := TWaveformCacheItem.Create;
  oCacheItem.VideoFilename := AWaveformRequest.VideoFilename;
  oCacheItem.WaveformBuckets := oWaveBuckets;

  FLock.Acquire;
  Try
    FResults.Add(oCacheItem);
  Finally
    FLock.Release;
  End;

  TThread.Queue(Self, @DeliverResults);
End;

// This procedure happens on the Main Thread.
Procedure TWaveformThread.DeliverResults;
Var
  oResult: TWaveformCacheItem;
Begin
  While True Do
  Begin
    FLock.Acquire;
    Try
      If FResults.Count = 0 Then
        Exit;

      oResult := FResults.Extract(FResults.Last);
    Finally
      FLock.Release;
    End;

    Try
      FManager.ReceiveWaveform(oResult.VideoFilename, oResult.WaveformBuckets);
    Finally
      oResult.Free;
    End;
  End;
End;

End.
