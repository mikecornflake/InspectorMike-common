Unit AudioWaveform;

// pcm wave file routines

{$mode ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Graphics, TimeTrackbar;

Type

  { TWavInfo }

  TWavInfo = Record
    AudioFormat: Word;
    Channels: Word;
    SampleRate: Cardinal;
    ByteRate: Cardinal;
    BlockAlign: Word;
    BitsPerSample: Word;

    // WAVE_FORMAT_EXTENSIBLE
    ExtensionSize: Word;
    ValidBitsPerSample: Word;
    ChannelMask: Cardinal;
    SubFormat: TGUID;

    DataOffset: Int64;
    DataSize: Cardinal;
  End;

  { TWaveformBucket }

  TWaveformBucket = Packed Record
    MinValue: Single;
    MaxValue: Single;
  End;

  TWaveformBuckets = Array Of TWaveformBucket;

  { TAudioWaveformRenderer }

  TAudioWaveformRenderer = Class(TRenderer)
  Private
    FColor: TColor;
    FPeakMin, FPeakMax: Single;
    FPeakModifier: Single;
    FNormalise: Boolean;
    FWaveBuckets: TWaveformBuckets;
    Procedure SetWaveBuckets(Const AValue: TWaveformBuckets);
  Public
    Constructor Create;
    Procedure Paint(ACanvas: TCanvas; Const ARect: TRect); Override;

    Property WaveBuckets: TWaveformBuckets Read FWaveBuckets Write SetWaveBuckets;
    Property Normalise: Boolean Read FNormalise Write FNormalise;
    Property PeakModifier: Single Read FPeakModifier Write FPeakModifier;
    Property Color: TColor Read FColor Write FColor;
  End;

Function ReadWavHeader(Const AFilename: String; Out AWavInfo: TWavInfo): Boolean;
Function ReadWaveform(Const AFilename: String; Out AWavInfo: TWavInfo;
  Out ABuckets: TWaveformBuckets; ABucketMilliseconds: Integer = 20): Boolean;

Implementation

Uses
  Math;

Const
  WAVE_FORMAT_PCM = $0001;
  WAVE_FORMAT_IEEE_FLOAT = $0003;
  WAVE_FORMAT_EXTENSIBLE = $FFFE;
  READ_BUFFER_SIZE = 1024 * 1024;  // 1 MB

{ ---------------------------------------------------------------------------
  ReadWavHeader

  Reads the RIFF/WAVE structure and locates the fmt and data chunks.

  This deliberately does not assume that WAV audio starts at byte 44.
  --------------------------------------------------------------------------- }
Function ReadWavHeader(Const AFilename: String; Out AWavInfo: TWavInfo): Boolean;
Var
  Stream: TFileStream;
  ChunkID: Array[0..3] Of Ansichar;
  ChunkSize: Cardinal;
  RiffSize: Cardinal;
  FoundFmt: Boolean;
  FoundData: Boolean;

  Function ChunkIs(Const AText: Ansistring): Boolean;
  Begin
    Result :=
      (Length(AText) = 4) And (ChunkID[0] = AText[1]) And (ChunkID[1] = AText[2]) And
      (ChunkID[2] = AText[3]) And (ChunkID[3] = AText[4]);
  End;

Begin
  Result := False;
  AWavInfo := Default(TWavInfo);
  RiffSize := 0;
  ChunkSize := 0;

  FillChar(AWavInfo, SizeOf(AWavInfo), 0);

  If Not FileExists(AFilename) Then
    Exit;

  Stream := TFileStream.Create(AFilename, fmOpenRead Or fmShareDenyNone);
  Try
    // -----------------------------------------------------------------------
    // RIFF header
    // -----------------------------------------------------------------------

    If Stream.Read(ChunkID, SizeOf(ChunkID)) <> SizeOf(ChunkID) Then
      Exit;

    If Not ChunkIs('RIFF') Then
      Exit;

    If Stream.Read(RiffSize, SizeOf(RiffSize)) <> SizeOf(RiffSize) Then
      Exit;

    If Stream.Read(ChunkID, SizeOf(ChunkID)) <> SizeOf(ChunkID) Then
      Exit;

    If Not ChunkIs('WAVE') Then
      Exit;

    // -----------------------------------------------------------------------
    // Walk RIFF chunks
    // -----------------------------------------------------------------------
    FoundFmt := False;
    FoundData := False;

    While Stream.Position + 8 <= Stream.Size Do
    Begin
      // Chunk ID
      If Stream.Read(ChunkID, SizeOf(ChunkID)) <> SizeOf(ChunkID) Then
        Exit;

      // Chunk size
      If Stream.Read(ChunkSize, SizeOf(ChunkSize)) <> SizeOf(ChunkSize) Then
        Exit;

      // ---------------------------------------------------------------------
      // fmt chunk
      // ---------------------------------------------------------------------
      If ChunkIs('fmt ') Then
      Begin
        If ChunkSize < 16 Then
          Exit;

        Stream.ReadBuffer(AWavInfo.AudioFormat, SizeOf(AWavInfo.AudioFormat));
        Stream.ReadBuffer(AWavInfo.Channels, SizeOf(AWavInfo.Channels));
        Stream.ReadBuffer(AWavInfo.SampleRate, SizeOf(AWavInfo.SampleRate));
        Stream.ReadBuffer(AWavInfo.ByteRate, SizeOf(AWavInfo.ByteRate));
        Stream.ReadBuffer(AWavInfo.BlockAlign, SizeOf(AWavInfo.BlockAlign));
        Stream.ReadBuffer(AWavInfo.BitsPerSample, SizeOf(AWavInfo.BitsPerSample));

        // ---------------------------------------------------------------
        // WAVE_FORMAT_EXTENSIBLE
        // ---------------------------------------------------------------
        If (AWavInfo.AudioFormat = WAVE_FORMAT_EXTENSIBLE) And (ChunkSize >= 40) Then
        Begin
          Stream.ReadBuffer(AWavInfo.ExtensionSize, SizeOf(AWavInfo.ExtensionSize));
          Stream.ReadBuffer(AWavInfo.ValidBitsPerSample, SizeOf(AWavInfo.ValidBitsPerSample));
          Stream.ReadBuffer(AWavInfo.ChannelMask, SizeOf(AWavInfo.ChannelMask));
          Stream.ReadBuffer(AWavInfo.SubFormat, SizeOf(AWavInfo.SubFormat));

          // Standard extensible fmt structure = 40 bytes.
          If ChunkSize > 40 Then
            Stream.Seek(ChunkSize - 40, soCurrent);
        End
        Else
        Begin
          // Ordinary WAV fmt chunk.
          If ChunkSize > 16 Then
            Stream.Seek(ChunkSize - 16, soCurrent);
        End;

        FoundFmt := True;
      End

      // ---------------------------------------------------------------------
      // data chunk
      // ---------------------------------------------------------------------
      Else If ChunkIs('data') Then
      Begin
        AWavInfo.DataOffset := Stream.Position;
        AWavInfo.DataSize := ChunkSize;

        FoundData := True;

        // We only need to know where it is.
        // Don't read the potentially enormous PCM data here.
        Stream.Seek(ChunkSize, soCurrent);
      End

      // ---------------------------------------------------------------------
      // Unknown RIFF chunk
      // ---------------------------------------------------------------------
      Else
      Begin
        Stream.Seek(ChunkSize, soCurrent);
      End;
      // RIFF chunks are padded to an even byte boundary.
      If Odd(ChunkSize) Then
        Stream.Seek(1, soCurrent);

      If FoundFmt And FoundData Then
        Break;
    End;

    Result := FoundFmt And FoundData;
  Finally
    Stream.Free;
  End;
End;

{ ---------------------------------------------------------------------------
  Processes all mono samples from the Buffer and normalise to Singles.

  Result is nominally:
      -1.0 = maximum negative amplitude
       0.0 = silence
      +1.0 = maximum positive amplitude

  Currently supports the formats we've actually encountered from libmpv:
      ProcessPCM16():   16-bit signed integer PCM
      ProcessFloat32(): 32-bit IEEE floating point
  --------------------------------------------------------------------------- }

// -----------------------------------------------------------------------
// Signed integer PCM
// -----------------------------------------------------------------------
Procedure ProcessPCM16(Const ABuffer; AByteCount: Integer; Var ABuckets: TWaveformBuckets;
  Var ABucketIndex: Integer; Var ASamplesInBucket: Integer; ASamplesPerBucket: Integer;
  Var AMinValue: Single; Var AMaxValue: Single);
Var
  Samples: ^Smallint;
  SampleCount: Integer;
  I: Integer;
  Value: Single;
Begin
  Samples := @ABuffer;
  SampleCount := AByteCount Div SizeOf(Smallint);

  For I := 0 To SampleCount - 1 Do
  Begin
    Value := Samples[I] / 32768.0;

    If Value < AMinValue Then
      AMinValue := Value;

    If Value > AMaxValue Then
      AMaxValue := Value;

    Inc(ASamplesInBucket);

    If ASamplesInBucket = ASamplesPerBucket Then
    Begin
      ABuckets[ABucketIndex].MinValue := AMinValue;
      ABuckets[ABucketIndex].MaxValue := AMaxValue;

      Inc(ABucketIndex);

      ASamplesInBucket := 0;
      AMinValue := 0;
      AMaxValue := 0;
    End;
  End;
End;

// -----------------------------------------------------------------------
// IEEE floating point
// -----------------------------------------------------------------------
Procedure ProcessFloat32(Const ABuffer; AByteCount: Integer; Var ABuckets: TWaveformBuckets;
  Var ABucketIndex: Integer; Var ASamplesInBucket: Integer; ASamplesPerBucket: Integer;
  Var AMinValue: Single; Var AMaxValue: Single);
Var
  Samples: ^Single;
  SampleCount: Integer;
  I: Integer;
  Value: Single;
Begin
  Samples := @ABuffer;
  SampleCount := AByteCount Div SizeOf(Single);

  For I := 0 To SampleCount - 1 Do
  Begin
    Value := Samples[I];

    If Value < AMinValue Then
      AMinValue := Value;

    If Value > AMaxValue Then
      AMaxValue := Value;

    Inc(ASamplesInBucket);

    If ASamplesInBucket = ASamplesPerBucket Then
    Begin
      ABuckets[ABucketIndex].MinValue := AMinValue;
      ABuckets[ABucketIndex].MaxValue := AMaxValue;

      Inc(ABucketIndex);

      ASamplesInBucket := 0;
      AMinValue := 0;
      AMaxValue := 0;
    End;
  End;
End;

{ ---------------------------------------------------------------------------
  ReadWaveform

  Reads a WAV file and reduces it to Min/Max amplitude buckets.

  ABucketMilliseconds controls the waveform resolution.

      20 ms = 50 buckets/sec
      10 ms = 100 buckets/sec
      50 ms = 20 buckets/sec

  The default of 20 ms gives approximately 45,000 buckets for a
  15-minute video.
  --------------------------------------------------------------------------- }

Function ReadWaveform(Const AFilename: String; Out AWavInfo: TWavInfo;
  Out ABuckets: TWaveformBuckets; ABucketMilliseconds: Integer): Boolean;
Var
  Stream: TFileStream;

  MinValue: Single;
  MaxValue: Single;

  SamplesPerBucket: Integer;
  SampleCount: Int64;

  BucketCount: Integer;

  Buffer: Array Of Byte;
  BytesRemaining: Int64;
  BytesToRead: Integer;
  BytesRead: Integer;
  BucketIndex: Integer;
  SamplesInBucket: Integer;
  FormatID: Word;
Begin
  Result := False;

  ABuckets := nil;
  SetLength(ABuckets, 0);

  // -------------------------------------------------------------------------
  // Read WAV metadata
  // -------------------------------------------------------------------------
  If Not ReadWavHeader(AFilename, AWavInfo) Then
    Exit;

  // -------------------------------------------------------------------------
  // For the moment our libmpv extraction requests mono audio.

  // Supporting stereo later isn't difficult, but silently interpreting
  // interleaved stereo samples as mono would be wrong.
  // -------------------------------------------------------------------------
  If AWavInfo.Channels <> 1 Then
    Raise Exception.CreateFmt('Waveform reader currently requires mono WAV data. Channels: %d',
      [AWavInfo.Channels]);

  If AWavInfo.BlockAlign = 0 Then
    Raise Exception.Create('Invalid WAV block alignment');

  If ABucketMilliseconds <= 0 Then
    Raise Exception.Create('Waveform bucket duration must be greater than zero');

  // -------------------------------------------------------------------------
  // Calculate number of audio frames.

  // Because this is mono:

  //     one frame = one sample

  // Using BlockAlign is important because our files may contain either:

  //     16-bit PCM   -> 2 bytes/frame
  //     32-bit float -> 4 bytes/frame
  // -------------------------------------------------------------------------
  SampleCount := AWavInfo.DataSize Div AWavInfo.BlockAlign;

  // -------------------------------------------------------------------------
  // Samples per waveform bucket.

  // At 44.1 kHz and 20 ms:

  //     44100 * 20 / 1000 = 882 samples
  // -------------------------------------------------------------------------
  SamplesPerBucket := Round(AWavInfo.SampleRate * ABucketMilliseconds / 1000.0);

  If SamplesPerBucket < 1 Then
    SamplesPerBucket := 1;

  // Ceiling division.

  BucketCount := (SampleCount + SamplesPerBucket - 1) Div SamplesPerBucket;

  ABuckets := nil;
  SetLength(ABuckets, BucketCount);

  If AWavInfo.AudioFormat = WAVE_FORMAT_EXTENSIBLE Then
    FormatID := AWavInfo.SubFormat.D1
  Else
    FormatID := AWavInfo.AudioFormat;

  Buffer := nil;
  SetLength(Buffer, READ_BUFFER_SIZE);

  // -------------------------------------------------------------------------
  // Read and reduce the PCM data
  // -------------------------------------------------------------------------
  Stream := TFileStream.Create(AFilename, fmOpenRead Or fmShareDenyNone);
  Try
    Stream.Position := AWavInfo.DataOffset;

    BytesRemaining := AWavInfo.DataSize;
    BucketIndex := 0;
    SamplesInBucket := 0;
    MinValue := 0;
    MaxValue := 0;

    While BytesRemaining > 0 Do
    Begin
      If BytesRemaining > Length(Buffer) Then
        BytesToRead := Length(Buffer)
      Else
        BytesToRead := BytesRemaining;

      BytesRead := Stream.Read(Buffer[0], BytesToRead);

      If BytesRead <= 0 Then
        Raise Exception.Create('Unexpected end of WAV data');

      Case FormatID Of

        WAVE_FORMAT_PCM:
        Begin
          If AWavInfo.BitsPerSample <> 16 Then
            Raise Exception.CreateFmt('Unsupported PCM sample size: %d bit',
              [AWavInfo.BitsPerSample]);

          ProcessPCM16(Buffer[0], BytesRead, ABuckets, BucketIndex,
            SamplesInBucket, SamplesPerBucket, MinValue, MaxValue);
        End;

        WAVE_FORMAT_IEEE_FLOAT:
        Begin
          If AWavInfo.BitsPerSample <> 32 Then
            Raise Exception.CreateFmt('Unsupported floating point sample size: %d bit',
              [AWavInfo.BitsPerSample]);

          ProcessFloat32(Buffer[0], BytesRead, ABuckets, BucketIndex,
            SamplesInBucket, SamplesPerBucket, MinValue, MaxValue);
        End;

        Else
          Raise Exception.CreateFmt('Unsupported WAV format: %d', [FormatID]);
      End;

      Dec(BytesRemaining, BytesRead);
    End;

    // Store the final partial bucket.
    If SamplesInBucket > 0 Then
    Begin
      ABuckets[BucketIndex].MinValue := MinValue;
      ABuckets[BucketIndex].MaxValue := MaxValue;
    End;

    Result := True;
  Finally
    Stream.Free;
  End;
End;

{ TAudioWaveformRenderer }

Constructor TAudioWaveformRenderer.Create;
Begin
  FNormalise := True;
  FPeakModifier := 1;
  FColor := clBlack;
End;

Procedure TAudioWaveformRenderer.SetWaveBuckets(Const AValue: TWaveformBuckets);
Var
  I: Integer;
Begin
  FWaveBuckets := Copy(AValue);

  FPeakMax := 0;
  FPeakMin := 0;

  For I := 0 To High(FWaveBuckets) Do
  Begin
    If FWaveBuckets[I].MaxValue > FPeakMax Then
      FPeakMax := FWaveBuckets[I].MaxValue;

    If FWaveBuckets[I].MinValue < FPeakMin Then
      FPeakMin := FWaveBuckets[I].MinValue;
  End;
End;

Procedure TAudioWaveformRenderer.Paint(ACanvas: TCanvas; Const ARect: TRect);
Var
  iX: Integer;
  iBucket, iBucketStart, iBucketEnd: Integer;
  iHalfHeight: Integer;
  dMin, dMax: Single;
  iYMin, iYMax: Integer;
Begin
  If Length(FWaveBuckets) = 0 Then
    Exit;

  If ARect.Width <= 0 Then
    Exit;

  iHalfHeight := ARect.Height Div 2;

  ACanvas.Pen.Color := FColor;

  For iX := 0 To ARect.Width - 1 Do
  Begin
    // Which waveform buckets are represented by this pixel?
    iBucketStart := (Int64(iX) * Length(FWaveBuckets)) Div ARect.Width;
    iBucketEnd := (Int64(iX + 1) * Length(FWaveBuckets)) Div ARect.Width - 1;

    // Make sure every pixel gets at least one bucket.
    If iBucketEnd < iBucketStart Then
      iBucketEnd := iBucketStart;

    // Find the extremes represented by this pixel.
    dMin := 0;
    dMax := 0;

    For iBucket := iBucketStart To iBucketEnd Do
    Begin
      If FWaveBuckets[iBucket].MinValue < dMin Then
        dMin := FWaveBuckets[iBucket].MinValue;

      If FWaveBuckets[iBucket].MaxValue > dMax Then
        dMax := FWaveBuckets[iBucket].MaxValue;
    End;

    If (FPeakMax <> 0) And (FNormalise) Then
      dMax := Min(1.0, dMax / (FPeakModifier * FPeakMax));

    If (FPeakMin <> 0) And (FNormalise) Then
      dMin := Max(-1.0, dMin / Abs(FPeakModifier * FPeakMin));

    iYMin := iHalfHeight - Trunc(iHalfHeight * Abs(dMin));
    iYMax := iHalfHeight + Trunc(iHalfHeight * Abs(dMax));

    ACanvas.Line(ARect.Left + iX, iYMin, ARect.Left + iX, iYMax);
  End;
End;

End.
