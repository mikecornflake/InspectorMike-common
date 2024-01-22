Unit FrameVideoBase;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, FrameBase;

Type
  TTimerEvent = Procedure(Sender: TObject; CurrentPos, Length: Cardinal) Of Object;

  { TfmeVideoBase }

  TfmeVideoBase = Class(TfmeBase)
  Protected
    FFilename: String;
    FOnTimer: TTimerEvent;

    Function GetPosition: Integer; Virtual;
    Function GetRate: Double; Virtual;
    Procedure SetFilename(AValue: String); Virtual;
    Procedure SetPosition(AValue: Integer); Virtual;
    Procedure SetRate(AValue: Double); Virtual;
  Public
    Constructor Create(TheOwner: TComponent); Override;

    Function Play: Boolean; Virtual;
    Function Pause: Boolean; Virtual;
    Function Resume: Boolean; Virtual;
    Function Stop: Boolean; Virtual;

    Function Paused: Boolean; Virtual;

    Function Duration: TDateTime; Virtual;

    Function CanRewind: Boolean; Virtual;
    Function GetBitmap(Bitmap: TBitmap): Boolean; Virtual;

    Property Filename: String read FFilename write SetFilename;
    Property OnTimer: TTimerEvent read FOnTimer write FOnTimer;

    Property Rate: Double read GetRate write SetRate;
    Property Position: Integer read GetPosition write SetPosition;
  End;

Implementation

{$R *.lfm}

{ TfmeVideoBase }

Constructor TfmeVideoBase.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);

  FFilename := '';
  FOnTimer := nil;
End;

Function TfmeVideoBase.GetPosition: Integer;
Begin
  Result := 0;
End;

Function TfmeVideoBase.GetRate: Double;
Begin
  Result := 0.0;
End;

Procedure TfmeVideoBase.SetFilename(AValue: String);
Begin
  If FFilename = AValue Then
    Exit;
  FFilename := AValue;
End;

Procedure TfmeVideoBase.SetPosition(AValue: Integer);
Begin

End;

Procedure TfmeVideoBase.SetRate(AValue: Double);
Begin

End;

Function TfmeVideoBase.Play: Boolean;
Begin
  Result := False;
End;

Function TfmeVideoBase.Pause: Boolean;
Begin
  Result := False;
End;

Function TfmeVideoBase.Resume: Boolean;
Begin
  Result := False;
End;

Function TfmeVideoBase.Stop: Boolean;
Begin
  Result := False;
End;

Function TfmeVideoBase.Paused: Boolean;
Begin
  Result := False;
End;

Function TfmeVideoBase.Duration: TDateTime;
Begin
  Result := -1;
End;

Function TfmeVideoBase.CanRewind: Boolean;
Begin
  Result := False;
End;

Function TfmeVideoBase.GetBitmap(Bitmap: TBitmap): Boolean;
Begin
  Result := False;
End;

End.
