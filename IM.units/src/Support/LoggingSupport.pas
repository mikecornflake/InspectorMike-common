Unit LoggingSupport;

{$mode ObjFPC}{$H+}

// Need to put -dIM_DEBUG in Project Options - Custom Options
{$IFNDEF RELEASE}
{$DEFINE IM_DEBUG}
{$ENDIF}

Interface

Uses
  LazLogger;

Type
  TLogTimestamp = (ltNone, ltTime, ltDateTime);

  { TLoggingSupport }

  TLoggingSupport = Class
  Private
    Procedure DebugLnEx(Sender: TObject; Var LogTxt, LogIndent: String;
      Var Handled: Boolean; Const AnInfo: TLazLoggerWriteExEventInfo);
  End;

Var
  FLoggingSupport: TLoggingSupport;

Procedure InitialiseLogging(ATimestamp: TLogTimestamp = ltTime);

Implementation

Uses
  SysUtils, Forms, VersionSupport;

Var
  FTimestamp: TLogTimestamp;
  FInitialised: Boolean = False;

Procedure InitialiseLogging(ATimestamp: TLogTimestamp = ltTime);
Begin
  FTimestamp := ATimestamp;

  DebugLogger.LogName := ChangeFileExt(Application.ExeName, '.log');

  If Not FInitialised Then
  Begin
    DebugLn(['']);
    DebugLn(['============================================================']);
    DebugLn(['Application started']);
    DebugLn(['Executable : ', Application.ExeName]);
    DebugLn(['Version    : ', GetFileVersion]);
    DebugLn(['Build mode : ',
      {$IFDEF RELEASE}
      'Release'
      {$ELSE}
      'Debug/Default'
      {$ENDIF}
      ]);
    DebugLn(['OS         : ', GetOS]);
    DebugLn(['CPU        : ', GetCPU]);
    DebugLn(['FPC        : ', GetCompilerInfo]);
    DebugLn(['Lazarus    : ', GetLCLVersion]);
    DebugLn(['============================================================']);

    FInitialised := True;
  End;

  DebugLogger.OnDebugLnEx := @FLoggingSupport.DebugLnEx;
End;

{ TLoggingSupport }

Procedure TLoggingSupport.DebugLnEx(Sender: TObject; Var LogTxt, LogIndent: String;
  Var Handled: Boolean; Const AnInfo: TLazLoggerWriteExEventInfo);
Var
  sTimestamp: String = '';
Begin
  Case FTimestamp Of
    ltTime: sTimestamp := FormatDateTime('hh:nn:ss.zzz', Now) + ': ';
    ltDateTime: sTimestamp := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ': ';
  End;

  LogIndent := sTimestamp + LogIndent;
End;

Initialization
  FLoggingSupport := TLoggingSupport.Create;
  InitialiseLogging;

finalization;
  FreeAndNil(FLoggingSupport);

End.
