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
  DBG_VIDEO: PLazLoggerLogGroup;
  DBG_VIDEO_PLAYER: PLazLoggerLogGroup;
  DBG_APPLICATION: PLazLoggerLogGroup;

Procedure InitialiseLogging(ATimestamp: TLogTimestamp = ltTime);

Implementation

Uses
  SysUtils, Forms, VersionSupport;

Var
  FTimestamp: TLogTimestamp;
  FLoggingSupport: TLoggingSupport;
  FInitialised: Boolean = False;

Procedure InitialiseLogging(ATimestamp: TLogTimestamp = ltTime);
Var
  sFilename, sDir, sFileBase: String;
Begin
  FTimestamp := ATimestamp;

  If Not FInitialised Then
  Begin
    DebugLogger.CloseLogFileBetweenWrites := True;
    sDir := IncludeTrailingPathDelimiter(GetAppConfigDir(False));
    sFileBase := ChangeFileExt(ExtractFilename(Application.ExeName), '');
    sFilename := sDir + sFileBase + ' - ' + FormatDateTime('yyyymmdd hhnnss', now) + '.log';

    DebugLogger.CurrentIndentLevel;
    DebugLogger.LogName := sFilename;
    DebugLogger.OnDebugLnEx := @FLoggingSupport.DebugLnEx;

    DBG_VIDEO := DebugLogger.FindOrRegisterLogGroup('VIDEO', True);
    DBG_VIDEO_PLAYER := DebugLogger.FindOrRegisterLogGroup('VIDEO_PLAYER', True);
    DBG_APPLICATION := DebugLogger.FindOrRegisterLogGroup('APPLICATION', True);

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
End;

{ TLoggingSupport }

Procedure TLoggingSupport.DebugLnEx(Sender: TObject; Var LogTxt, LogIndent: String;
  Var Handled: Boolean; Const AnInfo: TLazLoggerWriteExEventInfo);
Var
  sTimestamp: String = '';
Begin
  Case FTimestamp Of
    ltTime: sTimestamp := FormatDateTime('hh:nn:ss.zzz', now) + ': ';
    ltDateTime: sTimestamp := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', now) + ': ';
  End;

  LogIndent := sTimestamp + LogIndent;
End;

Initialization
  FLoggingSupport := TLoggingSupport.Create;

Finalization;
  FreeAndNil(FLoggingSupport);

End.
