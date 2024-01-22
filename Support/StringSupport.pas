Unit StringSupport;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, Graphics, SysUtils;

Type
  TCaseOperation = (coLowercase, coUppercase, coPropercase);

// string routines
Function FindReplace(Const ASource, sFind, sReplace: String): String;
Function FindNextString(Const ASource, sSubstr: String; iStart: Integer): Integer;
Function YYYYmmddHHmmssToDateTimeDef(ASource: String; dtDefault: TDateTime): TDateTime;
Function YYYYmmddHHmmssToDateTime(ASource: String): TDateTime;
Function TextBetween(ASource, sStart, sEnd: String): String;
Function Count(Const sSubstr, ASource: String): Integer;
Function ExtractField(Const ASource: String; cSeparator: Char; iIndex: Integer): String;
Function ChangeCase(Const ASource: String; ACaseOperation: TCaseOperation): String;

Function DateToFilename(ADate: TDateTime): String;
Function TimeToFilename(ATime: TDateTime): String;
Function DateTimeToFilename(ADateTime: TDateTime): String;

Function IsIn(sSearch: String; Const AValues: Array Of Const): Boolean;

// From fpVectorial
Procedure AddStringToArray(Var AStringArray: TStringArray; Const AString: String);
Procedure AddStringsToArray(Var AStringArray: TStringArray; Const AString: TStringArray);
Function ArrayToString(Const AStrings: TStringArray; ADelimiter: String = ''): String;

Type
  CharSet = Set Of Char;

Function TrimChars(AInput: String; AChars: CharSet): String;
Function ExcludeSemicolon(AInput: String): String;
Function IncludeSemicolon(AInput: String): String;

Const
  Quote = '''';
  BOOLEAN_YES_NO: Array[Boolean] Of String = ('No', 'Yes');
  BOOLEAN_TRUE_FALSE: Array[Boolean] Of String = ('False', 'True');
  CR = #13;
  CRLF = #13#10;
  TAB = #9;

Var
  GFilenameDateTimeFormat: TFormatSettings;
  GDisplayDateTimeFormat: TFormatSettings;

Implementation

Uses
  StrUtils;

Function ChangeCase(Const ASource: String; ACaseOperation: TCaseOperation): String;
Var
  sTemp: String;
Begin
  Result := '';
  Case ACaseOperation Of
    coLowercase: Result := Lowercase(ASource);
    coUppercase: Result := Uppercase(ASource);
    coPropercase:
    Begin
      sTemp := Copy(ASource, 1, Length(ASource));
      Result := AnsiProperCase(sTemp, StdWordDelims); // TODO Proper case
    End;
  End;
End;

Function FindNextString(Const ASource, sSubstr: String; iStart: Integer): Integer;
Begin
  If iStart > Length(ASource) Then
    Result := 0
  Else
  Begin
    Result := Pos(sSubstr, PChar(@ASource[iStart]));

    If Result > 0 Then
      Inc(Result, iStart - 1);
  End;
End;

Function Count(Const sSubstr, ASource: String): Integer;
Var
  iPos: Integer;
Begin
  Result := 0;

  iPos := FindNextString(ASource, sSubstr, 1);
  While (iPos > 0) Do
  Begin
    Inc(iPos, Length(sSubstr));

    iPos := FindNextString(ASource, sSubstr, iPos);

    Inc(Result);
  End;
End;

Function FindReplace(Const ASource, sFind, sReplace: String): String;
Begin
  Result := StringReplace(ASource, sFind, sReplace, [rfReplaceAll, rfIgnoreCase]);
End;

Function TextBetween(ASource, sStart, sEnd: String): String;
Var
  iStart, iEnd, iLen: Integer;
Begin
  iLen := Length(ASource);

  If sStart = '' Then
    iStart := 1
  Else
    iStart := Pos(sStart, ASource) + Length(sStart);

  If sEnd = '' Then
    iEnd := iLen + 1
  Else
    iEnd := iStart + Pos(sEnd, Copy(ASource, iStart, iLen)) - 1;

  Result := Copy(ASource, iStart, (iEnd - iStart));
End;

Function ExtractField(Const ASource: String; cSeparator: Char; iIndex: Integer): String;
Begin
  Result := ExtractWord(iIndex + 1, ASource, [cSeparator]);
End;

Function DateToFilename(ADate: TDateTime): String;
Begin
  Result := DateToStr(ADate, GFilenameDateTimeFormat);
End;

Function TimeToFilename(ATime: TDateTime): String;
Begin
  Result := TimeToStr(ATime, GFilenameDateTimeFormat);
End;

Function DateTimeToFilename(ADateTime: TDateTime): String;
Begin
  Result := DateTimeToStr(ADateTime, GFilenameDateTimeFormat);
End;

Function IsIn(sSearch: String; Const AValues: Array Of Const): Boolean;
Var
  i: Integer;
Begin
  Result := False;

  For i := Low(AValues) To High(AValues) Do
  Begin
    If (AValues[i].vType = vtAnsiString) Then
      Result := Ansistring(AValues[i].vAnsiString) = sSearch;

    If Result Then
      Break;
  End;
End;

// nicked from sysstr.inc
Function TrimChars(AInput: String; AChars: CharSet): String;
Var
  iOffset, iLen: Integer;
Begin
  ilen := Length(AInput);
  While (iLen > 0) And (AInput[iLen] In AChars) Do
    Dec(iLen);
  iOffset := 1;
  While (iOffset <= iLen) And (AInput[iOffset] In AChars) Do
    Inc(iOffset);
  Result := Copy(AInput, iOffset, 1 + iLen - iOffset);
End;

Function ExcludeSemicolon(AInput: String): String;
Begin
  Result := TrimChars(AInput, [' ', ';']);
End;

Function IncludeSemicolon(AInput: String): String;
Begin
  Result := ExcludeSemicolon(AInput) + '; ';
End;

Function YYYYmmddHHmmssToDateTimeDef(ASource: String; dtDefault: TDateTime): TDateTime;
Begin
  Try
    Result := YYYYmmddHHmmssToDateTime(ASource);
  Except
    Result := dtDefault;
  End;
End;

Function YYYYmmddHHmmssToDateTime(ASource: String): TDateTime;
Var
  sDate, sTime: String;
  dtTemp: TDateTime;
  iY, iM, iD: Integer;
  arrDate, arrTemp: TStringArray;
Begin
  Result := 0;

  If Trim(ASource) = '' Then
    Exit;

  ASource := Trim(ASource);
  If (Pos('/', ASource) > 0) And (Pos(':', ASource) > 0) And (Pos(' ', ASource) > 0) Then
  Begin
    arrTemp := ASource.Split(' ');
    If Length(arrTemp) <> 2 Then
      Raise Exception.Create('Input not in expected format "<date> <time>": ' + ASource);

    sDate := arrTemp[0];
    sTime := arrTemp[1];

    arrDate := sDate.Split('/');
    If Length(arrDate) <> 3 Then
      Raise Exception.Create('Date portion not in expected format YYYY/mm/dd: ' + ASource);

    iY := StrToIntDef(arrDate[0], 0);
    iM := StrToIntDef(arrDate[1], 0);
    iD := StrToIntDef(arrDate[2], 0);

    If TryEncodeDate(iY, iM, iD, dtTemp) Then
      Result := dtTemp + StrToTime(sTime)
    Else
      Raise Exception.Create(Format('Unable to encode date Y=%d, M=%d, D=%d',
        [iY, iM, iD]));
  End
  Else
    Raise Exception.Create(ASource + ' is invalid date time format');
End;

Procedure AddStringToArray(Var AStringArray: TStringArray; Const AString: String);
Begin
  SetLength(AStringArray, Length(AStringArray) + 1);
  AStringArray[High(AStringArray)] := AString;
End;

Procedure AddStringsToArray(Var AStringArray: TStringArray; Const AString: TStringArray);
Var
  n, i: Integer;
Begin
  n := Length(AStringArray);
  SetLength(AStringArray, n + Length(AString));
  For i := 0 To High(AString) Do
    AStringArray[i + n] := AString[i];
End;

Function ArrayToString(Const AStrings: TStringArray; ADelimiter: String): String;
Var
  sItem: String;
Begin
  Result := '';
  For sItem In AStrings Do
    Result := Result + sItem + ADelimiter;
End;

Initialization
  GFilenameDateTimeFormat := DefaultFormatSettings;
  GFilenameDateTimeFormat.DateSeparator := ' ';
  GFilenameDateTimeFormat.ShortDateFormat := 'yyyy mm dd';
  GFilenameDateTimeFormat.TimeSeparator := #0;
  GFilenameDateTimeFormat.LongTimeFormat := 'HHmmss';

  GDisplayDateTimeFormat := DefaultFormatSettings;
  GDisplayDateTimeFormat.DateSeparator := '/';
  GDisplayDateTimeFormat.ShortDateFormat := 'DD/MM/YYYYY';
  GDisplayDateTimeFormat.TimeSeparator := ':';
  GDisplayDateTimeFormat.LongTimeFormat := 'HH:mm:ss';
End.
