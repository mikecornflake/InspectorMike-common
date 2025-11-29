Unit StringSupport;

{$mode ObjFPC}{$H+}
{$codepage utf8}

Interface

Uses
  Classes, Graphics, SysUtils, TypInfo;

Type
  TCaseOperation = (coLowercase, coUppercase, coPropercase);

// string routines
Function FindReplace(Const ASource, sFind, sReplace: String): String;
Function FindNextString(Const ASource, sSubstr: String; iStart: Integer): Integer;
Function YYYYmmddHHmmssToDateTimeDef(ASource: String; dtDefault: TDateTime): TDateTime;
Function YYYYmmddHHmmssToDateTime(ASource: String): TDateTime;
Function TextBetween(Const ASource, sStart, sEnd: String): String;
Function Count(Const sSubstr, ASource: String): Integer;
Function ExtractField(Const ASource: String; cSeparator: Char; iIndex: Integer): String;
Function ChangeCase(Const ASource: String; ACaseOperation: TCaseOperation): String;

// Date Time helpers
Function FormatDateTimeAsISO8601(dt: TDateTime): String;

// TStringArray Routines (From fpVectorial)
Procedure AddStringToArray(Var AStringArray: TStringArray; Const AString: String);
Procedure AddStringsToArray(Var AStringArray: TStringArray; Const AString: TStringArray);
Function ArrayToString(Const AStrings: TStringArray; ADelimiter: String = ''): String;

// Array of String Helpers
Function InArray(AValue: String; AArray: Array Of String): Boolean;

// Array of Const (parameters) helpers
Function IsIn(sSearch: String; Const AValues: Array Of Const): Boolean;

// TypInfo Helpers
Function FieldTypeToString(AType: TTypeKind): String;
Function StringToFieldType(Const S: String): TTypeKind;

// HTML
Function ValidateHTML(AInput: String): String;

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

Function FormatDateTimeAsISO8601(dt: TDateTime): String;
Var
  tzOffset: Integer;
  tzHours, tzMinutes: Integer;
  tzSign: String;
  tzStr: String;
Begin
  tzOffset := GetLocalTimeOffset; // in minutes
  tzHours := Abs(tzOffset) Div 60;
  tzMinutes := Abs(tzOffset) Mod 60;
  tzSign := IfThen(tzOffset >= 0, '+', '-');
  tzStr := Format('%s%.2d:%.2d', [tzSign, tzHours, tzMinutes]);

  Result := FormatDateTime('yyyy-mm-dd', dt) + 'T';
  Result += FormatDateTime('HH:nn:ss', dt) + tzStr
End;

Function FieldTypeToString(AType: TTypeKind): String;
Begin
  Result := GetEnumName(TypeInfo(TTypeKind), Ord(AType));
End;

Function StringToFieldType(Const S: String): TTypeKind;
Var
  v: Integer;
Begin
  v := GetEnumValue(TypeInfo(TTypeKind), S);
  If v < 0 Then
    Raise Exception.CreateFmt('Invalid field type name: %s', [S]);
  Result := TTypeKind(v);
End;

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

Function TextBetween(Const ASource, sStart, sEnd: String): String;
Var
  iStart, iEnd, iLen: Integer;
Begin
  If ASource = '' Then
    Exit('');

  iLen := Length(ASource);

  If sStart = '' Then
    iStart := 1
  Else
  Begin
    iStart := Pos(sStart, ASource);

    If iStart = 0 Then
      Exit('');

    Inc(iStart, Length(sStart));
  End;

  If sEnd = '' Then
    iEnd := iLen + 1
  Else
  Begin
    iEnd := PosEx(sEnd, ASource, iStart);

    If iEnd = 0 Then
      iEnd := iLen + 1;
  End;

  If (iStart >= 1) And (iStart <= iLen) And (iEnd > iStart) Then
    Result := Copy(ASource, iStart, iEnd - iStart)
  Else
    Result := '';
End;

Function ExtractField(Const ASource: String; cSeparator: Char; iIndex: Integer): String;
Begin
  Result := ExtractWord(iIndex + 1, ASource, [cSeparator]);
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

Function InArray(AValue: String; AArray: Array Of String): Boolean;
Var
  sValue: String;
Begin
  Result := False;

  For sValue In AArray Do
    If CompareText(AValue, sValue) = 0 Then
      exit(True);
End;

// TODO Suspect this is a duplicate
Function ValidateHTML(AInput: String): String;
Begin
  Result := AInput;

  // Encode < and > characters
  Result := FindReplace(Result, '<', '&lt;');
  Result := FindReplace(Result, '>', '&gt;');

  // Convert line breaks to <br> tags
  Result := FindReplace(Result, #13#10, '<br> ');
  Result := FindReplace(Result, #13, '');
  Result := FindReplace(Result, #10, '<br> ');

  If Trim(Result) = '' Then
    Result := '&nbsp;';
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
