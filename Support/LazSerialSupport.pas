Unit LazSerialSupport;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, LazSerial, Inifiles;

Type

  { TLazSerialHelper }

  TLazSerialHelper = Class helper For TlazSerial
  private
    function GetSettingsAsCommaSep: String;
    function GetSettingsAsXML: String;
    procedure SetSettingsAsCommaSep(AValue: String);
    procedure SetSettingsAsXML(AValue: String);
    function Settings: TStringArray;
  public
    Procedure ReadInifile(AInifile: TInifile; ASection: String);
    Procedure WriteInifile(AInifile: TInifile; ASection: String);

    Function WriteLn(AData: String): Integer;

    Function ToString: String;
    Property SettingsAsCommaSep: String Read GetSettingsAsCommaSep Write SetSettingsAsCommaSep;
    Property SettingsAsXML: String Read GetSettingsAsXML Write SetSettingsAsXML;
  End;

Implementation

Uses
  lazserialsetup, StringSupport;

Function TLazSerialHelper.Settings : TStringArray;
Begin
  Result := nil;
  SetLength(Result, 6);

  Result[0] := Format('Serial Port=%s', [Device]);
  Result[1] := Format('Baud Rate=%s', [BaudRateToStr(BaudRate)]);
  Result[2] := Format('Data Bits=%s', [DataBitsToStr(DataBits)]);
  Result[3] := Format('Parity=%s', [ParityToStr(Parity)]);
  Result[4] := Format('Stop Bits=%s', [StopBitsToStr(StopBits)]);
  Result[5] := Format('Flowcontrol=%s', [FlowControlToStr(FlowControl)]);
end;

function TLazSerialHelper.ToString: String;
Var
  arrSettings: TStringArray;
Begin
  arrSettings := Settings;

  Result := ArrayToString(arrSettings, #13#10);
  Result := FindReplace(Result, '=', ': ');
End;

function TLazSerialHelper.GetSettingsAsCommaSep: String;
Var
  arrSettings: TStringArray;
Begin
  arrSettings := Settings;

  Result := ArrayToString(arrSettings, ',');
End;

procedure TLazSerialHelper.SetSettingsAsCommaSep(AValue: String);
Var
  oTemp: TStringList;
Begin
  oTemp := TStringList.Create;
  Try
    oTemp.NameValueSeparator := '=';
    oTemp.Delimiter := ',';
    oTemp.DelimitedText := AValue;

    Device := oTemp.Values['Serial Port'];
    BaudRate := StrToBaudRate(oTemp.Values['Baud Rate']);
    DataBits := StrToDataBits(oTemp.Values['Data Bits']);
    Parity := StrToParity(oTemp.Values['Parity']);
    StopBits := StrToStopBits(oTemp.Values['Stop Bits']);
    FlowControl := StrToFlowControl(oTemp.Values['Flowcontrol']);
  Finally
    oTemp.Free;
  End;
End;

function TLazSerialHelper.GetSettingsAsXML: String;
Var
  s, sItem: String;
  arrSettings: TStringArray;
begin
  arrSettings := Settings;

  s := '<TLazSerial>';
  For sItem in arrSettings Do
    s := s + Format('<setting name="%s" value="%s"/>', [ExtractField(sItem, '=', 0), ExtractField(sItem, '=', 1)]);
  s := s + '</TLazSerial>';

  Result := s;
end;

procedure TLazSerialHelper.SetSettingsAsXML(AValue: String);
  Function GetValue(AName: String): String;
  var
    sTemp: String;
  Begin
    sTemp := Trim(TextBetween(AValue, '<setting name="'+AName+'"', '/>'));
    Result := TextBetween(sTemp, '"', '"');
  end;
begin
  Device := GetValue('Serial Port');
  BaudRate := StrToBaudRate(GetValue('Baud Rate'));
  DataBits := StrToDataBits(GetValue('Data Bits'));
  Parity := StrToParity(GetValue('Parity'));
  StopBits := StrToStopBits(GetValue('Stop Bits'));
  FlowControl := StrToFlowControl(GetValue('Flowcontrol'));
end;

procedure TLazSerialHelper.ReadInifile(AInifile: TInifile; ASection: String);
Begin
  Device := AInifile.ReadString(ASection, 'Serial Port', 'COM1');
  BaudRate := StrToBaudRate(AInifile.ReadString(ASection, 'Baud Rate', '9600'));
  DataBits := StrToDataBits(AInifile.ReadString(ASection, 'Data Bits', '8'));
  Parity := StrToParity(AInifile.ReadString(ASection, 'Parity', 'None'));
  StopBits := StrToStopBits(AInifile.ReadString(ASection, 'Stop Bits', '1'));
  FlowControl := StrToFlowControl(AInifile.ReadString(ASection, 'Flowcontrol', 'None'));
End;

procedure TLazSerialHelper.WriteInifile(AInifile: TInifile; ASection: String);
Begin
  AInifile.WriteString(ASection, 'Serial Port', Device);
  AInifile.WriteString(ASection, 'Baud Rate', BaudRateToStr(BaudRate));
  AInifile.WriteString(ASection, 'Data Bits', DataBitsToStr(DataBits));
  AInifile.WriteString(ASection, 'Parity', ParityToStr(Parity));
  AInifile.WriteString(ASection, 'Stop Bits', StopBitsToStr(StopBits));
  AInifile.WriteString(ASection, 'Flowcontrol', FlowControlToStr(FlowControl));
End;

function TLazSerialHelper.WriteLn(AData: String): Integer;
Begin
  Result := WriteData(AData + CRLF);
End;

End.
