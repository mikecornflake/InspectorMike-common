Unit LazSerialSupport;

{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : LazSerialSupport.pas
  Description
    My first Class Helper

    Helper unit for LazSerial.pas

  Source
    Copyright (c) 2025
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    2022-10-09: Creation and upload to SourceForge/Package "Shared"
    2024-01-22: Migrated to Github.  Refactored package to "IM_units"
    2025-11-06: Added XML helper
    2025-11-29: Added this header

  License
    This file is part of IM_units.lpk.

    This library is free software: you can redistribute it and/or modify it
    under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or (at
    your option) any later version.

    This library is distributed in the hope that it will be useful, but
    WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
    General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this library. If not, see <https://www.gnu.org/licenses/>.

    SPDX-License-Identifier: LGPL-3.0-or-later
-------------------------------------------------------------------------------}

{$mode objfpc}{$H+}
{$codepage utf8}

Interface

Uses
  Classes, SysUtils, LazSerial, Inifiles;

Type

  { TLazSerialHelper }

  TLazSerialHelper = Class helper For TlazSerial
  Private
    Function GetSettingsAsCommaSep: String;
    Function GetSettingsAsXML: String;
    Procedure SetSettingsAsCommaSep(AValue: String);
    Procedure SetSettingsAsXML(AValue: String);
    Function Settings: TStringArray;
  Public
    Procedure ReadInifile(AInifile: TInifile; ASection: String);
    Procedure WriteInifile(AInifile: TInifile; ASection: String);

    Function WriteLn(AData: String): Integer;

    Function SimpleString: String;

    Function ToString: String;
    Property SettingsAsCommaSep: String Read GetSettingsAsCommaSep Write SetSettingsAsCommaSep;
    Property SettingsAsXML: String Read GetSettingsAsXML Write SetSettingsAsXML;
  End;

Implementation

Uses
  lazserialsetup, StringSupport;

Function TLazSerialHelper.Settings: TStringArray;
Begin
  Result := nil;
  SetLength(Result, 6);

  Result[0] := Format('Serial Port=%s', [Device]);
  Result[1] := Format('Baud Rate=%s', [BaudRateToStr(BaudRate)]);
  Result[2] := Format('Data Bits=%s', [DataBitsToStr(DataBits)]);
  Result[3] := Format('Parity=%s', [ParityToStr(Parity)]);
  Result[4] := Format('Stop Bits=%s', [StopBitsToStr(StopBits)]);
  Result[5] := Format('Flowcontrol=%s', [FlowControlToStr(FlowControl)]);
End;

Function TLazSerialHelper.ToString: String;
Var
  arrSettings: TStringArray;
Begin
  arrSettings := Settings;

  Result := ArrayToString(arrSettings, #13#10);
  Result := FindReplace(Result, '=', ': ');
End;

Function TLazSerialHelper.SimpleString: String;
Var
  s: String;
Begin
  s := '';
  s += Device + ',';
  s += BaudRateToStr(BaudRate) + ',';
  s += DataBitsToStr(DataBits) + ',';
  s += ParityToStr(Parity) + ',';
  s += StopBitsToStr(StopBits) + ',';
  s += FlowControlToStr(FlowControl);

  Result := s;
End;

Function TLazSerialHelper.GetSettingsAsCommaSep: String;
Var
  arrSettings: TStringArray;
Begin
  arrSettings := Settings;

  Result := ArrayToString(arrSettings, ',');
End;

Procedure TLazSerialHelper.SetSettingsAsCommaSep(AValue: String);
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

Function TLazSerialHelper.GetSettingsAsXML: String;
Var
  s, sItem: String;
  arrSettings: TStringArray;
Begin
  arrSettings := Settings;

  s := '<TLazSerial>';
  For sItem In arrSettings Do
    s := s + Format('<setting name="%s" value="%s"/>',
      [ExtractField(sItem, '=', 0), ExtractField(sItem, '=', 1)]);
  s := s + '</TLazSerial>';

  Result := s;
End;

Procedure TLazSerialHelper.SetSettingsAsXML(AValue: String);

  Function GetValue(AName: String): String;
  Var
    sTemp: String;
  Begin
    sTemp := Trim(TextBetween(AValue, '<setting name="' + AName + '"', '/>'));
    Result := TextBetween(sTemp, '"', '"');
  End;

Begin
  Device := GetValue('Serial Port');
  BaudRate := StrToBaudRate(GetValue('Baud Rate'));
  DataBits := StrToDataBits(GetValue('Data Bits'));
  Parity := StrToParity(GetValue('Parity'));
  StopBits := StrToStopBits(GetValue('Stop Bits'));
  FlowControl := StrToFlowControl(GetValue('Flowcontrol'));
End;

Procedure TLazSerialHelper.ReadInifile(AInifile: TInifile; ASection: String);
Begin
  Device := AInifile.ReadString(ASection, 'Serial Port', 'COM1');
  BaudRate := StrToBaudRate(AInifile.ReadString(ASection, 'Baud Rate', '9600'));
  DataBits := StrToDataBits(AInifile.ReadString(ASection, 'Data Bits', '8'));
  Parity := StrToParity(AInifile.ReadString(ASection, 'Parity', 'None'));
  StopBits := StrToStopBits(AInifile.ReadString(ASection, 'Stop Bits', '1'));
  FlowControl := StrToFlowControl(AInifile.ReadString(ASection, 'Flowcontrol', 'None'));
End;

Procedure TLazSerialHelper.WriteInifile(AInifile: TInifile; ASection: String);
Begin
  AInifile.WriteString(ASection, 'Serial Port', Device);
  AInifile.WriteString(ASection, 'Baud Rate', BaudRateToStr(BaudRate));
  AInifile.WriteString(ASection, 'Data Bits', DataBitsToStr(DataBits));
  AInifile.WriteString(ASection, 'Parity', ParityToStr(Parity));
  AInifile.WriteString(ASection, 'Stop Bits', StopBitsToStr(StopBits));
  AInifile.WriteString(ASection, 'Flowcontrol', FlowControlToStr(FlowControl));
End;

Function TLazSerialHelper.WriteLn(AData: String): Integer;
Begin
  Result := WriteData(AData + CRLF);
End;

End.
