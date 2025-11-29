Unit GPSSupport;

{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : GPSSupport.pas
  Description
    Helper unit for GPS Operations
    Works closely with WGS84.pas
      See https://forum.lazarus.freepascal.org/index.php?topic=57818.msg456377#msg456377

    Currently being migrated to a NMEA String helper unit as GPS is a subset...

  Source
    Copyright (c) 2025
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    2022-10-08: Creation and upload to Github
    2022-12-07: Worked around Hemisphere oddity in WGS84.pas
    2025-11-29: Added this header (and fixed minor typo in code)

  License
    This file is part of IM_units.lpk.

    It is free software: you can redistribute it and/or modify it under the
    terms of the GNU General Public License as published by the Free Software
    Foundation, either version 3 of the License, or (at your option) any
    later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program.  If not, see <https://www.gnu.org/licenses/>.

    SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------}

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, WGS84;

// If you want to convert from EN to LATLON, then
// one of these HAS to be called first
Procedure InitialiseGPS(ALat, ALon: Double); Overload;
Procedure InitialiseGPS(AFuseau: Integer; ASouthernHemisphere: Boolean); Overload;

Function GetWGS84: TWGS84;
Procedure LatLonToEN(ALat, ALon: Double; Var AEast, ANorth: Double);
Procedure ENToLatLon(AEast, ANorth: Double; Var ALat, ALon: Double);

Function NMEA_GPGGA(ALat, ALon: Double; AAlt: Double = 0): String;
Function NMEA_GPGLL(ALat, ALon: Double): String;
Function NMEA_GPRMC(ALat, ALon: Double): String;
Function NMEA_GPGGAbyEN(AEast, ANorth: Double; AAlt: Double = 0): String;
Function NMEA_GPGLLbyEN(AEast, ANorth: Double): String;
Function NMEA_GPRMCbyEN(AEast, ANorth: Double): String;

Function LatAsDDMM(ALat: Double): String;
Function LonAsDDDMM(ALon: Double): String;

// Fuseau is the UTM Zone.  A number between 1 and 60
// Suspect this is the Swedish term for "UTM Zone"
// For consistency I'm sticking to the original
// nomenclature used in unit WGS84.
Function Fuseau: Integer;
Function SouthernHemisphere: Boolean;
Function Hemisphere: String;

Function NMEA_Checksum(AInput: String): Byte;
Function NMEA_ChecksumAsHex(AInput: String): String;

Implementation

Uses
  StringSupport, Math;

Var
  FWGS84: TWGS84;
  FFuseau: Integer;
  FSouthernHemi: Boolean;

Function GetWGS84: TWGS84;
Begin
  If Not Assigned(FWGS84) Then
  Begin
    FWGS84 := TWGS84.Create;
    FWGS84.IgnoreGeodeticOrientation := True;
  End;

  Result := FWGS84;
End;

Procedure InitialiseGPS(ALat, ALon: Double);
Begin
  FFuseau := floor((ALon + 180) / 6) + 1;
  FSouthernHemi := (ALat < 0);
End;

Procedure InitialiseGPS(AFuseau: Integer; ASouthernHemisphere: Boolean);
Begin
  FFuseau := AFuseau;
  FSouthernHemi := ASouthernHemisphere;
End;

Procedure LatLonToEN(ALat, ALon: Double; Var AEast, ANorth: Double);
Var
  oLatLon: TrecLatLon;
  oUTM: TrecUTM;
  oWGS84: TWGS84;
Begin
  oLatLon.Lat := ALat;
  oLatLon.Lon := ALon;

  oWGS84 := GetWGS84;
  oWGS84.WGS84ToUTM(oLatLon, oUTM{%H-});

  FFuseau := oUTM.fuseau;
  FSouthernHemi := oUTM.southhemi;

  AEast := oUTM.X;
  ANorth := oUTM.Y;
End;

Procedure ENToLatLon(AEast, ANorth: Double; Var ALat, ALon: Double);
Var
  oLatLon: TrecLatLon;
  oUTM: TrecUTM;
  oWGS84: TWGS84;
Begin
  If FFuseau = -1 Then
    Raise Exception.Create('Fuseau (UTM Zone) not defined.  Call InitialiseGPS first.');

  oUTM.X := AEast;
  oUTM.Y := ANorth;

  oUTM.southhemi := FSouthernHemi;
  oUTM.fuseau := FFuseau;

  oWGS84 := GetWGS84;
  oWGS84.UTMToWGS84(oUTM, oLatLon{%H-});

  ALat := oLatLon.Lat;
  ALon := oLatLon.Lon;
End;

Function NMEA_Checksum(AInput: String): Byte;    // calculate Checksum
Var
  x: Integer;
  cCheckCalc: Byte;
Begin
  x := 1;
  cCheckCalc := 0;
  While AInput[x] <> '*' Do
  Begin
    If AInput[x] <> '$' Then  cCheckCalc := Ord(AInput[x]) Xor cCheckCalc;
    x := x + 1;
  End;
  Result := cCheckCalc;
End;

Function NMEA_ChecksumAsHex(AInput: String): String;
Begin
  Result := IntToHex(NMEA_Checksum(AInput), 2);
End;

// https://docs.novatel.com/OEM7/Content/Logs/GPGGA.htm
Function NMEA_GPGGA(ALat, ALon: Double; AAlt: Double): String;
Var
  sLat, sLon: String;
Begin
  If ALat > 0 Then
    sLat := 'N'
  Else
    sLat := 'S';

  If ALon > 0 Then
    sLon := 'E'
  Else
    sLon := 'W';

  Result := Format('$GPGGA,%s,%s,%s,%s,%s,8,1,1,%.3f,M,1,M,,*',
    [FormatDateTime('hhnnss".00"', now), LatAsDDMM(ALat), sLat, LonAsDDDMM(ALon), sLon, AAlt]);
  Result := Result + NMEA_ChecksumAsHex(Result);

  //Str := '$GPGGA,' +
  //       FormatDateTime('hhnnss".00,"',now ) +
  //       ChaineLatitude +
  //       ',' + PoleLat + ',' +
  //       ChaineLongitude +
  //       ',' + PoleLon + ',' +
  //       '1,04,47.56,' + EditAlt.Text + ',M,10,M,,*';
  //Str := Str + inttohex(checksum(Str),2)+ Char(13)+ Char(10);
End;

// https://docs.novatel.com/OEM7/Content/Logs/GPGLL.htm
Function NMEA_GPGLL(ALat, ALon: Double): String;
Var
  sLat, sLon: String;
Begin
  If ALat > 0 Then
    sLat := 'N'
  Else
    sLat := 'S';

  If ALon > 0 Then
    sLon := 'E'
  Else
    sLon := 'W';

  Result := Format('$GPGLL,%s,%s,%s,%s,%s,A,M*', [LatAsDDMM(ALat), sLat,
    LonAsDDDMM(ALon), sLon, FormatDateTime('hhnnss".00"', now)]);
  Result := Result + NMEA_ChecksumAsHex(Result);

  //Str := '$GPGLL,' +
  //     ChaineLatitude +
  //     ',' + PoleLat + ',' +
  //     ChaineLongitude +
  //     ',' + PoleLon + ',' +
  //     FormatDateTime('hhnnss".00,"',now ) +
  //     'A*';
End;

// https://docs.novatel.com/OEM7/Content/Logs/GPRMC.htm
Function NMEA_GPRMC(ALat, ALon: Double): String;
Var
  sLat, sLon: String;
Begin
  If ALat > 0 Then
    sLat := 'N'
  Else
    sLat := 'S';

  If ALon > 0 Then
    sLon := 'E'
  Else
    sLon := 'W';

  Result := Format('$GPRMC,%s,A,%s,%s,%s,%s,,,%s,,E*',
    [FormatDateTime('hhnnss', now), LatAsDDMM(ALat), sLat, LonAsDDDMM(ALon),
    sLon, FormatDateTime('ddmmyy', now)]);
  Result := Result + NMEA_ChecksumAsHex(Result);
  //Str := '$GPRMC,' +
  //       FormatDateTime('hhnnss","',now ) +
  //       'A,' +
  //       ChaineLatitude +
  //       ',' + PoleLat + ',' +
  //       ChaineLongitude +
  //       ',' + PoleLon + ',' +
  //       '000.5,054.7,' +
  //       FormatDateTime('ddmmyy","',now ) +
  //       '020.3,E*';
End;

Function NMEA_GPGGAbyEN(AEast, ANorth: Double; AAlt: Double): String;
Var
  dLat: Double = 0;
  dLon: Double = 0;
Begin
  ENToLatLon(AEast, ANorth, dLat, dLon);
  Result := NMEA_GPGGA(dLat, dLon, AAlt);
End;

Function NMEA_GPGLLbyEN(AEast, ANorth: Double): String;
Var
  dLat: Double = 0;
  dLon: Double = 0;
Begin
  ENToLatLon(AEast, ANorth, dLat, dLon);
  Result := NMEA_GPGLL(dLat, dLon);
End;

Function NMEA_GPRMCbyEN(AEast, ANorth: Double): String;
Var
  dLat: Double = 0;
  dLon: Double = 0;
Begin
  ENToLatLon(AEast, ANorth, dLat, dLon);
  Result := NMEA_GPRMC(dLat, dLon);
End;

Function LatAsDDMM(ALat: Double): String;
Begin
  Result := FormatFloat('0000.0000000', Trunc(Abs(ALat)) * 100 +
    (Abs(ALat) - Trunc(Abs(ALat))) * 60);
End;

Function LonAsDDDMM(ALon: Double): String;
Begin
  Result := FormatFloat('00000.0000000', Trunc(Abs(ALon)) * 100 +
    (Abs(ALon) - Trunc(Abs(ALon))) * 60);
End;

Function Fuseau: Integer;
Begin
  Result := FFuseau;
End;

Function SouthernHemisphere: Boolean;
Begin
  Result := FSouthernHemi;
End;

Function Hemisphere: String;
Begin
  If FSouthernHemi Then
    Result := 'Southern hemisphere'
  Else
    Result := 'Northern hemisphere';
End;

Initialization
  FWGS84 := nil;
  FFuseau := -1;
  FSouthernHemi := True;

Finalization
  FreeAndNil(FWGS84);

End.
