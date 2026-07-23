{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit IM_units;

{$warn 5023 off : no warning about unused units}
interface

uses
  Logs, MRUs, Settings, ffmpegSupport, FileSupport, GPSSupport, 
  ImageMagickSupport, LibmpvSupport, netMCSupport, OSSupport, StringSupport, 
  TesseractSupport, VersionSupport, WGS84, XMLSupport, XPDFSupport, 
  InspectionSupport, qpdfSupport, PDF, ImageSupport, PopplerSupport, 
  ThirdPartySupport, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('IM_units', @Register);
end.
