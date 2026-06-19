{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit IM_units.LazSerial;

{$warn 5023 off : no warning about unused units}
interface

uses
  LazSerialSupport, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('IM_units.LazSerial', @Register);
end.
