{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit IM_database;

{$warn 5023 off : no warning about unused units}
interface

uses
  Exporters, ExportersFPVectorial, DBSupport, DialogDBGridColEditor, 
  DialogSQLFilter, FrameCSVs, FrameGrids, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('IM_database', @Register);
end.
