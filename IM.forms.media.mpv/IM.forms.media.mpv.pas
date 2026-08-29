{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit IM.forms.media.mpv;

{$warn 5023 off : no warning about unused units}
interface

uses
  FrameVideoLibmpv, LibmpvSupport, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('IM.forms.media.mpv', @Register);
end.
