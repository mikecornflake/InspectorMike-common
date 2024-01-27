{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit IM_application;

{$warn 5023 off : no warning about unused units}
interface

uses
  DockBase, FormAbout, FormMain, FormMultiDock, FormPersistent, FrameBase, 
  FrameCSVs, FrameGrids, FrameImages, FramePDFViewers, FrameRelatedVideos, 
  FrameVideoPlayers, FrameHTMLs, DialogSQLFilter, DialogDockManager, 
  DockManagers, FrameVideoBase, FrameVideoDirectShow, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('IM_application', @Register);
end.
