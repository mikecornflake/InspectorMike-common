{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit IM_Shared;

interface

uses
  Exporters, MRUs, Settings, DialogFilter, DockBase, FormAbout, FormMain, 
  FormMultiDock, FormPersistent, FrameBase, FrameCSVs, FrameGrids, 
  FrameImages, FramePDFViewers, FrameRelatedVideos, FrameVideoPlayers, 
  FrameVideos, ControlsSupport, DBSupport, ffmpegSupport, FileSupport, 
  ImageMagickSupport, netMCSupport, OSSupport, StringSupport, VersionSupport, 
  XPDFSupport, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('IM_Shared', @Register);
end.
