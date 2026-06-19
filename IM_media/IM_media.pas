{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit IM_media;

{$warn 5023 off : no warning about unused units}
interface

uses
  VideoGridLayout, FrameImages, FramePDFViewers, FrameRelatedVideos, 
  FrameSyncedVideo, FrameVideoPlayer, FrameVideoLibmpv, FrameVideoBase,
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('IM_media', @Register);
end.
