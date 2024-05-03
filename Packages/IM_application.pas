{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

Unit IM_application;

{$warn 5023 off : no warning about unused units}
Interface

Uses
  DockBase, FormAbout, FormMain, FormMultiDock, FormPersistent, FrameBase,
  FrameCSVs, FrameGrids, FrameImages, FramePDFViewers, FrameRelatedVideos,
  FrameVideoPlayers, FrameHTMLs, DialogSQLFilter, DialogDockManager,
  FrameVideoBase, FrameVideoDirectShow, DockManagers, LazarusPackageIntf;

Implementation

Procedure Register;
Begin
End;

Initialization
  RegisterPackage('IM_application', @Register);
End.
