{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit IM_units;

{-------------------------------------------------------------------------------
  Package   : IM_application
  Unit      : IM_units.pas
  Description
    Lazarus package registration unit aggregating core support modules.

  Source
    Copyright (c) 2025
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    ~2008: Creation date unknown, original local SVN repository lost
    2014-07-05: Uploaded to SourceForge/Package "Shared"
    2024-01-22: Migrated to Github.  Refactored package to "IM_application"
    2025-11-29: Added LGPL-3.0-or-later license header

  License
    This file is part of IM_application.lpk.

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



{$warn 5023 off : no warning about unused units}
interface

uses
  Settings, Exporters, Logs, MRUs, ControlsSupport, DBSupport, ffmpegSupport, 
  FileSupport, GPSSupport, ImageMagickSupport, LazSerialSupport, netMCSupport, 
  OSSupport, StringSupport, TesseractSupport, VersionSupport, WGS84, 
  XPDFSupport, DialogDBGridColEditor, ExportersFPVectorial, PipelineDisplay, 
  XMLSupport, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('IM_units', @Register);
end.
