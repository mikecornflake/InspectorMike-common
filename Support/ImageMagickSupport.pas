Unit ImageMagickSupport;

{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : ImageMagickSupport.pas
  Description
    Support for imagemagick CLI

    My use of imagemagick is so simple, I mostly just use this unit to find WHERE
    I've placed imagemagick.exe...

  Source
    Copyright (c) 2025
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    ~2011: Creation date unknown, original local SVN repository lost
             I do remember this was added to support PDF Viewer during cyclone
             downtime in 2011
           No further commits
    2024-01-22: Migrated to Github .  Refactored package to "IM_units"
    2025-11-29: Added this header

  License
    This file is part of IM_units.lpk.

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

{$mode objfpc}{$H+}

// Running the command line interface itself is so easy I see no
// need to subclass that code.
// This is really here just to find the directory

Interface

Uses
  Classes, SysUtils;

Function ImageMagickAvailable: Boolean;
Function ImageMagickPath: String;
Procedure SetImageMagickPath(AValue: String);
Procedure InitializeImageMagick;

Implementation

Uses
  Forms;

Var
  FImageMagickPath: String;

Function ImageMagickAvailable: Boolean;
Begin
  Result := FImageMagickPath <> '';
End;

Function ImageMagickPath: String;
Begin
  Result := FImageMagickPath;
End;

Procedure SetImageMagickPath(AValue: String);
Begin
  If DirectoryExists(AValue) Then
    FImageMagickPath := AValue
  Else
    FImageMagickPath := '';
End;

Procedure InitializeImageMagick;
Begin
  FImageMagickPath := IncludeTrailingBackslash(Application.Location) + 'ImageMagick';
  If DirectoryExists(FImageMagickPath) Then
    Exit;

  FImageMagickPath := IncludeTrailingBackslash(Application.Location) + '..\ImageMagick';
  If DirectoryExists(FImageMagickPath) Then
    Exit;

  FImageMagickPath := '';
End;

Initialization
  FImageMagickPath := '';

End.
