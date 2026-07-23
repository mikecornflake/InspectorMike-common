Unit ImageSupport;

{$mode objfpc}{$H+}
{$WARN 5044 off : Symbol "$1" is not portable}
{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : ImageSupport.pas
  Description
    Image processing routines

    Trying to reduce ImageMagick usage

  Source
    Copyright (c) 2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    2026-07-23: Created unit

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
Interface

Uses
  Classes, SysUtils, Graphics, BGRABitmap, BGRABitmapTypes;

// On disk image manipulation
// Filename should, in theory, take any image extension
Function LoadImageIntoBitmap(Const AFilename: String; Const ABitmap: TBitmap): Boolean;
Function SaveBitmapToFile(Const AFilename: String; Const ABitmap: TBitmap): Boolean;

// Wrappers for BGRABitmap functionality
Function CreateThumbnail(Const ASourceFile, ADestFile: String; W, H: Integer): Boolean;
Function RotateImageFile90CCW(Const ASourceFile, ADestFile: String): Boolean;
Function RotateImageFile90CW(Const ASourceFile, ADestFile: String): Boolean;

Implementation

Function CreateThumbnail(Const ASourceFile, ADestFile: String; W, H: Integer): Boolean;
Var
  oSource: TBGRABitmap;
  oThumb: TBGRABitmap;
Begin
  Result := False;

  If Not FileExists(ASourceFile) Then
    Exit;

  oSource := TBGRABitmap.Create(ASourceFile);
  Try
    oThumb := oSource.Resample(W, H) As TBGRABitmap;
    Try
      oThumb.SaveToFile(ADestFile);
      Result := True;
    Finally
      oThumb.Free;
    End;
  Finally
    oSource.Free;
  End;
End;

Function LoadImageIntoBitmap(Const AFilename: String; Const ABitmap: TBitmap): Boolean;
Var
  oPict: TPicture;
Begin
  Result := False;

  Assert(Assigned(ABitmap), 'LoadImageIntoBitmap: Needs created TBitmap');

  If Not FileExists(AFilename) Then
    Exit;

  oPict := TPicture.Create;
  Try
    Try
      oPict.LoadFromFile(AFilename);

      If Not Assigned(oPict.Graphic) Then
        Exit;

      If oPict.Graphic.Empty Then
        Exit;

      ABitmap.Assign(oPict.Graphic);

      Result := True;
    Except
      Result := False;
    End;
  Finally
    oPict.Free;
  End;
End;

Function SaveBitmapToFile(Const AFilename: String; Const ABitmap: TBitmap): Boolean;
Var
  oPict: TPicture;
  oStream: TFileStream;
Begin
  Result := False;

  Assert(Assigned(ABitmap), 'SaveBitmapToFile: Needs created TBitmap');

  oPict := TPicture.Create;
  Try
    oPict.Assign(ABitmap);

    oStream := TFileStream.Create(AFilename, fmCreate);
    Try
      oPict.SaveToStreamWithFileExt(oStream,
        ExtractFileExt(AFilename));

      Result := True;
    Finally
      oStream.Free;
    End;
  Finally
    oPict.Free;
  End;
End;

Function RotateImageFile90CCW(Const ASourceFile, ADestFile: String): Boolean;
Var
  bmpSource: TBGRABitmap;
  bmpDest: TBGRABitmap;
Begin
  Result := False;

  bmpSource := TBGRABitmap.Create(ASourceFile);
  Try
    bmpDest := bmpSource.RotateCCW;
    Try
      bmpDest.SaveToFile(ADestFile);
      Result := True;
    Finally
      bmpDest.Free;
    End;
  Finally
    bmpSource.Free;
  End;
End;

Function RotateImageFile90CW(Const ASourceFile, ADestFile: String): Boolean;
Var
  bmpSource: TBGRABitmap;
  bmpDest: TBGRABitmap;
Begin
  Result := False;

  bmpSource := TBGRABitmap.Create(ASourceFile);
  Try
    bmpDest := bmpSource.RotateCW;
    Try
      bmpDest.SaveToFile(ADestFile);
      Result := True;
    Finally
      bmpDest.Free;
    End;
  Finally
    bmpSource.Free;
  End;
End;


End.
