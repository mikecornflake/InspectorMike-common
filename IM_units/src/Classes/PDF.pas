Unit PDF;

{$mode objfpc}{$H+}

{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : PDF.pas
  Description
    Helpers for working with PDFs

  Source
    Copyright (c) 2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    2026-07-10: Created unit

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
  Classes, SysUtils, fgl;

  { TPDFAttachment }
Type

  TPDFAttachment = Class
    SourceFilename: String;
    Filename: String;
    Description: String;
    CreationDate: TDateTime;
    ModificationDate: TDateTime;

    Function DisplayText: String;
    Function CreationDateAsText: String;
  End;

  { TPDFAttachments }

  TPDFAttachments = Class(Specialize TFPGObjectList<TPDFAttachment>)
  Public
    Function Remove(AAttachment: TPDFAttachment): Boolean;
  End;

Implementation

Uses
  StringSupport;

  { TPDFAttachment }

Function TPDFAttachment.DisplayText: String;
Begin
  Result := '';

  If SourceFilename <> '' Then
    Result += Format('  SourceFilename="%s" %s', [SourceFilename, LineEnding]);

  If Filename <> '' Then
    Result += Format('  Filename="%s" %s', [Filename, LineEnding]);

  If Description <> '' Then
    Result += Format('    Description="%s" %s', [Description, LineEnding]);

  If CreationDate <> 0 Then
    Result += Format('    CreationDate="%s" %s', [CreationDateAsText, LineEnding]);

  If ModificationDate <> 0 Then
    Result += Format('    ModificationDate="%s" %s',
      [FormatDateTime('yyyy-mm-dd HH:nn:ss', ModificationDate), LineEnding]);
End;

Function TPDFAttachment.CreationDateAsText: String;
Begin
  If CreationDate = 0 Then
    Result := ''
  Else
    Result := FormatDateTime('yyyy-mm-dd HH:nn:ss', CreationDate);
End;

{ TPDFAttachments }

Function TPDFAttachments.Remove(AAttachment: TPDFAttachment): Boolean;
Var
  i: Integer;
Begin
  i := IndexOf(AAttachment);
  Result := (i >= 0);

  If Result Then
    Delete(i);
End;

End.
