Unit ControlsSupport;

{-------------------------------------------------------------------------------
  Package   : IM_units
  Unit      : ControlsSupport.pas
  Description
    Helper routines for visual controls

  TODO
    Is this still used anywhere?  If not, delete.

  Source
    Copyright (c) 2025
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    ~2008: Creation date unknown, original local SVN repository lost
    2014-07-05: Uploaded to SourceForge/Package "Shared"
                - No further commits
    2024-01-22: Migrated to Github.  Refactored package to "IM_units"
    2025-11-29: Added this header

  License
    This file is part of IM_units.lpk.

    It is free software: you can redistribute it and/or modify it under the
    terms of the GNU General Public License as published by the Free Software
    Foundation, either version 3 of the License, or (at your option) any
    later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along
    with this program.  If not, see <https://www.gnu.org/licenses/>.

    SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------}
{$mode objfpc}{$H+}

Interface

Uses
  Classes, Graphics, ComCtrls, SysUtils, IpHtml;

// Treeview routines
Function SelectNode(oTree: TCustomTreeView; sLocation, sDelimiter: String): Boolean;

// TurboPowerPro Routines
Procedure SetHTML(AIpHtmlPanel: TIpHtmlPanel; AHTML: String);

Implementation

Uses
  StringSupport, Dialogs;

// Treeview routines
Function SelectNode(oTree: TCustomTreeView; sLocation, sDelimiter: String): Boolean;

  Function SelectNode2(oNode: TTreeNode; sRemainingLocation: String): Boolean;
  Var
    sNode: String;
    oTemp: TTreeNode;
    bLast: Boolean;

  Begin
    If Assigned(oNode) Then
    Begin
      oNode.Expand(False);

      bLast := (Pos(sDelimiter, sRemainingLocation) = 0);

      If Not bLast Then
        sNode := TextBetween(sRemainingLocation, '', sDelimiter)
      Else
        sNode := sRemainingLocation;

      oTemp := oNode.FindNode(sNode);

      If Assigned(oTemp) Then
      Begin
        oTemp.Selected := True;

        If bLast Then
          Result := True
        Else
          Result := SelectNode2(oTemp, TextBetween(sRemainingLocation, sDelimiter, ''));
      End
      Else
        Result := False;
    End
    Else
      Result := False;
  End;

Var
  oNode: TTreeNode;
  sNode: String;

Begin
  // Good ol' recursion - it's been a while...

  oNode := oTree.TopItem;

  If Assigned(oNode) Then
  Begin
    While Assigned(oNode.GetPrev) Do
      oNode := oNode.GetPrev;

    // We'll manually locate the first node ourselves.
    If Copy(sLocation, 1, Length(sDelimiter)) = sDelimiter Then
      sLocation := Copy(sLocation, 2, Length(sLocation));

    sNode := ExtractField(sLocation, sDelimiter[1], 0);

    While Assigned(oNode) And (oNode.Text <> sNode) Do
      oNode := oNode.GetNext;

    If Assigned(oNode) Then
      Result := SelectNode2(oNode, TextBetween(sLocation, sDelimiter, ''))
    Else
      Result := False;
  End
  Else
    Result := False;
End;

Procedure SetHTML(AIpHtmlPanel: TIpHtmlPanel; AHTML: String);
Var
  oHTML: TIpHtml;
  oStream: TStringStream;
Begin
  oStream := TStringStream.Create(AHTML);
  Try
    Try
      oHTML := TIpHtml.Create; // Beware: Will be freed automatically by htmlPanel

      oStream.Position := 0;
      oHTML.LoadFromStream(oStream);

      oStream.Position := 0;

      AIpHtmlPanel.SetHtml(oHTML);
    Except
      On E: Exception Do
        MessageDlg('Unable to open HTML' + #13 + 'Error: ' + E.Message,
          mtError, [mbCancel], 0);
    End;
  Finally
    oStream.Free;
  End;
End;

End.
