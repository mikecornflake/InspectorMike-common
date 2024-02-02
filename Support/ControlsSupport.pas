Unit ControlsSupport;

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
