Unit XMLSupport;

{$mode objfpc}{$H+}
{$codepage utf8}
{$WARN 4105 off : Implicit string type conversion with potential data loss from "$1" to "$2"}
Interface

Uses
  Classes, SysUtils, DOM;

Type

  { TXMLHelper }

  TXMLHelper = Class
  Private
    Class Function FromFragment(Const AXML: String): TXMLDocument;
  Public
    Class Function NodeToXML(ANode: TDOMNode): String;

    Class Function GetXMLFragment(Const AXML, ANodeName: String): String;
    Class Function SetXMLFragment(Const ANodeName, AFragment: String): String;

    Class Function GetNodeText(Const AXML, ANodeName: String): String;
    Class Function GetNodeInt(Const AXML, ANodeName: String; ADefault: Integer = 0): Integer;
    Class Function GetNodeBool(Const AXML, ANodeName: String; ADefault: Boolean = False): Boolean;
    Class Function SetNodeText(Const AName, AValue: String): String;

    Class Function SetSetting(Const AName, AValue: String): String;
    Class Function GetSetting(Const AXML, AName: String): String;

    Class Function GetAllFragments(Const AXML, ANodeName: String): TStringArray; //TODO
    Class Function GetAllNodeTexts(Const AXML, ANodeName: String): TStringArray; //TODO

    Class Function EscapeXML(Const AXML: String): String;
    Class Function UnescapeXML(Const AXML: String): String;
  End;

Implementation

Uses
  XMLRead, XMLWrite, StringSupport;

  { TXMLHelper }

Class Function TXMLHelper.FromFragment(Const AXML: String): TXMLDocument;
Var
  Stream: TStringStream;
  sDummy: String;
Begin
  sDummy := '<__root__>' + AXML + '</__root__>';
  Stream := TStringStream.Create(sDummy, TEncoding.UTF8);
  Try
    ReadXMLFile(Result, Stream);
  Finally
    Stream.Free;
  End;
End;

Class Function TXMLHelper.NodeToXML(ANode: TDOMNode): String;
Var
  Stream: TStringStream;
Begin
  Result := '';
  If Not Assigned(ANode) Then Exit;

  Stream := TStringStream.Create('', TEncoding.UTF8);
  Try
    WriteXML(ANode, Stream);
    Result := Stream.DataString;
  Finally
    Stream.Free;
  End;
End;

Class Function TXMLHelper.GetXMLFragment(Const AXML, ANodeName: String): String;
Var
  sStart, sEnd: String;
  iStart, iEnd: Integer;
Begin
  Result := '';

  If AXML = '' Then
    Exit;

  sStart := '<' + ANodeName;
  sEnd := '</' + ANodeName + '>';

  iStart := Pos(sStart, AXML);
  If iStart > 0 Then
  Begin
    iStart := Pos('>', AXML, iStart);
    If iStart > 0 Then
      Inc(iStart);
  End;
  iEnd := Pos(sEnd, AXML);

  If (iStart > 0) And (iEnd > iStart) Then
    Result := Copy(AXML, iStart, iEnd - iStart);
End;

Class Function TXMLHelper.SetXMLFragment(Const ANodeName, AFragment: String): String;
Begin
  Result := Format('<%s>%s</%s>', [ANodeName, AFragment, ANodeName]);
End;

Class Function TXMLHelper.GetNodeText(Const AXML, ANodeName: String): String;
Var
  Doc: TXMLDocument;
  Node: TDOMNode;

  Function FindNodeRecursive(Parent: TDOMNode; Const Target: String): TDOMNode;
  Var
    i: Integer;
  Begin
    Result := nil;
    If SameText(Parent.NodeName, Target) Then
      Exit(Parent);

    For i := 0 To Parent.ChildNodes.Count - 1 Do
    Begin
      Result := FindNodeRecursive(Parent.ChildNodes[i], Target);
      If Assigned(Result) Then Exit;
    End;
  End;

Begin
  Result := '';
  Try
    Doc := FromFragment(AXML); // Wraps with <__root__>...</__root__>
    Node := FindNodeRecursive(Doc.DocumentElement, ANodeName);
    If Assigned(Node) Then
      Result := UnescapeXML(Node.TextContent);
    Doc.Free;
  Except
    Result := '';
  End;
End;

Class Function TXMLHelper.GetNodeInt(Const AXML, ANodeName: String; ADefault: Integer): Integer;
Begin
  Result := StrToIntDef(GetNodeText(AXML, ANodeName), ADefault);
End;

Class Function TXMLHelper.GetNodeBool(Const AXML, ANodeName: String; ADefault: Boolean): Boolean;
Begin
  Result := StrToBoolDef(GetNodeText(AXML, ANodeName), ADefault);
End;

Class Function TXMLHelper.SetNodeText(Const AName, AValue: String): String;
Begin
  Result := Format('<%s>%s</%s>', [AName, EscapeXML(AValue), AName]);
End;

Class Function TXMLHelper.SetSetting(Const AName, AValue: String): String;
Begin
  Result := Format('<setting name="%s" value="%s"/>', [AName, EscapeXML(AValue)]);
End;

Function FindSettingRecursive(Node: TDOMNode; Const AName: String): String;
Var
  i: Integer;
  Elem: TDOMElement;
Begin
  Result := '';

  If SameText(Node.NodeName, 'setting') And (Node Is TDOMElement) Then
  Begin
    Elem := TDOMElement(Node);
    If Elem.HasAttribute('name') And SameText(Elem.GetAttribute('name'), AName) Then
      Exit(Elem.GetAttribute('value'));
  End;

  For i := 0 To Node.ChildNodes.Count - 1 Do
  Begin
    Result := FindSettingRecursive(Node.ChildNodes[i], AName);
    If Result <> '' Then
      Exit;
  End;
End;

Class Function TXMLHelper.GetSetting(Const AXML, AName: String): String;
Var
  Doc: TXMLDocument;
Begin
  Result := '';
  Try
    Doc := FromFragment(AXML);
    Result := UnescapeXML(FindSettingRecursive(Doc.DocumentElement, AName));
    Doc.Free;
  Except
    Result := '';
  End;
End;

Class Function TXMLHelper.GetAllFragments(Const AXML, ANodeName: String): TStringArray;
Begin
  // TODO
End;

Class Function TXMLHelper.GetAllNodeTexts(Const AXML, ANodeName: String): TStringArray;
Begin
  // TODO
End;

Class Function TXMLHelper.EscapeXML(Const AXML: String): String;
Begin
  Result := StringReplace(AXML, '&', '&amp;', [rfReplaceAll]); // Has to be first
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '&apos;', [rfReplaceAll]);
End;

Class Function TXMLHelper.UnescapeXML(Const AXML: String): String;
Begin
  Result := StringReplace(AXML, '&lt;', '<', [rfReplaceAll]);
  Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll]);
  Result := StringReplace(Result, '&apos;', '''', [rfReplaceAll]);
  Result := StringReplace(Result, '&quot;', '"', [rfReplaceAll]);
  Result := StringReplace(Result, '&amp;', '&', [rfReplaceAll]);  // has to be last
End;

End.
