Unit SQLSupport;

{$mode objfpc}{$H+}

{-------------------------------------------------------------------------------
  Package   : IM_database
  Unit      : DSQLSupport.pas
  Description
    Small SQL generation helpers.

    Notes:
      * These routines generate SQL text. Prefer parameterised queries for new
        database execution code where possible.
      * SQLLiteral is for data values.
      * SQLRawExpression is deliberately named: use it only for trusted SQL
        fragments such as sequence calls or database functions.
      * Table and field names are treated as identifiers, not values. They are
        checked for a conservative set of unquoted identifier characters.
  Source
    Copyright (c) 2026
    Inspector Mike 2.0 Pty Ltd
    Mike Thompson (mike.cornflake@gmail.com)

  History
    2026-06-20: Created by ChatGPT 5.5 with guidance from Mike Thompson
                Original routines in DBSupport.pas, migrated here with
                substantial revision.

  License
    This file is part of IM_database.lpk.

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
  Classes, SysUtils, DB;

Function SQLIdentifier(Const AName: String): String;
Function SQLQuote(Const AValue: String): String;
Function SQLLiteral(Const AValue: String): String;
Function SQLRawExpression(Const AExpression: String): String;
Function SQLValue(AField: TField): String;

Function DeleteSQL(ATable: String; FKeyFields: TStringList; ADataset: TDataset): String;
Function InsertSQL(ATable: String; ADataset: TDataset): String;
Function InsertSQLWhereNotExists(ATable: String; ADataset: TDataset;
  AOracle: Boolean = True): String;

Function FormatSQLLiterals(ASQL: String; Const AValues: Array Of Const): String;

Implementation

Function IsIdentifierChar(Const C: Char): Boolean;
Begin
  Result := (C In ['A'..'Z']) Or
            (C In ['a'..'z']) Or
            (C In ['0'..'9']) Or
            (C In ['_', '.', '$']);
End;

Function SQLIdentifier(Const AName: String): String;
Var
  i: Integer;
Begin
  Result := Trim(AName);

  If Result = '' Then
    Raise Exception.Create('Blank SQL identifier');

  For i := 1 To Length(Result) Do
    If Not IsIdentifierChar(Result[i]) Then
      Raise Exception.CreateFmt('Unsafe SQL identifier: %s', [AName]);
End;

Function SQLQuote(Const AValue: String): String;
Begin
  Result := '''' + StringReplace(AValue, '''', '''''', [rfReplaceAll]) + '''';
End;

Function SQLLiteral(Const AValue: String): String;
Var
  sValue: String;
Begin
  sValue := Trim(AValue);

  If UpperCase(sValue) = 'NULL' Then
    Result := 'NULL'
  Else
    Result := SQLQuote(sValue);
End;

Function SQLRawExpression(Const AExpression: String): String;
Begin
  Result := Trim(AExpression);

  If Result = '' Then
    Raise Exception.Create('Blank SQL expression');
End;

Function SQLValue(AField: TField): String;
Begin
  If AField.IsNull Then
    Result := 'NULL'
  Else
    Result := SQLQuote(AField.AsString);
End;

Function KeyFieldName(AKeyFields: TStringList; AIndex: Integer): String;
Begin
  Result := Trim(AKeyFields.Names[AIndex]);

  If Result = '' Then
    Result := Trim(AKeyFields[AIndex]);
End;

Function DeleteSQL(ATable: String; FKeyFields: TStringList; ADataset: TDataset): String;
Var
  i: Integer;
  sWhere: String;
  sField: String;
  oField: TField;
Begin
  Result := '';

  If (Not Assigned(ADataset)) Or (Not ADataset.Active) Or
     (Not Assigned(FKeyFields)) Or (FKeyFields.Count = 0) Then
    Exit;

  sWhere := '';

  For i := 0 To FKeyFields.Count - 1 Do
  Begin
    sField := SQLIdentifier(KeyFieldName(FKeyFields, i));
    oField := ADataset.FieldByName(sField);

    If sWhere <> '' Then
      sWhere := sWhere + ' AND ';

    If oField.IsNull Then
      sWhere := sWhere + Format('%s IS NULL', [sField])
    Else
      sWhere := sWhere + Format('%s=%s', [sField, SQLQuote(oField.AsString)]);
  End;

  If sWhere <> '' Then
    Result := Format('DELETE FROM %s WHERE %s; ', [SQLIdentifier(ATable), sWhere]);
End;

Function InsertSQL(ATable: String; ADataset: TDataset): String;
Var
  sNames: String;
  sValues: String;
  i: Integer;
  oField: TField;

  Procedure AddField(Const AName, AValue: String);
  Begin
    If sNames <> '' Then
    Begin
      sNames := sNames + ', ';
      sValues := sValues + ', ';
    End;

    sNames := sNames + SQLIdentifier(AName);
    sValues := sValues + AValue;
  End;

Begin
  Result := '';

  If (Not Assigned(ADataset)) Or (Not ADataset.Active) Then
    Exit;

  sNames := '';
  sValues := '';

  For i := 0 To ADataset.FieldCount - 1 Do
  Begin
    oField := ADataset.Fields[i];

    If oField.Visible Then
      AddField(oField.FieldName, SQLValue(oField));
  End;

  If sNames <> '' Then
    Result := Format('INSERT INTO %s (%s) VALUES (%s); ',
      [SQLIdentifier(ATable), sNames, sValues]);
End;

Function InsertSQLWhereNotExists(ATable: String; ADataset: TDataset;
  AOracle: Boolean): String;
Var
  sNames: String;
  sWhere: String;
  sValues: String;
  sTable: String;
  i: Integer;
  oField: TField;

  Procedure AddField(AField: TField);
  Var
    sName: String;
    sValue: String;
  Begin
    sName := SQLIdentifier(AField.FieldName);
    sValue := SQLValue(AField);

    If sNames <> '' Then
    Begin
      sNames := sNames + ', ';
      sWhere := sWhere + ' AND ';
      sValues := sValues + ', ';
    End;

    sNames := sNames + sName;
    sValues := sValues + sValue;

    If AField.IsNull Then
      sWhere := sWhere + Format('%s IS NULL', [sName])
    Else
      sWhere := sWhere + Format('%s=%s', [sName, sValue]);
  End;

Begin
  Result := '';

  If (Not Assigned(ADataset)) Or (Not ADataset.Active) Then
    Exit;

  sNames := '';
  sWhere := '';
  sValues := '';

  For i := 0 To ADataset.FieldCount - 1 Do
  Begin
    oField := ADataset.Fields[i];

    If oField.Visible Then
      AddField(oField);
  End;

  If sNames = '' Then
    Exit;

  sTable := SQLIdentifier(ATable);

  If AOracle Then
    Result := Format(
      'INSERT INTO %s (%s) SELECT %s FROM DUAL WHERE NOT EXISTS (SELECT 1 FROM %s WHERE %s); ',
      [sTable, sNames, sValues, sTable, sWhere])
  Else
    Result := Format(
      'INSERT INTO %s (%s) SELECT %s WHERE NOT EXISTS (SELECT 1 FROM %s WHERE %s); ',
      [sTable, sNames, sValues, sTable, sWhere]);
End;

Function FormatSQLLiterals(ASQL: String; Const AValues: Array Of Const): String;
Var
  arrStrings: Array Of String;
  arrArgs: Array Of TVarRec;
  i: Integer;
Begin
  { Converts string/char arguments to SQL literals before passing to Format.
    This is retained for generating SQL scripts. It is not a replacement for
    parameterised queries. }

  SetLength(arrStrings{%H-}, Length(AValues));
  SetLength(arrArgs{%H-}, Length(AValues));

  For i := Low(AValues) To High(AValues) Do
  Begin
    arrArgs[i] := AValues[i];

    Case AValues[i].vType Of
      vtAnsiString:
      Begin
        arrStrings[i] := SQLLiteral(AnsiString(AValues[i].vAnsiString));
        arrArgs[i].vAnsiString := Pointer(arrStrings[i]);
        arrArgs[i].vType := vtAnsiString;
      End;

      vtString:
      Begin
        arrStrings[i] := SQLLiteral(String(AValues[i].vString^));
        arrArgs[i].vAnsiString := Pointer(arrStrings[i]);
        arrArgs[i].vType := vtAnsiString;
      End;

      vtPChar:
      Begin
        arrStrings[i] := SQLLiteral(String(AValues[i].vPChar));
        arrArgs[i].vAnsiString := Pointer(arrStrings[i]);
        arrArgs[i].vType := vtAnsiString;
      End;

      vtChar:
      Begin
        arrStrings[i] := SQLLiteral(AValues[i].vChar);
        arrArgs[i].vAnsiString := Pointer(arrStrings[i]);
        arrArgs[i].vType := vtAnsiString;
      End;

      vtWideChar:
      Begin
        arrStrings[i] := SQLLiteral(UnicodeString(AValues[i].vWideChar));
        arrArgs[i].vAnsiString := Pointer(arrStrings[i]);
        arrArgs[i].vType := vtAnsiString;
      End;

    End;
  End;

  Result := Format(ASQL, arrArgs);
End;

End.
