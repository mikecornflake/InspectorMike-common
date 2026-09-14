Unit MSSQLSupport;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, Controls, Dialogs, ExtCtrls, FileUtil, Forms, Graphics, LResources,
  StdCtrls, SysUtils, ThirdPartySupport, sqldb, mssqlconn, FrameBase;

Type

  { TFrameMSSQLConnection }

  TFrameMSSQLConnection = Class(TFrameBase)
    cbWindowsAuthentication: TCheckBox;
    cboDatabase: TComboBox;
    edtPassword: TLabeledEdit;
    edtPort: TLabeledEdit;
    edtUsername: TLabeledEdit;
    edtServer: TLabeledEdit;
    lblDatabase: TLabel;
    Procedure cboDatabaseDropDown(Sender: TObject);
    Procedure cbWindowsAuthenticationChange(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
  Private
    FDatabasePrefix: String;
    Procedure PopulateDatabaseList;

    Function GetDatabase: String;
    Function GetPassword: String;
    Function GetPort: Integer;
    Function GetServer: String;
    Function GetUsername: String;
    Procedure SetDatabase(AValue: String);
    Procedure SetPassword(AValue: String);
    Procedure SetPort(AValue: Integer);
    Procedure SetServer(AValue: String);
    Procedure SetUsername(AValue: String);
  Public
    Property Database: String Read GetDatabase Write SetDatabase;
    Property Server: String Read GetServer Write SetServer;
    Property Username: String Read GetUsername Write SetUsername;
    Property Password: String Read GetPassword Write SetPassword;
    Property Port: Integer Read GetPort Write SetPort;

    Property DatabasePrefix: String Read FDatabasePrefix Write FDatabasePrefix;
  End;

  { TMSSQLSupport }

  TMSSQLSupport = Class(TThirdParty)
  Public
    Constructor Create; Override;
  End;

Function MSSQL: TMSSQLSupport;
Function RegisterMSSQLDriver: Boolean;

Const
  THIRDPARTY_MSSQL = 'FreeTDS (MS SQL Connection)';

Implementation

Uses dblib, OSSupport;

Var
  FMSSQL: TMSSQLSupport;

Function MSSQL: TMSSQLSupport;
Begin
  If Not Assigned(FMSSQL) Then
    FMSSQL := TMSSQLSupport.Create;

  Result := FMSSQL;
End;

Function RegisterMSSQLDriver: Boolean;
Begin
  Result := False;

  If MSSQL.Available Then
  Begin
    // Have to do it this way as there are other DLLs in the folder that need
    // to be linked in.
    If DirectoryExists(MSSQL.Folder) Then
    Begin
      // This is temporary (per session)
      AddToEnvironmentPath(MSSQL.Folder);

      Try
        InitialiseDBLib('dblib.dll');
      Finally
        Result := True;
      End;
    End;
  End;
End;

{$R *.lfm}

{ TFrameMSSQLConnection }

Procedure TFrameMSSQLConnection.FormCreate(Sender: TObject);
Begin
  FDatabasePrefix := '';
End;

Procedure TFrameMSSQLConnection.FormDestroy(Sender: TObject);
Begin
End;

Procedure TFrameMSSQLConnection.cbWindowsAuthenticationChange(Sender: TObject);
Begin
  edtUsername.Enabled := Not cbWindowsAuthentication.Checked;
  edtPassword.Enabled := Not cbWindowsAuthentication.Checked;
End;

Procedure TFrameMSSQLConnection.PopulateDatabaseList;
Var
  oConn: TMSSQLConnection;
  oTrans: TSQLTransaction;
  oQuery: TSQLQuery;
Begin
  oConn := TMSSQLConnection.Create(nil);
  oTrans := TSQLTransaction.Create(nil);
  oQuery := TSQLQuery.Create(nil);
  Try
    oConn.Transaction := oTrans;

    If (Pos('\', edtServer.Text) > 0) Or (Pos(':', edtServer.Text) > 0) Then
      oConn.Hostname := Trim(edtServer.Text)
    Else
      oConn.Hostname := Format('%s:%s', [Trim(edtServer.Text), Trim(edtPort.Text)]);

    oConn.DatabaseName := 'master';

    If cbWindowsAuthentication.Checked Then
    Begin
      oConn.Username := '';
      oConn.Password := '';
    End
    Else
    Begin
      oConn.Username := Trim(edtUsername.Text);
      oConn.Password := edtPassword.Text;
    End;

    oQuery.Database := oConn;
    oQuery.Transaction := oTrans;

    Screen.Cursor := crHourGlass;
    Try
      oConn.Open;

      oQuery.SQL.Clear;
      oQuery.SQL.Add('select name ' + 'from sys.databases where state = 0 ');
      If (Trim(FDatabasePrefix) <> '') Then
        oQuery.SQL.Add(' and name like ''' + FDatabasePrefix + '%'' ');
      oQuery.SQL.Add('order by name');

      oQuery.Open;

      cboDatabase.Items.BeginUpdate;
      Try
        cboDatabase.Items.Clear;

        While Not oQuery.EOF Do
        Begin
          cboDatabase.Items.Add(oQuery.FieldByName('name').AsString);
          oQuery.Next;
        End;
      Finally
        cboDatabase.Items.EndUpdate;
      End;

      If cboDatabase.Items.Count > 0 Then
        cboDatabase.ItemIndex := 0;
    Finally
      Screen.Cursor := crDefault;
    End;
  Except
    on E: Exception Do
      MessageDlg('Database lookup failed', E.Message,
        mtError, [mbOK], 0);
  End;

  oQuery.Free;
  oTrans.Free;
  oConn.Free;
End;

Procedure TFrameMSSQLConnection.cboDatabaseDropDown(Sender: TObject);
Begin
  If cboDatabase.Items.Count = 0 Then
    PopulateDatabaseList;
End;

Function TFrameMSSQLConnection.GetDatabase: String;
Begin
  Result := cboDatabase.Text;
End;

Function TFrameMSSQLConnection.GetPassword: String;
Begin
  If cbWindowsAuthentication.Checked Then
    Result := ''
  Else
    Result := edtPassword.Text;
End;

Function TFrameMSSQLConnection.GetPort: Integer;
Begin
  Result := StrToIntDef(edtPort.Text, 1433);
End;

Function TFrameMSSQLConnection.GetServer: String;
Begin
  Result := edtServer.Text;
End;

Function TFrameMSSQLConnection.GetUsername: String;
Begin
  If cbWindowsAuthentication.Checked Then
    Result := ''
  Else
    Result := edtUsername.Text;
End;

Procedure TFrameMSSQLConnection.SetDatabase(AValue: String);
Begin
  //cboDatabase.Items.Text := AValue;
  cboDatabase.Text := AValue;
End;

Procedure TFrameMSSQLConnection.SetPassword(AValue: String);
Begin
  edtPassword.Text := AValue;
End;

Procedure TFrameMSSQLConnection.SetPort(AValue: Integer);
Begin
  edtPort.Text := IntToStr(AValue);
End;

Procedure TFrameMSSQLConnection.SetServer(AValue: String);
Begin
  edtServer.Text := AValue;
End;

Procedure TFrameMSSQLConnection.SetUsername(AValue: String);
Begin
  edtUsername.Text := Trim(AValue);

  cbWindowsAuthentication.Checked := (Trim(AValue) = '');
  cbWindowsAuthenticationChange(nil);
End;

{ TMSSQLSupport }

Constructor TMSSQLSupport.Create;
Var
  oDef: TThirdPartyDefinition;
Begin
  oDef := Default(TThirdPartyDefinition);

  // Dynamically Linked DLL
  oDef.Kind := tpkRuntimeLibrary;

  // DLL - we care if the exe is 32bit or 64bit
  oDef.CPUSensitive := True;

  // Preparation for default Initialise
  oDef.KeyFile := 'dblib.dll';
  oDef.KeyFolder := 'dblib';

  // Metadata
  oDef.Name := THIRDPARTY_MSSQL;

  oDef.Summary := 'FreeTDS provide a opensource driver for connecting to various MS SQL Server versions';

  oDef.ProjectURL := 'https://www.freetds.org/';
  oDef.CodeURL := 'https://github.com/FreeTDS/freetds';

  Inherited Create(oDef);
End;

Initialization
  FMSSQL := nil;

End.
