Unit DialogMSSQLConnection;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, Controls, Dialogs, ExtCtrls, FileUtil, Forms, Graphics, LResources,
  StdCtrls, EditBtn, SysUtils, ThirdPartySupport;

Type

  { TdlgMSSQLConnection }

  TdlgMSSQLConnection = Class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    cbWindowsAuthentication: TCheckBox;
    edtPassword: TLabeledEdit;
    edtDatabase: TLabeledEdit;
    edtPort: TLabeledEdit;
    edtUsername: TLabeledEdit;
    edtServer: TLabeledEdit;
    Procedure cbWindowsAuthenticationChange(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
  Private
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
    Property Database: String read GetDatabase write SetDatabase;
    Property Server: String read GetServer write SetServer;
    Property Username: String read GetUsername write SetUsername;
    Property Password: String read GetPassword write SetPassword;
    Property Port: Integer read GetPort write SetPort;
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
begin
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
      finally
        Result := True;
      end;
    end;
  End;
end;

{$R *.lfm}

{ TdlgMSSQLConnection }

Procedure TdlgMSSQLConnection.FormCreate(Sender: TObject);
Begin
End;

Procedure TdlgMSSQLConnection.cbWindowsAuthenticationChange(Sender: TObject);
Begin
  edtUsername.Enabled := Not cbWindowsAuthentication.Checked;
  edtPassword.Enabled := Not cbWindowsAuthentication.Checked;
End;

Procedure TdlgMSSQLConnection.FormDestroy(Sender: TObject);
Begin
End;

Function TdlgMSSQLConnection.GetDatabase: String;
Begin
  Result := edtDatabase.Text;
End;

Function TdlgMSSQLConnection.GetPassword: String;
Begin
  If cbWindowsAuthentication.Checked Then
    Result := ''
  Else
    Result := edtPassword.Text;
End;

Function TdlgMSSQLConnection.GetPort: Integer;
Begin
  Result := StrToIntDef(edtPort.Text, 1433);
End;

Function TdlgMSSQLConnection.GetServer: String;
Begin
  Result := edtServer.Text;
End;

Function TdlgMSSQLConnection.GetUsername: String;
Begin
  If cbWindowsAuthentication.Checked Then
    Result := ''
  Else
    Result := edtUsername.Text;
End;

Procedure TdlgMSSQLConnection.SetDatabase(AValue: String);
Begin
  edtDatabase.Text := AValue;
End;

Procedure TdlgMSSQLConnection.SetPassword(AValue: String);
Begin
  edtPassword.Text := AValue;
End;

Procedure TdlgMSSQLConnection.SetPort(AValue: Integer);
Begin
  edtPort.Text := IntToStr(AValue);
End;

Procedure TdlgMSSQLConnection.SetServer(AValue: String);
Begin
  edtServer.Text := AValue;
End;

Procedure TdlgMSSQLConnection.SetUsername(AValue: String);
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
end;

Initialization
  FMSSQL := nil;

End.
