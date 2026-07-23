Unit ThirdPartySupport;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, fgl;

Type

  { TThirdParty }

  TThirdParty = Class
  Private
  Protected
    FLicense: String;
    FReadMe: String;

    FAvailable: Boolean;
    FCodeURL: String;
    FName: String;
    FSummary: String;
    FProjectURL: String;

    // If the third party dependency
    // has a key identifiable file or dll,
    // then declare it here for default Initialise
    FKeyFile: String;

    // If the third party dependency
    // has a key identifiable folder to the FRequiredFile,
    // then declare it here for default Initialise
    FKeyFolder: String;

    // if the third party dependency is CPU dependant?
    //  ie i386, x86_64 etc
    // - for drivers, dlls etc
    FCPUSensitive: Boolean;

    // The found folder
    FFolder: String;
  Public
    Constructor Create; Virtual;

    Procedure Initialise; Virtual;

    Property Folder: String Read FFolder;

    Property Available: Boolean Read FAvailable;

    Property Name: String Read FName;
    Property Summary: String Read FSummary;
    Property CodeURL: String Read FCodeURL;
    Property ProjectURL: String Read FProjectURL;
    Property Readme: String Read FReadMe;
    Property License: String Read FLicense;
  End;

  TThirdParties = Class(Specialize TFPGObjectList<TThirdParty>);

Function ThirdParties: TThirdParties;

Const
  THIRDPARTY_FOLDER: String = 'Apps';

Implementation

Uses
  FileSupport, VersionSupport;

Var
  FThirdParties: TThirdParties;

Function ThirdParties: TThirdParties;
Begin
  If Not Assigned(FThirdParties) Then
    FThirdParties := TThirdParties.Create(False);

  Result := FThirdParties;
End;

{ TThirdPartySupport }

Constructor TThirdParty.Create;
Begin
  ThirdParties.Add(Self);
End;

Procedure TThirdParty.Initialise;
Var
  sFile: String;
  sKeyFolder: String;

  Function CheckFiles(AFolder: String; AFiles: Array Of String): String;
  Var
    i: Integer;
  Begin
    Result := '';

    For i := Low(AFiles) To High(AFiles) Do
      If FileExists(AFolder + AFiles[i]) Then
      Begin
        Result := AFolder + AFiles[i];
        Exit;
      End;
  End;

Begin
  If (FFolder = '') And (FKeyFile <> '') Then
  Begin
    sKeyFolder := FKeyFolder;

    If FCPUSensitive Then
      sKeyFolder := IncludeSlash(sKeyFolder) + GetCPU;

    sFile := FindSupportFileInFolders(THIRDPARTY_FOLDER, sKeyFolder, FKeyFile);

    If sFile <> '' Then
      FFolder := IncludeSlash(ExtractFileDir(sFile));
  End;

  FAvailable := (FFolder <> '') And DirectoryExists(FFolder);

  If FAvailable Then
  Begin
    // Lets have a good rummage around for a readme and license file
    FReadMe := CheckFiles(FFolder, ['readme.md', 'readme.txt', 'readme']);
    If not FileExists(FReadMe) Then
      FReadMe := CheckFiles(FFolder+'..\', ['readme.md', 'readme.txt', 'readme']);

    FLicense := CheckFiles(FFolder, ['license.txt', 'license.md', 'license']);
    If not FileExists(FLicense) Then
      FLicense := CheckFiles(FFolder+'..\', ['license.txt', 'license.md', 'license']);
  End;
End;

Initialization
  FThirdParties := nil;

Finalization;
  FreeAndNil(FThirdParties);

End.
