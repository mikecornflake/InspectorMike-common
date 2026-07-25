Unit ThirdPartySupport;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, fgl, FileUtil;

Type
  TThirdPartyKind = (tpkCommandLineTool, tpkDatabaseDriver, tpkRuntimeLibrary,
    tpkLazarusPackage, tpkSourceLibrary, tpkAssetCollection, tpkCollaborator);

  TThirdPartyDefinition = Record
    Name: String;
    Summary: String;
    ProjectURL: String;
    CodeURL: String;
    Kind: TThirdPartyKind;
    KeyFile: String;
    KeyFolder: String;
    CPUSensitive: Boolean;
  End;

  { TThirdParty }

  TThirdParty = Class
  Private
    Procedure SetFolder(AValue: String);
  Protected
    FKind: TThirdPartyKind;
    FUsed: Boolean;
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

    Procedure Initialise; Virtual;
  Public
    Constructor Create; Virtual; Overload;
    Constructor Create(ADefinition: TThirdPartyDefinition); Overload;

    Procedure IncludeAttribution; // A do nothing procedure designed to ensure attribution in about box

    Function FullExe(AExeNoExt: String): String;

    Property Used: Boolean Read FUsed Write FUsed;

    Property Folder: String Read FFolder Write SetFolder;
    Property Available: Boolean Read FAvailable;

    Property Kind: TThirdPartyKind Read FKind;

    Property Name: String Read FName;
    Property Summary: String Read FSummary;
    Property CodeURL: String Read FCodeURL;
    Property ProjectURL: String Read FProjectURL;
    Property Readme: String Read FReadMe;
    Property License: String Read FLicense;
  End;

  { TThirdParties }

  TThirdParties = Class(Specialize TFPGObjectList<TThirdParty>)
  Public
    Procedure Include(Const ANames: Array Of String);
  End;

Function ThirdParties: TThirdParties;

Const
  THIRDPARTY_FOLDER = 'Apps';

  THIRDPARTY_LAZARUS = 'Lazarus';
  THIRDPARTY_FPC = 'Free Pascal';
  THIRDPARTY_LAZARUSFORUM = 'Lazarus Forum';

  THIRDPARTY_IMAGEMAGICK = 'ImageMagick';
  THIRDPARTY_FATCOW_ICONS = 'FatCow Icons';

  THIRDPARTY_AI_CHATGPT = 'ChatGPT';

Const
  ThirdPartyDefinitions: Array[0..5] Of TThirdPartyDefinition = (
    (
    Name: THIRDPARTY_LAZARUSFORUM;
    Summary: 'Words alone cannot express my gratitude to the open source community for developing a wide range of versatile tools, and for making these easily available to other developers such as myself. ' + LineEnding + LineEnding + 'In particular I''d like to thank all the helpful individuals on the Lazarus forums.  These people give up their free time willingly, providing help and support.';
    ProjectURL: 'https://forum.lazarus.freepascal.org/index.php';
    CodeURL: 'https://wiki.lazarus.freepascal.org/';
    Kind: tpkCollaborator;
    KeyFile: '';
    KeyFolder: '';
    CPUSensitive: False
    ), (
    Name: THIRDPARTY_LAZARUS;
    Summary: 'Lazarus is a Delphi compatible cross-platform IDE for Rapid Application Development. It has variety of components ready for use and a graphical form designer to easily create complex graphical user interfaces.';
    ProjectURL: 'https://www.lazarus-ide.org/';
    CodeURL: 'https://gitlab.com/freepascal.org/lazarus';
    Kind: tpkLazarusPackage;
    KeyFile: '';
    KeyFolder: '';
    CPUSensitive: False
    ), (
    Name: THIRDPARTY_FPC;
    Summary: 'Free Pascal is a mature, versatile, open source Pascal compiler. It can target many processor architectures.';
    ProjectURL: 'https://www.freepascal.org/';
    CodeURL: 'https://gitlab.com/freepascal.org/';
    Kind: tpkSourceLibrary;
    KeyFile: '';
    KeyFolder: '';
    CPUSensitive: False
    ), (
    Name: THIRDPARTY_IMAGEMAGICK;
    Summary: 'ImageMagick® is a free, open-source software suite, used for editing and manipulating digital images';
    ProjectURL: 'https://imagemagick.org';
    CodeURL: 'https://github.com/imagemagick/imagemagick';
    Kind: tpkCommandLineTool;
    KeyFile: 'magick.exe'; // TODO Linux
    KeyFolder: 'ImageMagick';
    CPUSensitive: False
    ), (
    Name: THIRDPARTY_FATCOW_ICONS;
    Summary: 'Free Icon set: commercial usage allowed under Creative Commons license 3.0';
    ProjectURL: 'http://www.softicons.com/toolbar-icons/fatcow-hosting-icons-by-fatcow';
    CodeURL: 'https://creativecommons.org/licenses/by/3.0/us/';
    Kind: tpkAssetCollection;
    KeyFile: '';
    KeyFolder: '';
    CPUSensitive: False
    ), (
    Name: THIRDPARTY_AI_CHATGPT;
    Summary:
    'Since early 2025, I have been using ChatGPT as a coding assistant. ' +
    'It is good to feel part of a coding team again. I review, understand ' +
    'and test all suggested code before including it.  I accept full ' +
    'responsibility for the result, only publishing code I am willing ' + 'to maintain.';
    ProjectURL: 'https://chatgpt.com';
    CodeURL: 'https://openai.com';
    Kind: tpkCollaborator;
    KeyFile: '';
    KeyFolder: '';
    CPUSensitive: False
    )
    );

  // TODO: Handle BGRABITMAP, ZEOS, WGS84, URUWORKS, TURBOPOWER, LAZSERIAL

Implementation

Uses
  FileSupport, VersionSupport;

Var
  FThirdParties: TThirdParties;

Function ThirdParties: TThirdParties;
Var
  oDefinition: TThirdPartyDefinition;
Begin
  If Not Assigned(FThirdParties) Then
  Begin
    FThirdParties := TThirdParties.Create(True);

    // During Creation, TThirdParty registers itself with FThirdParties
    For oDefinition In ThirdPartyDefinitions Do
      TThirdParty.Create(oDefinition);
  End;

  Result := FThirdParties;
End;

{ TThirdParties }

Procedure TThirdParties.Include(Const ANames: Array Of String);
Var
  sName: String;
  oThirdParty: TThirdParty;
Begin
  Begin
    For sName In ANames Do
    Begin
      For oThirdParty In Self Do
        If SameText(oThirdParty.Name, sName) Then
        Begin
          If Assigned(oThirdParty) Then
            oThirdParty.Used := True
          Else
            Raise Exception.CreateFmt('Unknown third-party dependency: %s', [sName]);
        End;
    End;
  End;
End;

{ TThirdParty }

Function TThirdParty.FullExe(AExeNoExt: String): String;
Var
  sFile: String;
Begin
  Result := '';

  If Not DirectoryExists(FFolder) Then
    Exit;

  sFile := IncludeSlash(FFolder) + AExeNoExt + GetExeExt;

  If FileExists(sFile) Then
    Result := sFile;
End;

Procedure TThirdParty.SetFolder(AValue: String);
Begin
  // Outside world is trying to set folder because it knows where the exe's are
  If FFolder = AValue Then
    Exit;

  FFolder := AValue;

  Initialise;
End;

Constructor TThirdParty.Create;
Begin

End;

Constructor TThirdParty.Create(ADefinition: TThirdPartyDefinition);
Begin
  FUsed := False;

  FName := ADefinition.Name;
  FSummary := ADefinition.Summary;
  FKind := ADefinition.Kind;

  FCodeURL := ADefinition.CodeURL;
  FProjectURL := ADefinition.ProjectURL;

  FKeyFile := ADefinition.KeyFile;
  FKeyFolder := ADefinition.KeyFolder;

  FCPUSensitive := ADefinition.CPUSensitive;

  Initialise;
  ThirdParties.Add(Self);
End;

Procedure TThirdParty.IncludeAttribution;
Begin

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
    If Not FileExists(FReadMe) Then
      FReadMe := CheckFiles(FFolder + '..\', ['readme.md', 'readme.txt', 'readme']);

    FLicense := CheckFiles(FFolder, ['license.txt', 'license.md', 'license']);
    If Not FileExists(FLicense) Then
      FLicense := CheckFiles(FFolder + '..\', ['license.txt', 'license.md', 'license']);
  End;
End;

Initialization
  FThirdParties := nil;

  // Ensure these are always attributed
  ThirdParties.Include([THIRDPARTY_LAZARUS, THIRDPARTY_FPC, THIRDPARTY_LAZARUSFORUM,
    THIRDPARTY_AI_CHATGPT]);

Finalization;
  FreeAndNil(FThirdParties);

End.
