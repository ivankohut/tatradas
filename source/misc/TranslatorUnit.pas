{ TODO:
}

unit TranslatorUnit;


interface

uses
  Forms, Graphics,
  INIFiles,
  SysUtils,
  Classes,
  Types,
  Math,
  Contnrs,

  procmat,
  VersionUnit,
  StringRes;

const
  LangFileSectionCount = 26;
  DefaultString = '- error -';
  LanguagesFolder = 'languages';

type
  TLanguageInfo = class
  public
    Name: string;
    Hint: string;
    Shortcut: string;
    Icon: TIcon;
    FileName: string;

    constructor Create(INI: TCustomIniFile);
    destructor Destroy; override;
  end;


  TTranslator = class
  private
    fLanguages: TObjectList; //TLanguageInfo;
    fActiveLanguageIndex: cardinal;
    fINI: TMemINIFile;
    function CheckLangFile(filename: string):boolean; overload;
    function CheckLangFile(testINI: TCustomINIFile):boolean; overload;
    function GetLanguageInfo(Index: Integer): TLanguageInfo;
    function GetActiveLanguageInfo: TLanguageInfo;
    procedure Translate;
    function GetLanguagesCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function ChangeLanguage(Index: Integer): boolean; overload;
    function ChangeLanguage(LanguageShortCut: string): boolean; overload;
    function TranslateControl(Category: string; Name: string): string; overload;
    property AvailableLanguages[Index: Integer]: TLanguageInfo read GetLanguageInfo;
    property AvailableLanguagesCount: Integer read GetLanguagesCount;
    property ActiveLanguage: TLanguageInfo read GetActiveLanguageInfo;
  end;


var
  Translator: TTranslator;
  TatraDAS_Version: TVersion;

implementation


procedure TranslateStringResStrings; forward;


{ TLanguageInfo }


constructor TLanguageInfo.Create(INI: TCustomIniFile);
begin
  FileName := INI.FileName;
  Name := INI.ReadString('General', 'LanguageName', DefaultString);
  Hint := INI.ReadString('General', 'LanguageHint', DefaultString);
  ShortCut := INI.ReadString('General', 'LanguageShortCut', DefaultString);
  Icon := TIcon.Create;
  Icon.LoadFromFile(
    IncludeTrailingPathDelimiter(ExtractFilePath(FileName)) +
      INI.ReadString('General', 'IconFile', DefaultString)
  );
end;



destructor TLanguageInfo.Destroy;
begin
  Icon.Free;
end;


{ TTranslator }


constructor TTranslator.Create;
var
  sr: TSearchRec;
  tini: TCustomIniFile;
  Folder: string;
begin
  fLanguages := TObjectList.Create(true);
  Folder := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + IncludeTrailingPathDelimiter(LanguagesFolder);
  if FindFirst(Folder + '*.ini', faAnyFile, sr) = 0 then begin
    repeat
      tINI := TMemIniFile.Create(Folder + sr.Name);
      if CheckLangFile(tINI) then
        fLanguages.Add(TLanguageInfo.Create(tINI));

      tINI.Free;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;



destructor TTranslator.Destroy;
begin
  fINI.Free;
  fLanguages.Free;
end;




function TTranslator.GetActiveLanguageInfo: TLanguageInfo;
begin
  Result := GetLanguageInfo(fActiveLanguageIndex);
end;



function TTranslator.GetLanguageInfo(Index: Integer): TLanguageInfo;
begin
  Result := TLanguageInfo(fLanguages[Index]);
end;




function TTranslator.GetLanguagesCount: Integer;
begin
  Result := fLanguages.Count;
end;




function TTranslator.CheckLangFile(FileName: string): Boolean;
var
  tINI: TMemINIFile;
begin
  result:= false;
  try
    tINI:= TMemINIFile.Create(FileName);
  except
    Exit;
  end;

  try
    result := CheckLangFile(tINI);
  finally
    tINI.Free;
  end;
end;



function TTranslator.CheckLangFile(testINI: TCustomIniFile): Boolean;
var
  SectionList: TStrings;
begin
  Result := False;
  SectionList:= TStringList.Create;
  testINI.ReadSections(SectionList);
  if SectionList.Count <> LangFileSectionCount then begin
    SectionList.Free;
    Exit;
  end;
  SectionList.Free;

  if testINI.ReadString('General', 'LanguageName', DefaultString) = DefaultString then Exit;
  if testINI.ReadString('General', 'LanguageShortCut', DefaultString) = DefaultString then Exit;
  if testINI.ReadInteger('General', 'LanguageID',-1) = -1 then Exit;

  if   (TatraDAS_Version.Compare(testINI.ReadString('General', 'MinVersion', DefaultString)) = GreaterThanValue)
     OR
       (TatraDAS_Version.Compare(testINI.ReadString('General', 'MaxVersion', DefaultString)) = LessThanValue)
  then
    Exit;

  result:= true;
end;



function TTranslator.ChangeLanguage(LanguageShortCut: string): boolean;
var
  i: integer;
begin
  result := false; //UseDefault;
  for i := 0 to fLanguages.Count - 1 do
    if (fLanguages[i] as TLanguageInfo).ShortCut = LanguageShortCut then begin
      // works only during startup, in the future the default language shlould be built in TatraDAS.exe
      result := ChangeLanguage(i);
      Break;
    end;
end;



function TTranslator.ChangeLanguage(Index: Integer): boolean;
var
  INI: TMemINIFile;
begin
  result := false;
  try
    INI := TMemINIFile.Create(GetLanguageInfo(Index).FileName);
  except
    Exit;
  end;
  fINI.Free;
  fINI := INI;
  Translate;
  fActiveLanguageIndex := Index;
  result := true;
end;



procedure TTranslator.Translate;

  procedure TranslateComponent(AComponent: TComponent);
  var
    i: Integer;
  begin
    if Supports(AComponent, ITranslatable) then
      (AComponent as ITranslatable).Translate;
    for i := 0 to AComponent.ComponentCount - 1 do
      TranslateComponent(AComponent.Components[i]);
  end;

begin
  TranslateStringResStrings;
  TranslateComponent(Application);
end;



function TTranslator.TranslateControl(Category: string; Name: string): string;
begin
  result := fINI.ReadString(Category, Name, DefaultString);
end;


{ Support routines }


procedure TranslateStringResStrings;
begin
  FileNotFoundStr := Translator.TranslateControl('Texty', 'FileNotFound');
  CouldNotOpenFileStr := Translator.TranslateControl('Texty', 'CouldNotOpenFile');
  CouldNotOpenProjectStr := Translator.TranslateControl('Texty', 'CouldNotOpenProject');
  CouldNotOpenReadWriteFileStr := Translator.TranslateControl('Texty', 'CouldNotOpenReadWrite');
  ProjectFilterStr := Translator.TranslateControl('Texty', 'ProjectFilter');
  SaveDisassemblyFilterStr := Translator.TranslateControl('Texty', 'SaveDisassemblyFilter');
  FileModifiedStr := Translator.TranslateControl('Texty', 'FileModified');
  ProjectModifiedStr := Translator.TranslateControl('Texty', 'ProjectModified');
  FileCorruptedStr := Translator.TranslateControl('Texty', 'FileCorrupted');

  InvalidAddressStr := Translator.TranslateControl('Texty', 'InvalidAddress');
  InvalidStartOffsetStr := Translator.TranslateControl('Texty', 'InvalidStartOffset');
  InvalidSizeStr := Translator.TranslateControl('Texty', 'InvalidSize');
  InvalidEntrypointStr := Translator.TranslateControl('Texty', 'InvalidEntryPoint');
  FileOffsetStr := Translator.TranslateControl('Texty', 'FileOffset');
  SectionOffsetStr := Translator.TranslateControl('Texty', 'SectionOffset');

  CodeSectionStr := Translator.TranslateControl('Texty', 'CodeSection');
  SectionStr := Translator.TranslateControl('Texty', 'Section');
  UnusedSpaceStr := Translator.TranslateControl('Texty', 'UnusedSpace');

  InCompatibleProjectVersion := Translator.TranslateControl('Texty', 'InCompatibleProjectVersion');
  CurrentVersion := Translator.TranslateControl('Texty', 'CurrentVersion');
  UnableToChangeLanguageStr := Translator.TranslateControl('Texty', 'UnableToChangeLanguage');

  DivisionByZeroStr:= Translator.TranslateControl('Texty', 'DivisionByZero');
  NotFoundStr := Translator.TranslateControl('Texty', 'NotFound');
  ErrorStr := Translator.TranslateControl('Texty', 'Error');
  ExecAbortedStr := Translator.TranslateControl('Texty', 'ExecAborted');
  IllegalStateStr := Translator.TranslateControl('Texty', 'IllegalState');
end;


initialization
  TatraDAS_Version := TVersion.Create(ShortTatraDASVersion);
  Translator := TTranslator.Create;

finalization
  Translator.Free;
  TatraDAS_Version.Free;


end.
