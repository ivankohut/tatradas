{ TODO:
  - zjednotit sposob prekladania Formov a Framov
}

unit TranslatorUnit;


interface

uses

{$IFDEF MSWINDOWS}
  Menus, Controls, Dialogs, Graphics,
{$ENDIF}
{$IFDEF LINUX}
  QMenus, QControls, QDialogs, QGraphics, QImgList,
{$ENDIF}
  INIFiles,
  SysUtils,
  Classes,
  Types,
  Math,
  Forms,

  procmat,
  VersionUnit,
  StringRes;

const
  LangFileSectionCount = 25;

type

  TLangItem = record
    Index: cardinal;
    FileName: string;
    MenuItem: TMenuItem;
    Name: string;
    Hint: string;
    Shortcut: string;
    ID: cardinal;
    IconFileName: string;
  end;

  TTranslator = class
    LangArray: array of TLangItem;
    Index: cardinal;
    fFolder: string;
    fCount: integer;
    fMenu: TMenuItem;
    fINI: TMemINIFile;
    fError: string;
  private
    procedure LangMenuItemClick(Sender: TObject);
    function CheckLangFile(filename: string):boolean; overload;
    function CheckLangFile(testINI: TMemINIFile):boolean; overload;
    function GetShortCut: string;
    function GetLanguage: string;
//    function GetID: cardinal;
  public
    constructor Create(AFolder: string; AMenu: TMenuItem; AImageList: TImageList);
    function ChangeLanguage(LanguageShortCut: string): boolean; overload;
    function ChangeLanguage(LanguageID: cardinal): boolean; overload;
    function ChangeLanguage(LangItem: TLangItem): boolean; overload;
    procedure Translate;
    function TranslateControl(Category: string; Name: string): string; overload;
//    function TranslateControl(aControl: TControl; Category: string; Name: string): boolean; overload;
//    function TranslateCaption(var aCaption: TCaption; Category: string; Name: string): boolean;

    property Language: string read GetLanguage;
    property INI: TMemINIFile read fINI;
    property ShortCut: string read GetShortCut;
  end;


var
  Translator: TTranslator;

implementation

uses
  MainFormUnit, TabFrameTemplateUnit;

procedure TranslateStringResStrings; forward;



function AddPathDelimiter(APath: TFileName): TFileName;
var
  PathLength: integer;
begin
  PathLength:= Length(APath);
  if PathLength = 0 then
    result:= PathDelim
  else if APath[PathLength] <> PathDelim then
    result:= APath + PathDelim
  else
    result:= APath;
end;



constructor TTranslator.Create(AFolder: string; AMenu: TMenuItem; AImageList: TImageList);
var
  LangItem: TLangItem;
  sr: TSearchRec;
  tini: TMemINIFile;
  Icon: TIcon;
begin
  fError:= 'error';
  fMenu:= AMenu;
  fFolder:= AddPathDelimiter(AFolder);
  fCount:= 0;
  if FindFirst(fFolder + '*.ini', faAnyFile, sr) = 0 then begin
    repeat
      tINI:= TMemINIFile.Create(fFolder + sr.Name);
      if CheckLangFile(tINI) then begin
        Inc(fCount);
        SetLength(LangArray, fCount);
        LangItem.FileName := fFolder + sr.Name;
        LangItem.Name:= tINI.ReadString('General', 'LanguageName', fError);
        LangItem.Hint:= tINI.ReadString('General', 'LanguageHint', fError);
        LangItem.ShortCut:=tINI.ReadString('General', 'LanguageShortCut', fError);
        LangItem.ID:= tINI.ReadInteger('General', 'LanguageID', 0);
        LangItem.IconFileName:= fFolder + tINI.ReadString('General', 'IconFile', fError);
        LangItem.Index:= fCount - 1;
        LangItem.MenuItem:= TMenuItem.Create(fMenu);
        LangItem.MenuItem.Caption:= LangItem.Name;
        LangItem.MenuItem.Hint:= LangItem.Hint;
        LangItem.MenuItem.OnClick:= LangMenuItemClick;
        Icon:= TIcon.Create;
        try
          Icon.LoadFromFile(LangItem.IconFileName);
          LangItem.MenuItem.ImageIndex:= AImageList.AddIcon(Icon);
        except
          Icon.Free;
        end;
        fMenu.Add(LangItem.MenuItem);
        LangArray[fCount-1]:= LangItem;
      end;
      tINI.Free;
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;
end;



function TTranslator.GetShortCut: string;
begin
  result:=LangArray[Index].ShortCut;
end;



function TTranslator.GetLanguage: string;
begin
  result:=LangArray[Index].Name;
end;


{
function TTranslator.GetID: cardinal;
begin
  result:=LangArray[Index].ID;
end;
}


function TTranslator.CheckLangFile(FileName: string): boolean;
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



function TTranslator.CheckLangFile(testINI: TMemINIFile): boolean;
var SectionList: TStrings;
begin
  result:=false;
  SectionList:= TStringList.Create;
  testINI.ReadSections(SectionList);
  if SectionList.Count <> LangFileSectionCount then begin
    SectionList.Free;
    Exit;
  end;
  SectionList.Free;

  if testINI.ReadString('General', 'LanguageName',fError) = fError then Exit;
  if testINI.ReadString('General', 'LanguageShortCut',fError) = fError then Exit;
  if testINI.ReadInteger('General', 'LanguageID',-1) = -1 then Exit;

  if   (TatraDAS_Version.Compare(testINI.ReadString('General', 'MinVersion', fError)) = GreaterThanValue)
     OR
       (TatraDAS_Version.Compare(testINI.ReadString('General', 'MaxVersion', fError)) = LessThanValue)
  then
    Exit;

  result:= true;
end;



function TTranslator.ChangeLanguage(LanguageShortCut: string): boolean;
var
  i: integer;
begin
  result:=false;
  for i:=0 to fCount-1 do
    if LangArray[i].ShortCut=LanguageShortCut then
      result:= ChangeLanguage(LangArray[i]);
end;



function TTranslator.ChangeLanguage(LanguageID: cardinal): boolean;
var
  i: integer;
begin
  result:=false;
  for i:=0 to fCount-1 do
   if LangArray[i].ID=LanguageID then
     result:=ChangeLanguage(LangArray[i]);
end;



function TTranslator.ChangeLanguage(LangItem: TLangItem): boolean;
var
  tINI: TMemINIFile;
begin
  result:= false;
  try
    tINI:=TMemINIFile.Create(LangItem.FileName);
  except
    Exit;
  end;
  fINI:=tINI;
  LangArray[Index].MenuItem.Checked:= false;
  LangItem.MenuItem.Checked:= true;
  Index:= LangItem.Index;
  result:= true;
end;



procedure TTranslator.LangMenuItemClick(Sender: TObject);
var
  i: integer;
begin
  for i:=0 to fCount - 1 do
    if LangArray[i].MenuItem = Sender then begin
      if ChangeLanguage(LangArray[i]) then
        Translate
      else
        ShowMessage('Unable to change language!');
    end;
end;



procedure TTranslator.Translate;
var
  i: integer;
begin
  // Translate StringRes strings
  TranslateStringResStrings;

  // Translate all forms
  for i:=0 to Application.ComponentCount - 1 do begin
    if Supports(Application.Components[i], ITranslatable) then
      (Application.Components[i] as ITranslatable).Translate;
  end;

  // Translate all frames
  for i:=0 to MainForm.MainPageControl.PageCount - 1 do
    (MainForm.MainPageControl.Pages[i] as TTabSheetTemplate).Frame.Translate;

end;



function TTranslator.TranslateControl(Category: string; Name: string): string;
begin
  result:= fINI.ReadString(Category, Name, fError);
end;


{
function TTranslator.TranslateControl(aControl: TControl; Category: string; Name: string): boolean;
begin
end;



function TTranslator.TranslateCaption(var aCaption: TCaption; Category: string; Name: string): boolean;
begin
  aCaption:= fINI.ReadString(Category, Name, fError);
end;
}


procedure TranslateStringResStrings;
begin

  FileNotFoundStr:= Translator.TranslateControl('Texty','FileNotFound');
  CouldNotOpenFileStr:= Translator.TranslateControl('Texty','CouldNotOpenFile');
  CouldNotOpenProjectStr:= Translator.TranslateControl('Texty','CouldNotOpenProject');
  CouldNotOpenReadWriteFileStr:= Translator.TranslateControl('Texty','CouldNotOpenReadWrite');
  ProjectFilterStr:= Translator.TranslateControl('Texty','ProjectFilter');
  SaveDisassemblyFilterStr:= Translator.TranslateControl('Texty','SaveDisassemblyFilter');
  FileModifiedStr:= Translator.TranslateControl('Texty', 'FileModified');
  ProjectModifiedStr:= Translator.TranslateControl('Texty', 'ProjectModified');

  InvalidAddressStr:= Translator.TranslateControl('Texty','InvalidAddress');
  InvalidStartOffsetStr:= Translator.TranslateControl('Texty','InvalidStartOffset');
  InvalidSizeStr:= Translator.TranslateControl('Texty','InvalidSize');
  InvalidEntrypointStr:= Translator.TranslateControl('Texty','InvalidEntryPoint');
  FileOffsetStr:= Translator.TranslateControl('Texty','FileOffset');
  SectionOffsetStr:= Translator.TranslateControl('Texty','SectionOffset');

  CodeSectionStr:= Translator.TranslateControl('Texty','CodeSection');
  SectionStr:= Translator.TranslateControl('Texty','Section');

  InCompatibleProjectVersion:= Translator.TranslateControl('Texty','InCompatibleProjectVersion');
  CurrentVersion:= Translator.TranslateControl('Texty','CurrentVersion');

  DivisionByZeroStr:= Translator.TranslateControl('Texty','DivisionByZero');
  NotFoundStr:= Translator.TranslateControl('Texty','NotFound');
end;



end.
