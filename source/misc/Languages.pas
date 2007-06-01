unit Languages;

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

  procmat,
  StringRes;

const
  LangFileSectionCount = 24;



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

  TTatradasLanguages = class
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
    function GetID: cardinal;
  public
    constructor Create(Folder: string; Menu: TMenuItem; ImageList: TImageList);
    function ChangeLanguage(LanguageShortCut: string): boolean; overload;
    function ChangeLanguage(LanguageID: cardinal): boolean; overload;
    function ChangeLanguage(LangItem: TLangItem): boolean; overload;
    procedure Translate;
    function TranslateControl(Category: string; Name: string): string;
    property Language:string read GetLanguage;
    property INI: TMemINIFile read fINI;
    property ShortCut:string read GetShortCut;
  end;




var
  Langs: TTatradasLanguages;

implementation

uses
  AboutBoxUnit, GotoAddressFormUnit, UnknownFileFormUnit, SaveOptionsFormUnit,
  AdvancedDisassembleFormUnit, AdvancedChangingToDataFormUnit, HexEditFormUnit,
  CalculatorUnit, OptionsFormUnit, CodeSectionUnit, InsertCommentFormUnit, MainFormUnit;




constructor TTatradasLanguages.Create(Folder: string; Menu: TMenuItem; ImageList: TImageList);
var
  LangItem: TLangItem;
  sr: TSearchRec;
  tini: TMemINIFile;
  Icon: TIcon;
begin
  fError:='error';
  fMenu:=Menu;
  fFolder:= Folder;
  fCount:=0;
  if FindFirst(Folder+'\*.ini',faAnyFile,sr)=0 then begin
    repeat
      tINI:=TMemINIFile.Create(Folder+'\'+sr.Name);
      if CheckLangFile(tINI) then begin
        inc(fCount);
        SetLength(LangArray,fCount);
        LangItem.FileName:=Folder+'\'+sr.Name;
        LangItem.Name:= tINI.ReadString('General','LanguageName',fError);
        LangItem.Hint:=tINI.ReadString('General','LanguageHint',fError);
        LangItem.ShortCut:=tINI.ReadString('General','LanguageShortCut',fError);
        LangItem.ID:=tINI.ReadInteger('General','LanguageID',0);
        LangItem.IconFileName:=Folder+'\'+tINI.ReadString('General','IconFile',fError);
        LangItem.Index:=fCount-1;
        LangItem.MenuItem:=TMenuItem.Create(fMenu);
        LangItem.MenuItem.Caption:=LangItem.Name;
        LangItem.MenuItem.Hint:=LangItem.Hint;
        LangItem.MenuItem.OnClick:=LangMenuItemClick;
        Icon:=TIcon.Create;
        try
          Icon.LoadFromFile(LangItem.IconFileName);
          LangItem.MenuItem.ImageIndex:=ImageList.AddIcon(Icon);
        except
          Icon.Free;
        end;
        fMenu.Add(LangItem.MenuItem);
        LangArray[fCount-1]:= LangItem;
      end;
      tINI.Free;
    until FindNext(sr)<>0;
    FindClose(sr);
  end;
end;



function TTatradasLanguages.GetShortCut: string;
begin
  result:=LangArray[Index].ShortCut;
end;

function TTatradasLanguages.GetLanguage: string;
begin
  result:=LangArray[Index].Name;
end;

function TTatradasLanguages.GetID: cardinal;
begin
  result:=LangArray[Index].ID;
end;

function TTatradasLanguages.CheckLangFile(FileName: string):boolean;
var tINI: TMemINIFile;
begin
  result:=false;
  try
    tINI:= TMemINIFile.Create(FileName);
  except
    tINI.Free;
    Exit;
  end;
  result:=CheckLangFile(tINI);
  tINI.Free;
end;

function TTatradasLanguages.CheckLangFile(testINI: TMemINIFile):boolean;
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

  if testINI.ReadString('General','LanguageName',fError) = fError then Exit;
  if testINI.ReadString('General','LanguageShortCut',fError) = fError then Exit;
  if testINI.ReadInteger('General','LanguageID',-1) = -1 then Exit;
  if testINI.ReadString('General','Version',fError) <> ShortTatraDASVersion then Exit;
  result:=true;
end;

function TTatradasLanguages.ChangeLanguage(LanguageShortCut: string): boolean;
var i: integer;
begin
  result:=false;
  for i:=0 to fCount-1 do
    if LangArray[i].ShortCut=LanguageShortCut then
      result:= ChangeLanguage(LangArray[i]);
end;

function TTatradasLanguages.ChangeLanguage(LanguageID: cardinal): boolean;
var i:integer;
begin
  result:=false;
  for i:=0 to fCount-1 do
   if LangArray[i].ID=LanguageID then
     result:=ChangeLanguage(LangArray[i]);
end;

function TTatradasLanguages.ChangeLanguage(LangItem: TLangItem): boolean;
var tINI: TMemINIFile;
begin
  result:=false;
  try
    tINI:=TMemINIFile.Create(LangItem.FileName);
  except
    tINI.Free;
    exit;
  end;
  fINI:=tINI;
  LangArray[Index].MenuItem.Checked:=false;
  LangItem.MenuItem.Checked:=true;
  Index:=LangItem.Index;
  result:=true;
end;

procedure TTatradasLanguages.LangMenuItemClick(Sender: TObject);
var i:integer;
begin
  for i:=0 to fCount - 1 do
    if LangArray[i].MenuItem = Sender then begin
      if ChangeLanguage(LangArray[i]) then Translate
      else ShowMessage('Unable to change language!');
    end;
end;

procedure TTatradasLanguages.Translate;
begin
  TranslateStrings(fINI,fError);
  MainForm.Translate(fINI,fError);
  AboutBox.Translate(fINI,fError);
  UnknownFileFormatForm.Translate(fINI,fError);
  SaveOptionsForm.Translate(fINI,fError);
  GotoAddressForm.Translate(fINI,fError);
  InsertCommentForm.Translate(fINI,fError);
  AdvancedDisassembleForm.Translate(fINI,fError);
  AdvancedChangingToDataForm.Translate(fINI,fError);
  HexEditForm.Translate(fINI,fError);
  Calculator.Translate(fINI,fError);
  OptionsForm.Translate(fINI,fError);
//  if MainForm.ExecFile<>nil then MainForm.ExecFile.Translate(fINI,fError);
end;

function TTatradasLanguages.TranslateControl(Category: string; Name: string):string;
begin
  result:=fINI.ReadString(Category,Name,fError);
end;



end.
