unit Translatables;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  IniFiles,
  Controls,
  ComCtrls,
  Menus,
  ActnList,
  // project unit
  procmat;


const
  DefaultString = '- error -';

type

  { TSectionOfStrings }

  TSectionOfStrings = class
  private
    FIni: TIniFile;
    FSection: string;
  public
    constructor Create(Ini: TIniFile; Section: string);
    function Value(Name: string): string;
  end;

  { TSectionsOfStrings }

  TSectionsOfStrings = class
  private
    FIni: TIniFile;
  public
    constructor Create(Ini: TIniFile);
    function Section(Name: string): TSectionOfStrings;
  end;

  { TTranslatable }

  TTranslatable = interface
    ['{E293B4CE-B91A-42FE-884A-27F54EEAD8DF}']
    procedure Translate(Sections: TSectionsOfStrings);
  end;

  { TTranslatableWidget }

  TTranslatableWidget = class
  private
    FName: string;
  protected
    constructor Create(Name: string);
    procedure SetTranslation(Value: string); virtual; abstract;
  public
    procedure Translate(Section: TSectionOfStrings);
  end;

  TTranslatableWidgetArray = array of TTranslatableWidget;

  { TTranslatableString }

  TTranslatableString = class(TTranslatableWidget)
  private
    FStringPointer: ^string;
  protected
    procedure SetTranslation(Value: string); override;
  public
    constructor Create(var TheString: string; Name: string);
  end;

  { TTranslatableControl }

  TTranslatableControl = class(TTranslatableWidget)
  protected
    FControl: TControl;
    constructor Create(Control: TControl; Name: string);
  end;

  { TTranslatableCaption }

  TTranslatableCaption = class(TTranslatableControl)
  protected
    procedure SetTranslation(Value: string); override;
  public
    constructor Create(Control: TControl); overload;
    constructor Create(Control: TControl; Name: string); overload;
  end;

  { TTranslatableCaptionWithSuffix }

  TTranslatableCaptionWithSuffix = class(TTranslatableCaption)
  private
    FSuffix: string;
  protected
    procedure SetTranslation(Value: string); override;
  public
    constructor Create(Control: TControl; Name: string; Suffix: string);
  end;

  { TTranslatableHint }

  TTranslatableHint = class(TTranslatableControl)
  protected
    procedure SetTranslation(Value: string); override;
  public
    constructor Create(Control: TControl); overload;
    constructor Create(Control: TControl; Name: string); overload;
  end;

  { TTranslatableAction }

  TTranslatableAction = class(TTranslatableWidget)
  protected
    FAction: TAction;
    procedure SetTranslation(Value: string); override;
  public
    constructor Create(Action: TAction; Name: string); overload;
  end;

  { TTranslatableMenuItem }

  TTranslatableMenuItem = class(TTranslatableWidget)
  protected
    FMenuItem: TMenuItem;
    procedure SetTranslation(Value: string); override;
  public
    constructor Create(MenuItem: TMenuItem; Name: string); overload;
  end;

  { TTranslatableMenuItemHint }

  TTranslatableMenuItemHint = class(TTranslatableMenuItem)
  protected
    procedure SetTranslation(Value: string); override;
  public
    constructor Create(MenuItem: TMenuItem; Name: string); overload;
  end;

  { TTranslatableListColumn }

  TTranslatableListColumn = class(TTranslatableWidget)
  private
    FColumn: TListColumn;
  protected
    procedure SetTranslation(Value: string); override;
  public
    constructor Create(Column: TListColumn; Name: string); overload;
  end;

  { TTranslatableStringsItem }

  TTranslatableStringsItem = class(TTranslatableWidget)
  private
    FStrings: TStrings;
    FIndex: Integer;
  protected
    procedure SetTranslation(Value: string); override;
  public
    constructor Create(Strings: TStrings; Index: Integer; Name: string);
  end;

  { TTranslatableGroup }

  TTranslatableGroup = class(TInterfacedObject, TTranslatable)
  private
    FName: string;
    FWidgets: TTranslatableWidgetArray;
  public
    constructor Create(Name: string; Widgets: array of TTranslatableWidget); overload;
    constructor Create(Name: string; Widgets: TTranslatableWidgetArray); overload;
    procedure Translate(Sections: TSectionsOfStrings); virtual;
  end;

  { TTranslatableListColumns }

  TTranslatableListColumns = class(TTranslatableGroup)
  public
    constructor Create(Name: string; Columns: TListColumns; ColumnNames: array of string);
  end;

  { TTranslatableProcessText }

  TTranslatableProcessText = class(TTranslatableGroup)
  public
    constructor Create(Name: string; var ProcessText: TProcessText);
  end;

  { TTranslatableBookmarks }

  TTranslatableBookmarks = class(TTranslatableGroup)
  private
    FBookmarks: TMenuItem;
    FHintPrefix: string;
  public
    constructor Create(Name: string; Bookmarks: TMenuItem; HintPrefix: string);
    procedure Translate(Sections: TSectionsOfStrings); override;
  end;

  { TTranslatableStrings }

  TTranslatableStrings = class(TTranslatableGroup)
  public
    constructor Create(Name: string; Strings: TStrings; StringNames: array of string);
  end;

  { TTranslatables }

  TTranslatables = class(TInterfacedObject, TTranslatable)
  private
    FTranslatables: array of TTranslatable;
  public
    constructor Create(Translatables: array of TTranslatable);
    procedure Translate(Sections: TSectionsOfStrings);
  end;

  { TTranslatableSimpleForm }

  TTranslatableSimpleForm = class(TTranslatables)
  public
    constructor Create(Form: TControl; Name: string; Widgets: array of TTranslatableWidget; CommonControls: array of TControl);
  end;

  { TTranslatableGroups }

  TTranslatableGroups = class(TInterfacedObject, TTranslatable)
  private
    FGroups: array of TTranslatableGroup;
  public
    constructor Create(Groups: array of TTranslatableGroup);
    procedure Translate(Sections: TSectionsOfStrings);
  end;

  { ITranslatable }

  ITranslatable = interface
    ['{E293B4CE-B91A-42FE-884A-27F54EEAD8DE}']
    function Translatable: TTranslatable;
  end;


implementation


uses
  LoggerUnit;


{ TTranslatableSimpleForm }

constructor TTranslatableSimpleForm.Create(Form: TControl; Name: string;
  Widgets: array of TTranslatableWidget; CommonControls: array of TControl);
var
  CommonWidgets: TTranslatableWidgetArray;
  i: Integer;
begin
  SetLength(CommonWidgets, Length(CommonControls));
  for i := 0 to Length(CommonControls) - 1 do begin
    CommonWidgets[i] := TTranslatableCaption.Create(CommonControls[i]);
  end;
  inherited Create([
    TTranslatableGroup.Create(Name, [TTranslatableCaption.Create(Form, 'Caption')]),
    TTranslatableGroup.Create(Name, Widgets),
    TTranslatableGroup.Create('Common', CommonWidgets)
  ]);
end;

{ TTranslatables }

constructor TTranslatables.Create(Translatables: array of TTranslatable);
var
  i: Integer;
begin
  SetLength(FTranslatables, Length(Translatables));
  for i := 0 to Length(Translatables) - 1 do begin
    FTranslatables[i] := Translatables[i];
  end;
end;



procedure TTranslatables.Translate(Sections: TSectionsOfStrings);
var
  Translatable: TTranslatable;
begin
  for Translatable in FTranslatables do begin
    Translatable.Translate(Sections);
  end;
end;

{ TTranslatableStringsItem }

procedure TTranslatableStringsItem.SetTranslation(Value: string);
begin
  FStrings[FIndex] := Value;
end;



constructor TTranslatableStringsItem.Create(Strings: TStrings; Index: Integer; Name: string);
begin
  inherited Create(Name);
  FStrings := Strings;
  FIndex := Index;
end;

{ TTranslatableStrings }

constructor TTranslatableStrings.Create(Name: string; Strings: TStrings; StringNames: array of string);
var
   Widgets: TTranslatableWidgetArray;
   i: Integer;
begin
  SetLength(Widgets, Length(StringNames));
  for i := 0 to Length(StringNames) do begin
    Widgets[i] := TTranslatableStringsItem.Create(Strings, i, StringNames[i]);
  end;
  inherited Create(Name, Widgets);
end;


{ TTranslatableString }

procedure TTranslatableString.SetTranslation(Value: string);
begin
  FStringPointer^ := Value;
end;



constructor TTranslatableString.Create(var TheString: string; Name: string);
begin
  inherited Create(Name);
  FStringPointer := @TheString;
end;

{ TTranslatableCaptionWithSuffix }

procedure TTranslatableCaptionWithSuffix.SetTranslation(Value: string);
begin
  inherited SetTranslation(Value + FSuffix);
end;



constructor TTranslatableCaptionWithSuffix.Create(Control: TControl; Name: string; Suffix: string);
begin
  inherited Create(Control, Name);
  FSuffix := Suffix;
end;

{ TTranslatableBookmarks }

constructor TTranslatableBookmarks.Create(Name: string; Bookmarks: TMenuItem; HintPrefix: string);
begin
  FBookmarks := Bookmarks;
  FHintPrefix := HintPrefix;
  inherited Create(Name, [
    TTranslatableMenuItem.Create(Bookmarks, HintPrefix + 'Bookmark'),
    TTranslatableMenuItemHint.Create(Bookmarks, 'Main' + HintPrefix + 'BookmarkHint')
  ]);
end;



procedure TTranslatableBookmarks.Translate(Sections: TSectionsOfStrings);
var
  Section: TSectionOfStrings;
  BookMarkIndex: Integer;
  Caption, Hint, Suffix: string;
begin
  inherited;
  Section := Sections.Section(FName);
  Caption := Section.Value('Bookmark');
  Hint := Section.Value(FHintPrefix + 'BookmarkHint');
  for BookMarkIndex := 0 to 9 do begin
    Suffix := ' ' + IntToStr(BookMarkIndex);
    FBookmarks.Items[BookMarkIndex].Caption := Caption + Suffix;
    FBookmarks.Items[BookMarkIndex].Hint := Hint + Suffix;
  end;
end;

{ TTranslatableProcessText }

constructor TTranslatableProcessText.Create(Name: string; var ProcessText: TProcessText);
begin
  inherited Create(Name, [
    TTranslatableString.Create(ProcessText.Disassembling, 'Disassembling'),
    TTranslatableString.Create(ProcessText.PreparingOutput, 'PreparingOutput'),
    TTranslatableString.Create(ProcessText.LoadingDAS, 'LoadingDAS'),
    TTranslatableString.Create(ProcessText.LoadingDHF, 'LoadingDHF'),
    TTranslatableString.Create(ProcessText.SavingDAS, 'SavingDAS'),
    TTranslatableString.Create(ProcessText.SavingDHF, 'SavingDHF')
  ]);
end;

{ TTranslatableMenuItemHint }

procedure TTranslatableMenuItemHint.SetTranslation(Value: string);
begin
  FMenuItem.Hint := Value;
end;



constructor TTranslatableMenuItemHint.Create(MenuItem: TMenuItem; Name: string);
begin
  inherited;
end;

{ TTranslatableMenuItem }

procedure TTranslatableMenuItem.SetTranslation(Value: string);
begin
  FMenuItem.Caption := Value;
end;



constructor TTranslatableMenuItem.Create(MenuItem: TMenuItem; Name: string);
begin
  inherited Create(Name);
  FMenuItem := MenuItem;
end;

{ TTranslatableAction }

procedure TTranslatableAction.SetTranslation(Value: string);
begin
  FAction.Caption := Value;
end;



constructor TTranslatableAction.Create(Action: TAction; Name: string);
begin
  inherited Create(Name);
  FAction := Action;
end;

{ TTranslatableListColumns }

constructor TTranslatableListColumns.Create(Name: string; Columns: TListColumns; ColumnNames: array of string);
var
  i: Integer;
  Widgets: TTranslatableWidgetArray;
begin
  SetLength(Widgets, Length(ColumnNames));
  for i := 0 to Length(ColumnNames) - 1 do begin
    Widgets[i] := TTranslatableListColumn.Create(Columns[i], ColumnNames[i]);
  end;
  inherited Create(Name, Widgets);
end;

{ TTranslatableWidget }

constructor TTranslatableWidget.Create(Name: string);
begin
  FName := Name;
end;



procedure TTranslatableWidget.Translate(Section: TSectionOfStrings);
begin
  SetTranslation(Section.Value(FName));
end;

{ TTranslatableControl }

constructor TTranslatableControl.Create(Control: TControl; Name: string);
begin
  inherited Create(Name);
  FControl := Control;
end;


{ TTranslatableCaption }

constructor TTranslatableCaption.Create(Control: TControl); overload;
begin
  self.Create(Control, Control.Name);
end;



constructor TTranslatableCaption.Create(Control: TControl; Name: string); overload;
begin
  inherited;
end;



procedure TTranslatableCaption.SetTranslation(Value: string);
begin
  FControl.Caption := Value;
end;


{ TTranslatableHint }

constructor TTranslatableHint.Create(Control: TControl);
begin
  self.Create(Control, Control.Name + 'Hint');
end;



constructor TTranslatableHint.Create(Control: TControl; Name: string);
begin
  inherited;
end;



procedure TTranslatableHint.SetTranslation(Value: string);
begin
  FControl.Hint := Value;
end;

{ TTranslatableListColumn }

procedure TTranslatableListColumn.SetTranslation(Value: string);
begin
  FColumn.Caption := Value;
end;



constructor TTranslatableListColumn.Create(Column: TListColumn; Name: string);
begin
  inherited Create(Name);
  FColumn := Column;
end;


{ TTranslatableGroup }

constructor TTranslatableGroup.Create(Name: string; Widgets: array of TTranslatableWidget); overload;
var
  i: Integer;
  WidgetsArray: TTranslatableWidgetArray;
begin
  SetLength(WidgetsArray, Length(Widgets));
  for i := Low(Widgets) to High(Widgets) do begin
    WidgetsArray[i - Low(Widgets)] := Widgets[i];
  end;
  self.Create(Name, WidgetsArray);
end;



constructor TTranslatableGroup.Create(Name: string; Widgets: TTranslatableWidgetArray); overload;
begin
  FName := Name;
  FWidgets := Widgets;
end;



procedure TTranslatableGroup.Translate(Sections: TSectionsOfStrings);
var
  Section: TSectionOfStrings;
  Widget: TTranslatableWidget;
begin
  Section := Sections.Section(FName);
  for Widget in FWidgets do begin
    Widget.Translate(Section);
  end;
end;

{ TTranslatableGroups }

constructor TTranslatableGroups.Create(Groups: array of TTranslatableGroup);
var
  i: Integer;
begin
  SetLength(FGroups, Length(Groups));
  for i := 0 to Length(Groups) - 1 do begin
    FGroups[i] := Groups[i];
  end;
end;



procedure TTranslatableGroups.Translate(Sections: TSectionsOfStrings);
var
  Group: TTranslatableGroup;
begin
  for Group in FGroups do begin
    Group.Translate(Sections);
  end;
end;

{ TSectionsOfStrings }

constructor TSectionsOfStrings.Create(Ini: TIniFile);
begin
  FIni := Ini;
end;



function TSectionsOfStrings.Section(Name: string): TSectionOfStrings;
begin
  Result := TSectionOfStrings.Create(FIni, Name);
end;

{ TSectionOfStrings }

constructor TSectionOfStrings.Create(Ini: TIniFile; Section: string);
begin
  FIni := Ini;
  FSection := Section;
end;



function TSectionOfStrings.Value(Name: string): string;
begin
  Result := FIni.ReadString(FSection, Name, DefaultString);
  if Result = DefaultString then
    Logger.Warning('Translation for ' + FSection + '.' + Name + ' not found.');
end;



end.

