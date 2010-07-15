unit ImportTabFrameUnit;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  ComCtrls,
  IniFiles,

  procmat,
  SectionUnit,
  ExecFileUnit,
  ImportSectionUnit,
  TabFrameTemplateUnit,
  TranslatorUnit;

type
  TImportTabFrame = class(TTabFrameTemplate)
    OccurHintLabel: TLabel;
    Panel1: TPanel;
    FunctionListView: TListView;
    Panel2: TPanel;
    AddressListBox: TListBox;
    FunctionCallsLabel: TLabel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    ModulComboBox: TComboBox;
    ModulLabel: TLabel;
    FunctionLabel: TLabel;
    procedure ModulComboBoxChange(Sender: TObject);
    procedure FunctionListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure AddressListBoxDblClick(Sender: TObject);
    procedure FunctionListViewDblClick(Sender: TObject);
    procedure FunctionListViewColumnClick(Sender: TObject; Column: TListColumn);
  private
    fSection: TImportSection;
  protected
    function GetSection: TSection; override;
  public
    constructor Create(AOwner: TComponent; ASection: TSection); overload; override;
    procedure Translate; override;
  end;

var
  ImportTabFrame: TImportTabFrame;

implementation

uses
  MainFormUnit,
  CodeTabFrameUnit;

{$R *.dfm}


constructor TImportTabFrame.Create(AOwner: TComponent; ASection: TSection);
var
  i: Integer;
begin
  inherited;
  fSection := ASection as TImportSection;
  Caption := 'Import';
  for i := 0 to fSection.ModulCount - 1 do
    ModulComboBox.Items.Add(fSection.Moduls[i].name);
end;



procedure TImportTabFrame.ModulComboBoxChange(Sender: TObject);
var
  FunctionIndex: Integer;
  ListItem: TListItem;
begin
  FunctionListview.Clear;
  AddressListBox.Clear;

  with fSection.moduls[ModulCombobox.ItemIndex] do
    for FunctionIndex := 0 to integer(FunctionCount) - 1 do begin
      ListItem := FunctionListView.Items.Add;
      ListItem.Data := Pointer(FunctionIndex);
      ListItem.Caption := IntToStr(FunctionIndex + 1) + '.';
      ListItem.SubItems.Add(functions[FunctionIndex].Name);
      if functions[FunctionIndex].MemAddress <> 0 then
        ListItem.SubItems.Add(IntToHex(functions[FunctionIndex].MemAddress, 8))
      else
        ListItem.SubItems.Add('');

      if functions[FunctionIndex].ByOrdinal then begin
        ListItem.SubItems.Add(IntToHex(functions[FunctionIndex].Ordinal, 8));
        ListItem.SubItems.Add('');
      end
      else begin
        ListItem.SubItems.Add('');
        if functions[FunctionIndex].Hint <> 0 then
          ListItem.SubItems.Add(IntToHex(functions[FunctionIndex].Hint, 4))
        else
          ListItem.SubItems.Add('');
      end;
    end;
end;



procedure TImportTabFrame.FunctionListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  OccurIndex: Integer;
  Address: Cardinal;
  ModulIndex: Integer;
  FunctionIndex: Integer;
  pocetvyskytov: Integer;
begin
  if (not Selected) or ( not (fSection.ExecFile as TExecutableFile).IsDisassembled) then
    Exit;

  AddressListBox.Clear;
  ModulIndex := ModulComboBox.ItemIndex;
  FunctionIndex := Integer(FunctionListView.Selected.Data);
  PocetVyskytov := Length(fSection.moduls[ModulIndex].functions[FunctionIndex].Occurs);
  for OccurIndex := 0 to PocetVyskytov - 1 do begin
    Address := fSection.moduls[ModulIndex].functions[FunctionIndex].Occurs[OccurIndex].Address;
    AddressListbox.Items.Add(IntToHex(Address, 8));
  end;
end;



procedure TImportTabFrame.AddressListBoxDblClick(Sender: TObject);
var
  ModulIndex, FunctionIndex, OccurenceIndex, SectionIndex: Integer;
  Address: Cardinal;
  Tab: TTabSheetTemplate;
begin
  ModulIndex := ModulComboBox.ItemIndex;
  FunctionIndex := Integer(FunctionListView.Selected.Data);
  OccurenceIndex := AddressListBox.ItemIndex;

  Address := fSection.Moduls[ModulIndex].Functions[FunctionIndex].Occurs[OccurenceIndex].Address;
  SectionIndex := fSection.Moduls[ModulIndex].Functions[FunctionIndex].Occurs[OccurenceIndex].SectionIndex;

  Tab := MainForm.GetSectionsTabSheet(MainForm.ExecFile.Sections[SectionIndex]);
  with (Tab.Frame as TCodeTabFrame) do begin
    GotoPosition(GetPosition(Address), soBeginning);
  end;
  MainForm.MainPageControl.ActivePage := Tab;
end;



procedure TImportTabFrame.FunctionListViewDblClick(Sender: TObject);
var
  ModulIndex, FunctionIndex, OccurenceIndex, SectionIndex: Integer;
  Address: Cardinal;
  Tab: TTabSheetTemplate;
begin
  if (AddressListBox.Items.Count <> 1) then Exit;

  ModulIndex := ModulComboBox.ItemIndex;
  FunctionIndex := Integer(FunctionListView.Selected.Data);
  OccurenceIndex := 0;
  Address := fSection.Moduls[ModulIndex].Functions[FunctionIndex].Occurs[OccurenceIndex].Address;

  SectionIndex := fSection.Moduls[ModulIndex].Functions[FunctionIndex].Occurs[OccurenceIndex].SectionIndex;

  Tab := MainForm.GetSectionsTabSheet(MainForm.ExecFile.Sections[SectionIndex]);
  with (Tab.Frame as TCodeTabFrame) do begin
    GotoPosition(GetPosition(Address), soBeginning);
  end;
  MainForm.MainPageControl.ActivePage := Tab;
end;



procedure TImportTabFrame.Translate;
begin
  inherited;
  Caption := Translator.TranslateControl('Import', 'Caption');
  ModulLabel.Caption := Translator.TranslateControl('Import', 'ModulLabel');
  FunctionLabel.Caption := Translator.TranslateControl('Import', 'FunctionLabel');
  FunctionCallsLabel.Caption := Translator.TranslateControl('Import', 'FunctionCallsLabel');
  OccurHintLabel.Caption := Translator.TranslateControl('Import', 'OccurHintLabel');

  FunctionListView.Columns.Items[0].Caption := Translator.TranslateControl('Import', 'FunctionListNumber');
  FunctionListView.Columns.Items[1].Caption := Translator.TranslateControl('Import', 'FunctionListName');
  FunctionListView.Columns.Items[2].Caption := Translator.TranslateControl('Import', 'FunctionListAddress');
  FunctionListView.Columns.Items[3].Caption := Translator.TranslateControl('Import', 'FunctionListOrdinal');
  FunctionListView.Columns.Items[4].Caption := Translator.TranslateControl('Import', 'FunctionListHint');
end;



function TImportTabFrame.GetSection: TSection;
begin
  Result := fSection;
end;



function ImportTabFrameSortListView(Item1, Item2, SortColumn: Integer): Integer stdcall;
var
  ListItem1, ListItem2: TListItem;
  Num1, Num2: Integer;
  Str1, Str2: string;
begin
  ListItem1 := TListItem(item1);
  ListItem2 := TListItem(item2);

  if SortColumn = 0 then begin
    str1 := ListItem1.Caption;
    str2 := ListItem2.Caption;
  end
  else begin
    str1 := ListItem1.SubItems[SortColumn - 1];
    str2 := ListItem2.SubItems[SortColumn - 1];
  end;

  case SortColumn of
    0: begin
      num1 := StrToInt(Copy(str1, 1, Length(str1) - 1));
      num2 := StrToInt(Copy(str2, 1, Length(str2) - 1));
      if num1 > num2 then
        Result := +1
      else
        if num1 < num2 then
          Result := -1
        else
          Result := 0;
    end;

    1, 2, 3, 4: begin
      if str1 > str2 then
        Result := +1
      else
        if str1 < str2 then
          Result := -1
        else
          Result := 0;
    end;

    else
      Result := 0;
  end;
end;



procedure TImportTabFrame.FunctionListViewColumnClick(Sender: TObject; Column: TListColumn);
begin
  inherited;
  {$IFNDEF LCL}
  FunctionListView.Items.BeginUpdate;
  FunctionListView.CustomSort(ImportTabFrameSortListView, Column.Index);
  FunctionListView.Items.EndUpdate;
  {$ENDIF}
end;



end.
