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
  Languages;

type
  TImportTabFrame = class(TTabFrameTemplate)
    ModulComboBox: TComboBox;
    FunctionListView: TListView;
    ModulLabel: TLabel;
    FunctionLabel: TLabel;
    AddressListBox: TListBox;
    OccurHintLabel: TLabel;
    OccurLabel: TLabel;
    constructor Create(AOwner: TComponent; ASection: TSection); overload; override;
    procedure ModulComboBoxChange(Sender: TObject);
    procedure FunctionListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure AddressListBoxDblClick(Sender: TObject);
    procedure AddressListBoxMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FunctionListViewDblClick(Sender: TObject);
    procedure Translate(ini:TMemINIFile; error:string);
  private
    fSection: TImportSection;
  protected
    function GetSection: TSection; override;
  public
    GotoEnabled: boolean;
  end;

var
  ImportTabFrame: TImportTabFrame;

implementation

uses
  MainFormUnit,
  CodeTabFrameUnit;

{$R *.dfm}


constructor TImportTabFrame.Create(AOwner: TComponent; ASection: TSection);
var i: integer;
begin
  inherited;
  fSection:=ASection as TImportSection;
  Caption:='Import';
  GotoEnabled:=false;
  for i:=0 to fSection.ModulCount-1 do
    ModulComboBox.Items.Add(fSection.Moduls[i].name);
end;



procedure TImportTabFrame.ModulComboBoxChange(Sender: TObject);
var
  FunctionIndex: integer;
  ListItem: TListItem;
begin
  FunctionListview.Clear;
  AddressListBox.Clear;

  with fSection.moduls[ModulCombobox.ItemIndex] do 
   for FunctionIndex:=0 to integer(FunctionCount)-1 do begin
     ListItem:=FunctionListView.Items.Add;
     ListItem.Caption:=IntToStr(FunctionIndex + 1) + '.';
     ListItem.SubItems.Add(functions[FunctionIndex].name);
     ListItem.SubItems.Add(IntToHex(functions[FunctionIndex].addressRVA,8));
     ListItem.SubItems.Add(IntToHex(functions[FunctionIndex].ordinal,8));
     ListItem.SubItems.Add(IntToHex(functions[FunctionIndex].hint,4));
    end;
end;



procedure TImportTabFrame.FunctionListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  OccurIndex: integer;
  Address: cardinal;
  ModulIndex: integer;
  FunctionIndex: integer;
  pocetvyskytov: integer;
begin
  if (not Selected) or ( not (fSection.ExecFile as TExecutableFile).IsDisassembled) then exit;

  AddressListBox.Clear;
  ModulIndex:=ModulComboBox.ItemIndex;
  FunctionIndex:=FunctionListView.ItemIndex;
  PocetVyskytov:=Length(fSection.moduls[ModulIndex].functions[FunctionIndex].vyskyty);
  for OccurIndex:=0 to PocetVyskytov - 1 do begin
    Address:=fSection.moduls[ModulIndex].functions[FunctionIndex].vyskyty[OccurIndex];
    AddressListbox.Items.Add(IntToHex(Address, 8));
  end;
end;



procedure TImportTabFrame.AddressListBoxDblClick(Sender: TObject);
var
  ModulIndex, FunctionIndex, OccurenceIndex, SectionIndex: integer;
  Address: cardinal;
  Tab: TTabSheetTemplate;
begin
  if not GotoEnabled then Exit;

  ModulIndex:= ModulComboBox.ItemIndex;
  FunctionIndex:= FunctionListView.ItemIndex;
  OccurenceIndex:= AddressListBox.ItemIndex;
  Address:= fSection.Moduls[ModulIndex].Functions[FunctionIndex].vyskyty[OccurenceIndex];
  SectionIndex:= MainForm.ExecFile.Sections.GetSectionIndexFromMemOffset(Address);

  Tab:=MainForm.GetSectionsTabSheet(MainForm.ExecFile.Sections[SectionIndex]);
  with (Tab.Frame as TCodeTabFrame) do begin
    GotoPosition(GetPosition(Address), soBeginning);
  end;
  MainForm.PageControl1.ActivePage:=Tab;
end;



procedure TImportTabFrame.AddressListBoxMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if AddressListBox.ItemAtPos(point(x,y),true) = -1 then
    Exit
  else
    GotoEnabled:=true;
end;



procedure TImportTabFrame.FunctionListViewDblClick(Sender: TObject);
var
  ModulIndex, FunctionIndex, OccurenceIndex, SectionIndex: integer;
  Address: cardinal;
  Tab: TTabSheetTemplate;
begin
  if (AddressListBox.Items.Count <> 1) then Exit;
  if not GotoEnabled then Exit;

  ModulIndex:= ModulComboBox.ItemIndex;
  FunctionIndex:= FunctionListView.ItemIndex;
  OccurenceIndex:= AddressListBox.ItemIndex;
  Address:= fSection.Moduls[ModulIndex].Functions[FunctionIndex].vyskyty[OccurenceIndex];
  SectionIndex:= MainForm.ExecFile.Sections.GetSectionIndexFromMemOffset(Address);

  Tab:=MainForm.GetSectionsTabSheet(MainForm.ExecFile.Sections[SectionIndex]);
  with (Tab.Frame as TCodeTabFrame) do begin
    GotoPosition(GetPosition(Address), soBeginning);
  end;
  MainForm.PageControl1.ActivePage:=Tab;
end;



procedure TImportTabFrame.Translate(ini:TMemINIFile; error:string);
begin
  inherited;
  Caption:=ini.ReadString('Import','Caption',error);
  ModulLabel.Caption:=ini.ReadString('Import','ModulLabel',error);
  FunctionLabel.Caption:=ini.ReadString('Import','FunctionLabel',error);
  OccurLabel.Caption:=ini.ReadString('Import','OccurLabel',error);
  OccurHintLabel.Caption:=ini.ReadString('Import','OccurHintLabel',error);

  FunctionListView.Columns.Items[0].Caption:=ini.ReadString('Import','FunctionListNumber',error);
  FunctionListView.Columns.Items[1].Caption:=ini.ReadString('Import','FunctionListName',error);
  FunctionListView.Columns.Items[2].Caption:=ini.ReadString('Import','FunctionListAddress',error);
  FunctionListView.Columns.Items[3].Caption:=ini.ReadString('Import','FunctionListOrdinal',error);
  FunctionListView.Columns.Items[4].Caption:=ini.ReadString('Import','FunctionListHint',error);
end;



function TImportTabFrame.GetSection: TSection;
begin
  result:=fSection;
end;



end.
