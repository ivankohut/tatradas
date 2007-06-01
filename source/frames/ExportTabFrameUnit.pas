unit ExportTabFrameUnit;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  ComCtrls,
  IniFiles,

  
  TabFrameTemplateUnit,
  procmat,
  ExecFileUnit,
  SectionUnit,
  ExportSectionUnit;


type
  TExportTabFrame = class(TTabFrameTemplate)
    FunctionListView: TListView;
    procedure FunctionListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure GotoFunctionClick(Sender: TObject);
  private
    fSection: TExportSection;
  protected
    function GetSection: TSection; override;
  public
    GotoEnabled: boolean;
    constructor Create(AOwner: TComponent; ASection: TSection);
    procedure Translate(ini: TMemINIFile; error: string);
 
  end;


implementation

uses MainFormUnit, CodeTabFrameUnit;

{$R *.dfm}

{ TExportTabFrame }


constructor TExportTabFrame.Create(AOwner: TComponent; ASection: TSection);
var
  ListItem: TListItem;
  i: integer;
begin
  inherited;
  fSection:=ASection as TExportSection;
  Caption:='Export';
  GotoEnabled:=false;

  for i:=0 to fSection.FunctionCount-1 do begin
    ListItem:=FunctionListView.Items.Add;
    ListItem.Caption:=IntToStr(i+1)+'.';
    with fSection.functions[i] do begin
      ListItem.SubItems.Add(Name);
      ListItem.SubItems.Add(IntToStr(Section));
      ListItem.SubItems.Add(IntToHex(CodeSectionOffset, 8));
      ListItem.SubItems.Add(IntToHex(MemOffset, 8));
      ListItem.SubItems.Add(IntToHex(ordinal, 8));
    end;  
  end;
end;



function TExportTabFrame.GetSection: TSection;
begin
  result:=fSection;
end;



procedure TExportTabFrame.FunctionListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  GotoEnabled:=false;
  if Selected and (Section.execfile as TExecutableFile).IsDisassembled then
    if fSection.functions[FunctionListView.ItemIndex].name <> '! INVALID RVA !' then
      GotoEnabled:=true;
end;



procedure TExportTabFrame.GotoFunctionClick(Sender: TObject);
var
  FunctionIndex, SectionIndex: integer;
  Address: cardinal;
  Tab: TTabSheetTemplate;
begin
  if not GotoEnabled then Exit;

  FunctionIndex:= FunctionListView.ItemIndex;
  Address:= fSection.Functions[FunctionIndex].MemOffset;
  SectionIndex:= fSection.Functions[FunctionIndex].Section;

  Tab:=MainForm.GetSectionsTabSheet(MainForm.ExecFile.Sections[SectionIndex]);
  with (Tab.Frame as TCodeTabFrame) do begin
    GotoPosition(GetPosition(Address), soBeginning);
  end;
  MainForm.PageControl1.ActivePage:=Tab;
end;



procedure TExportTabFrame.Translate(ini: TMemINIFile; error: string);
begin
  Caption:=ini.ReadString('Export','Caption',error);
  FunctionListView.Columns.Items[0].Caption:=ini.ReadString('Export','FunctionListNumber',error);
  FunctionListView.Columns.Items[1].Caption:=ini.ReadString('Export','FunctionListName',error);
  FunctionListView.Columns.Items[2].Caption:=ini.ReadString('Export','FunctionListSection',error);
  FunctionListView.Columns.Items[3].Caption:=ini.ReadString('Export','FunctionListOffset',error);
  FunctionListView.Columns.Items[4].Caption:=ini.ReadString('Export','FunctionListAddress',error);
  FunctionListView.Columns.Items[5].Caption:=ini.ReadString('Export','FunctionListOrdinal',error);
end;



end.
