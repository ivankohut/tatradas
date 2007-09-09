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
  TranslatorUnit,
  procmat,
  ExecFileUnit,
  SectionUnit,
  ExportSectionUnit;


type
  TExportTabFrame = class(TTabFrameTemplate)
    FunctionListView: TListView;
    procedure FunctionListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure GotoFunctionClick(Sender: TObject);
    procedure FunctionListViewColumnClick(Sender: TObject; Column: TListColumn);
  private
    fGotoEnabled: boolean;
    fSection: TExportSection;
  protected
    function GetSection: TSection; override;
  public
    constructor Create(AOwner: TComponent; ASection: TSection); override;
    procedure Translate; override;
  end;


implementation

uses
  MainFormUnit,
  CodeTabFrameUnit;

{$R *.dfm}


constructor TExportTabFrame.Create(AOwner: TComponent; ASection: TSection);
var
  ListItem: TListItem;
  i: integer;
begin
  inherited;
  fSection:=ASection as TExportSection;
  Caption:='Export';
  fGotoEnabled:=false;

  for i:=0 to fSection.FunctionCount-1 do begin
    ListItem:=FunctionListView.Items.Add;
    ListItem.Data:= Pointer(i);
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
  fGotoEnabled:=false;
  if Selected and (Section.ExeCfile as TExecutableFile).IsDisassembled then
    if (fSection.Functions[Integer(FunctionListView.Selected.Data)].Name <> '! INVALID RVA !') and (fSection.Functions[Integer(FunctionListView.Selected.Data)].Section >= 0) then
      fGotoEnabled:=true;
end;



procedure TExportTabFrame.GotoFunctionClick(Sender: TObject);
var
  FunctionIndex, SectionIndex: integer;
  Address: cardinal;
  Tab: TTabSheetTemplate;
begin
  if not fGotoEnabled then Exit;

  FunctionIndex:= Integer(FunctionListView.Selected.Data);
  Address:= fSection.Functions[FunctionIndex].MemOffset;
  SectionIndex:= fSection.Functions[FunctionIndex].Section;

  Tab:=MainForm.GetSectionsTabSheet(MainForm.ExecFile.Sections[SectionIndex]);
  with (Tab.Frame as TCodeTabFrame) do begin
    GotoPosition(GetPosition(Address), soBeginning);
  end;
  MainForm.PageControl1.ActivePage:=Tab;
end;



procedure TExportTabFrame.Translate;
begin
  Caption:= Translator.INI.ReadString('Export','Caption', TranslateErrorStr);
  FunctionListView.Columns.Items[0].Caption:= Translator.ini.ReadString('Export','FunctionListNumber',TranslateErrorStr);
  FunctionListView.Columns.Items[1].Caption:= Translator.ini.ReadString('Export','FunctionListName',TranslateErrorStr);
  FunctionListView.Columns.Items[2].Caption:= Translator.ini.ReadString('Export','FunctionListSection',TranslateErrorStr);
  FunctionListView.Columns.Items[3].Caption:= Translator.ini.ReadString('Export','FunctionListOffset',TranslateErrorStr);
  FunctionListView.Columns.Items[4].Caption:= Translator.ini.ReadString('Export','FunctionListAddress',TranslateErrorStr);
  FunctionListView.Columns.Items[5].Caption:= Translator.ini.ReadString('Export','FunctionListOrdinal',TranslateErrorStr);
end;



function ExportTabFrameSortListView(Item1, Item2, SortColumn: integer): integer stdcall;
var
  ListItem1, ListItem2: TListItem;
  Num1, Num2: integer;
  Str1, Str2: string;
begin
  ListItem1:= TListItem(item1);
  ListItem2:= TListItem(item2);

  if SortColumn = 0 then begin
    str1:= ListItem1.Caption;
    str2:= ListItem2.Caption;
  end
  else begin
    str1:= ListItem1.SubItems[SortColumn-1];
    str2:= ListItem2.SubItems[SortColumn-1];
  end;

  case SortColumn of
    0: begin
      num1:= StrToInt(Copy(str1, 1, Length(str1)-1));
      num2:= StrToInt(Copy(str2, 1, Length(str2)-1));
      if num1 > num2 then
        result:= +1
      else
        if num1 < num2 then
          result:= -1
        else
          result:=0;
    end;

    2: begin
      Num1:= StrToInt(Str1);
      Num2:= StrToInt(Str2);
      if num1 > num2 then
        result:= +1
      else
        if num1 < num2 then
          result:= -1
        else
          result:=0;
    end;

    1,3,4,5: begin
      if str1 > str2 then
        result:= +1
      else
        if str1 < str2 then
          result:=-1
        else
          result:= 0;
    end;

    else
      result:= 0;
  end;
end;



procedure TExportTabFrame.FunctionListViewColumnClick(Sender: TObject; Column: TListColumn);
begin
  inherited;
  FunctionListView.Items.BeginUpdate;
  FunctionListView.CustomSort(ExportTabFrameSortListView, Column.Index);
  FunctionListView.Items.EndUpdate;
end;



end.
