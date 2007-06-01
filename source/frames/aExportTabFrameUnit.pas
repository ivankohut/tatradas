unit aExportTabFrameUnit;

interface

  TExportTabSheet = class(TTabSheet)
  public
    Panel: TPanel;
    FunctionListView: TListView;
    Section: TExportSection;
    GotoEnabled: boolean;
    constructor Create(AOwner: TComponent; Section: TExportSection); 
    destructor Destroy; override;
    procedure FunctionListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure GotoFunctionClick(Sender: TObject);
    procedure Translate(ini: TMemINIFile; error: string);
  end;

implementation



//============================================================================================================
// TExportTabSheet class
//============================================================================================================

{$IFDEF GUI_B}
constructor TExportTabSheet.Create(AOwner: TComponent; Section: TExportSection);
var i:integer;
    ListItem: TListItem;
    ListColumn: TListColumn;
begin
  inherited Create(AOwner);
  parent:=(AOwner as TPageControl);
  self.Section:=Section;
  PageControl:=(AOwner as TPageControl);

  Panel:= TPanel.Create(self);
  Panel.Parent:=self;
  Panel.Left:=8;
  Panel.Top:=8;
  Panel.Width:=self.Width-16;
  Panel.Height:=self.Height-16;
  Panel.Anchors:=[akLeft,akRight,akTop,akBottom];
  Panel.BevelInner:=bvRaised;
  Panel.BevelOuter:=bvLowered;

  FunctionListView:= TListView.Create(Panel);
  FunctionListView.Parent:=Panel;
  FunctionListView.OnSelectItem:=FunctionListViewSelectItem;
  FunctionListView.OnDblClick:=GotoFunctionClick;
  FunctionListView.Top:=16;
  FunctionListView.Left:=16;
  FunctionListView.Height:=Panel.Height-32;
  FunctionListView.Width:=Panel.Width-32;
  FunctionListView.Anchors:=[akTop,akBottom,akLeft,akRight];
  FunctionListView.ViewStyle:=vsReport;
  FunctionListView.ReadOnly:=true;
  FunctionListView.RowSelect:=true;

  ListColumn:=FunctionListView.Columns.Add;
  ListColumn.Caption:='Poradie';
  ListColumn.MaxWidth:=50;
  ListColumn.MinWidth:=50;
  ListColumn.Width:=50;

  ListColumn:=FunctionListView.Columns.Add;
  ListColumn.Caption:='Name';
  ListColumn.MaxWidth:=200;
  ListColumn.MinWidth:=200;
  ListColumn.Width:=200;

  ListColumn:=FunctionListView.Columns.Add;
  ListColumn.Caption:='Section';
  ListColumn.MaxWidth:=50;
  ListColumn.MinWidth:=50;
  ListColumn.Width:=50;

  ListColumn:=FunctionListView.Columns.Add;
  ListColumn.Caption:='Section offset';
  ListColumn.MaxWidth:=90;
  ListColumn.MinWidth:=90;
  ListColumn.Width:=90;

  ListColumn:=FunctionListView.Columns.Add;
  ListColumn.Caption:='Memory Offset';
  ListColumn.MaxWidth:=90;
  ListColumn.MinWidth:=90;
  ListColumn.Width:=90;

  ListColumn:=FunctionListView.Columns.Add;
  ListColumn.Caption:='Ordinal';
  ListColumn.MaxWidth:=80;
  ListColumn.MinWidth:=80;
  ListColumn.Width:=80;

  for i:=0 to Section.Functioncount-1 do begin
    ListItem:=FunctionListView.Items.Add;
    ListItem.Caption:=IntToStr(i+1)+'.';
    ListItem.SubItems.Add(Section.functions[i].name);
    ListItem.SubItems.Add(IntToStr(Section.functions[i].Section));
    ListItem.SubItems.Add(IntToHex(Section.functions[i].CodeSectionOffset, 8));
    ListItem.SubItems.Add(IntToHex(Section.functions[i].MemOffset, 8));
    ListItem.SubItems.Add(IntToHex(Section.functions[i].ordinal, 8));
  end;
  GotoEnabled:=false;
end;

destructor TExportTabSheet.Destroy;
begin
  FunctionListView.Free;
  inherited;
end;

procedure TExportTabSheet.FunctionListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  GotoEnabled:=false;
  if Selected and (((Section.execfile as TExecutableFile).Status=tsDisassembled) or ((Section.execfile as TExecutableFile).Status=tsProject)) then
    if Section.functions[FunctionListView.ItemIndex].name <> '! INVALID RVA !' then
      GotoEnabled:=true;
end;

procedure TExportTabSheet.GotoFunctionClick(Sender: TObject);
var i:integer;
begin
  if not GotoEnabled then Exit;
  Section.GotoFunction(FunctionListView.itemindex);
{
  i:=StrToInt(FunctionListView.Items.Item[FunctionListView.itemindex].SubItems[1]);
  with Section.execfile as TExecutableFile do begin
    (Sections[i] as TCodeSection).tab.GotoPosition(
      (Sections[i] as TCodeSection).tab.GetPosition(
        StrToInt('$'+FunctionListView.Items.Item[FunctionListView.itemindex].SubItems[2]))-1,0);
  end;
  Section.ctrls.PageControl.ActivePage:=((Section.execfile as TExecutableFile).Sections[i] as TCodeSection).tab;
  Section.ctrls.PageControl.OnChange(nil);
}  
{
  for i:=0 to (Section.execfile as TExecutableFile).CodeSectionsCount-1 do
    If ((Section.execfile as TExecutableFile).Sections[i] as TCodeSection).InSection(StrToInt('$'+FunctionListView.Items.Item[FunctionListView.itemindex].SubItems[0])) then begin
      ((Section.execfile as TExecutableFile).Sections[i] as TCodeSection).tab.GotoPosition(
        ((Section.execfile as TExecutableFile).Sections[i] as TCodeSection).tab.GetPosition(
          StrToInt('$'+FunctionListView.Items.Item[FunctionListView.itemindex].SubItems[0]) - ((Section.execfile as TExecutableFile).Sections[i] as TCodeSection).LogOffset)-1,0);
      Section.ctrls.PageControl.ActivePage:=((Section.execfile as TExecutableFile).Sections[i] as TCodeSection).tab;
      Section.ctrls.PageControl.OnChange(nil);
      Exit;
    end;
}
end;

procedure TExportTabSheet.Translate(ini: TMemINIFile; error: string);
begin
  Caption:=ini.ReadString('Export','Caption',error);
  FunctionListView.Columns.Items[0].Caption:=ini.ReadString('Export','FunctionListNumber',error);
  FunctionListView.Columns.Items[1].Caption:=ini.ReadString('Export','FunctionListName',error);
  FunctionListView.Columns.Items[2].Caption:=ini.ReadString('Export','FunctionListSection',error);
  FunctionListView.Columns.Items[3].Caption:=ini.ReadString('Export','FunctionListOffset',error);
  FunctionListView.Columns.Items[4].Caption:=ini.ReadString('Export','FunctionListAddress',error);
  FunctionListView.Columns.Items[5].Caption:=ini.ReadString('Export','FunctionListOrdinal',error);
end;
{$ENDIF}

end.
