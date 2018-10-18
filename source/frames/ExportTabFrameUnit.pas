{
  - Po kliknuti na title riadok by sa mala prilusna bunka znizit (ako button pri kliknuti), aby to naozaj vyzeralo ako po kliku
  - Optimalizacia triedenia, mozno nejaky BeginUpdate (StringGrid nema a davat to na jednotlive riadky (TStrings) sa mi zda zbytocne
}

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
  Grids,
  // project units
  TabFrameTemplateUnit,
  Translatables,
  procmat,
  ExecFileUnit,
  SectionUnit,
  ExportSectionUnit;

type

  { TExportTabFrame }

  TExportTabFrame = class(TTabFrameTemplate)
    FunctionStringGrid: TStringGrid;
    procedure FunctionStringGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FunctionStringGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure FunctionStringGridDblClick(Sender: TObject);

  private
    fGotoEnabled: Boolean;
    fValidGotoClick: Boolean;
    fSortColumn: Integer;
    fSection: TExportSection;

    fSortArray: array of TStrings;
    function SortGetItem(ItemIndex: Integer): TObject;
    procedure SortSetItem(ItemIndex: Integer; Item: TObject);
    function SortCompare(Item1, Item2: Integer): Integer;

  protected
    function GetSection: TSection; override;
  public
    constructor Create(AOwner: TComponent; ASection: TSection); override;
    function Translatable: TTranslatable; override;
  end;


implementation


uses
  MainFormUnit,
  CodeTabFrameUnit,
  DateUtils, SortingUnit;

{$R *.dfm}


constructor TExportTabFrame.Create(AOwner: TComponent; ASection: TSection);
var
  FunctionIndex: Integer;
begin
  inherited;
  fSection := ASection as TExportSection;
  Caption := 'Export';
  fGotoEnabled := False;
  FunctionStringGrid.DoubleBuffered := True;

  // Naplnenie gridu
  FunctionStringGrid.RowCount := fSection.FunctionCount + 1;
  for FunctionIndex := 0 to fSection.FunctionCount - 1 do begin
    with fSection.functions[FunctionIndex] do begin
      FunctionStringGrid.Cells[0, FunctionIndex + 1] := IntToStr(FunctionIndex + 1) + '.';
      FunctionStringGrid.Cells[1, FunctionIndex + 1] := Name;
      FunctionStringGrid.Cells[2, FunctionIndex + 1] := IntToStr(Section);
      FunctionStringGrid.Cells[3, FunctionIndex + 1] := IntToHex(CodeSectionOffset, 8);
      FunctionStringGrid.Cells[4, FunctionIndex + 1] := IntToHex(MemOffset, 8);
      FunctionStringGrid.Cells[5, FunctionIndex + 1] := IntToHex(ordinal, 8);
      FunctionStringGrid.Objects[0, FunctionIndex + 1] := TObject(FunctionIndex);
    end;
  end;
end;



function TExportTabFrame.GetSection: TSection;
begin
  Result := fSection;
end;



procedure TExportTabFrame.FunctionStringGridDblClick(Sender: TObject);
var
  FunctionIndex, SectionIndex: Integer;
  Address: Cardinal;
  Tab: TTabSheetTemplate;
begin
  if not (fGotoEnabled and fValidGotoClick) then
    Exit;

  FunctionIndex := Integer(FunctionStringGrid.Objects[0, FunctionStringGrid.Row]);
  Address := fSection.Functions[FunctionIndex].MemOffset;
  SectionIndex := fSection.Functions[FunctionIndex].Section;

  Tab := MainForm.GetSectionsTabSheet(MainForm.ExecFile.Sections[SectionIndex]);
  with (Tab.Frame as TCodeTabFrame) do begin
    GotoPosition(GetPosition(Address), soBeginning);
  end;

  MainForm.MainPageControl.ActivePage := Tab;
  if Assigned(MainForm.MainPageControl.OnChange) then // TODO: nezda sa mi, ze treba volat OnChange, malo by sa to dat spravit nejak inak
    MainForm.MainPageControl.OnChange(nil);
end;



procedure TExportTabFrame.FunctionStringGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  RowIndex: Integer;
begin
  fValidGotoClick := (FunctionStringGrid.MouseCoord(X, Y).X > 0) and (FunctionStringGrid.MouseCoord(X, Y).Y > 0);

  // Po kliknuti na titulny riadok utriedime riadky podla kliknuteho stlpca
  if FunctionStringGrid.MouseCoord(X, Y).Y = 0 then begin
    fSortColumn := FunctionStringGrid.MouseCoord(X, Y).X;

    // Vytvorenie pomocneho pola riadkov pre ucely triedenia
    SetLength(fSortArray, FunctionStringGrid.RowCount - 1);
    for RowIndex := 0 to FunctionStringGrid.RowCount - 2 do begin
      fSortArray[RowIndex] := TStringList.Create;
      fSortArray[RowIndex].Assign(FunctionStringGrid.Rows[RowIndex + 1]);
    end;

    // Utriedenie
    MergeSortObjects(SortCompare, SortGetItem, SortSetItem, 0, FunctionStringGrid.RowCount - 2);

    // Zapis utriedenych riadkov do StringGrid-u
    for RowIndex := 0 to FunctionStringGrid.RowCount - 2 do begin
      FunctionStringGrid.Rows[RowIndex + 1].Assign(fSortArray[RowIndex]);
      fSortArray[RowIndex].Free;
    end;
  end;
end;



procedure TExportTabFrame.FunctionStringGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  FunctionIndex: Integer;
begin
  fGotoEnabled := False;
  if (Section.ExeCfile as TExecutableFile).IsDisassembled then begin
    FunctionIndex := Integer(FunctionStringGrid.Objects[0, FunctionStringGrid.Row]);
    if (fSection.Functions[FunctionIndex].Name <> '! INVALID RVA !') and (fSection.Functions[FunctionIndex].Section >= 0) then
      fGotoEnabled := True;
  end;
end;



function TExportTabFrame.Translatable: TTranslatable;
begin
  Result := TTranslatableGroups.Create([
    TTranslatableGroup.Create(
      'Export',
      [
        TTranslatableCaption.Create(self, 'Caption')
      ]
    ),
    TTranslatableStrings.Create('Export', FunctionStringGrid.Rows[0], [
      'FunctionListNumber',
      'FunctionListName',
      'FunctionListSection',
      'FunctionListOffset',
      'FunctionListAddress',
      'FunctionListOrdinal'
    ])
  ]);
end;



function TExportTabFrame.SortGetItem(ItemIndex: Integer): TObject;
begin
  Result := fSortArray[ItemIndex];
end;



procedure TExportTabFrame.SortSetItem(ItemIndex: Integer; Item: TObject);
begin
  fSortArray[ItemIndex] := TStrings(Item);
end;



function TExportTabFrame.SortCompare(Item1, Item2: Integer): Integer;
var
  Num1, Num2: Integer;
  Str1, Str2: string;
begin
  Str1 := fSortArray[Item1][fSortColumn];
  Str2 := fSortArray[Item2][fSortColumn];

  case fSortColumn of
    0: begin
      Num1 := StrToInt(Copy(Str1, 1, Length(Str1) - 1));
      Num2 := StrToInt(Copy(Str2, 1, Length(Str2) - 1));
      if num1 > num2 then
        Result := +1
      else if num1 < num2 then
        Result := -1
      else
        Result := 0;
    end;

    2: begin
      Num1 := StrToInt(Str1);
      Num2 := StrToInt(Str2);
      if num1 > num2 then
        Result := +1
      else if num1 < num2 then
        Result := -1
      else
        Result := 0;
    end;

    1, 3, 4, 5: begin
      if str1 > str2 then
        Result := +1
      else if str1 < str2 then
        Result := -1
      else
        Result := 0;
    end;

    else
      Result := 0;
  end;
end;

end.
