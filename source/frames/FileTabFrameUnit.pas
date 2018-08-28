unit FileTabFrameUnit;

interface

uses
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Grids,
  StdCtrls,
  ComCtrls,
  ExtCtrls,
  // project units
  procmat,
  SectionUnit,
  TabFrameTemplateUnit,
  TranslatorUnit,
  ExecFileUnit;

type

  TFileTabFrame = class(TTabFrameTemplate)
    Panel1: TPanel;
    FileOverviewGroupBox: TGroupBox;
    FilenameLabel: TLabel;
    FullPathLabel: TLabel;
    FileSizeLabel: TLabel;
    FileFormatLabel: TLabel;
    BytesLabel: TLabel;
    FilenameEdit: TEdit;
    FullpathEdit: TEdit;
    FilesizeEdit: TEdit;
    FileformatEdit: TEdit;
    ObjectListView: TListView;
    Splitter1: TSplitter;
    AdvancedInfoPanel: TPanel;
    AdvancedInfoGrid: TStringGrid;
    MoreInfoLabel: TLabel;
    Panel2: TPanel;
    procedure AdvancedInfoPanelResize(Sender: TObject);

  protected
    function GetSection: TSection; override;
    procedure HideAddvancedInfoPanel;
    procedure AddAdvancedInfo(InfoName, InfoValue: string);
  public
    constructor Create(AOwner: TComponent; aExecFile: TExecutableFile);
    procedure Translate; override;
  end;


  TCOMFileTabFrame = class(TFileTabFrame)
    constructor Create(AOwner: TComponent; aExecFile: TExecutableFile);
//    procedure Translate(Translator: TTatraDASLanguages); override;
  end;


  TMZFileTabFrame = class(TFileTabFrame)
    constructor Create(AOwner: TComponent; aExecFile: TExecutableFile);
//    procedure Translate(Translator: TTatraDASLanguages); override;
  end;


  TNEFileTabFrame = class(TFileTabFrame)
    constructor Create(AOwner: TComponent; aExecFile: TExecutableFile);
    procedure Translate; override;
  end;


  TPEFileTabFrame = class(TFileTabFrame)
    constructor Create(AOwner: TComponent; aExecFile: TExecutableFile);
    procedure Translate; override;
  end;


  TELFFileTabFrame = class(TFileTabFrame)
    constructor Create(AOwner: TComponent; aExecFile: TExecutableFile);
//    procedure Translate(Translator: TTatraDASLanguages); override;
  end;


  TCustomFileTabFrame = class(TFileTabFrame)
    constructor Create(AOwner: TComponent; aExecFile: TExecutableFile);
//    procedure Translate(Translator: TTatraDASLanguages); override;
  end;


implementation

uses
  CodeSectionUnit,
  MZFileUnit,
  NEFileUnit,
  PEFileUnit,
  ELFFileUnit;


{$R *.dfm}


constructor TFileTabFrame.Create(AOwner: TComponent; aExecFile: TExecutableFile);
begin
  inherited Create(AOwner);
  Caption := 'File info';
  ObjectListView.Columns.Clear;

  // Standard ExecFile file information (FileOverviewGroupBox subitems)
  FileNameEdit.Text := aExecFile.filename;
  FullPathEdit.Text := aExecFile.fullpath;
  FileSizeEdit.Text := IntToStr(aExecFile.FileSize);
end;



procedure TFileTabFrame.AddAdvancedInfo(InfoName, InfoValue: string);
begin
  with AdvancedInfoGrid do begin
    // Increase number of rows only if we add non-first pair of information (there always one row in grid in the beginning)
    if AdvancedInfoGrid.Cells[0, 0] <> '' then
      RowCount := RowCount + 1;

    Cells[0, RowCount - 1] := InfoName;
    Cells[1, RowCount - 1] := InfoValue;
  end;
end;



procedure TFileTabFrame.Translate;
begin
  (Parent as TTabSheet).Caption := Translator.TranslateControl('File', 'Caption');

  // Translate FileOverviewGroupBox and its subitems
  FileOverviewGroupBox.Caption := Translator.TranslateControl('File', 'FileOverview');
  FilenameLabel.Caption := Translator.TranslateControl('File', 'Filename');
  FilesizeLabel.Caption := Translator.TranslateControl('File', 'Filesize');
  FileformatLabel.Caption := Translator.TranslateControl('File', 'Fileformat');
  FullpathLabel.Caption := Translator.TranslateControl('File', 'Fullpath');
  BytesLabel.Caption := Translator.TranslateControl('File', 'Bytes');

  MoreInfoLabel.Caption := Translator.TranslateControl('File', 'MoreInformation');
end;



function TFileTabFrame.GetSection: TSection;
begin
  Result := nil;
end;



procedure TFileTabFrame.HideAddvancedInfoPanel;
begin
  // prerobit, nefunguje dobre
  AdvancedInfoPanel.Align := alNone;
  AdvancedInfoPanel.Width := 0;
end;


//******************************************************************************
// TCOMFileTabFrame class
//******************************************************************************


constructor TCOMFileTabFrame.Create(AOwner: TComponent; aExecFile: TExecutableFile);
begin
  inherited;
  FileFormatEdit.Text := fdCOM;
  ObjectListView.Visible := False;
  HideAddvancedInfoPanel;
end;


//******************************************************************************
// TMZFileTabFrame class
//******************************************************************************


constructor TMZFileTabFrame.Create(AOwner: TComponent; aExecFile: TExecutableFile);
var
  ExecFile: TMZFile;
  RelocIndex: Integer;
begin
  inherited;
  ExecFile := aExecFile as TMZFile;
  FileFormatEdit.Text := fdMZ;
  ObjectListView.Visible := False;

  with ExecFile do begin
    AddAdvancedInfo('Page remainder:', IntToHex(Header.PageRemainder, 4));
    AddAdvancedInfo('Page count:', IntToStr(Header.PageCount) + '(dec)');
    AddAdvancedInfo('Relocations:', IntToStr(Header.RelocCount) + '(dec)');
    AddAdvancedInfo('Header size:', IntToHex(Header.MinMem, 4));
    AddAdvancedInfo('MinMem:', IntToHex(Header.MinMem, 4));
    AddAdvancedInfo('MaxMem:', IntToHex(Header.MaxMem, 4));
    AddAdvancedInfo('SS:SP', IntToHex(Header.reloSS, 4) + ':' + IntToHex(header.EXESP, 4));
    AddAdvancedInfo('Check sum:', IntToHex(Header.CheckSum, 4));
    AddAdvancedInfo('CS:IP', IntToHex(Header.reloCS, 4) + ':' + IntToHex(header.EXEIP, 4));
    AddAdvancedInfo('Overlay:', IntToHex(Header.Overlay, 4));

    // Relocations
    AddAdvancedInfo('', '');
    for RelocIndex := 0 to RelocationsCount - 1 do
      AddAdvancedInfo('Relocation ' + IntToStr(RelocIndex), IntToHex(Relocations[RelocIndex].Segment, 4) + ':' + IntToHex(Relocations[RelocIndex].Offset, 4));
  end;
end;


//******************************************************************************
// TNEFileTabFrame class
//******************************************************************************


constructor TNEFileTabFrame.Create(AOwner: TComponent; aExecFile: TExecutableFile);
var
  ExecFile: TNEFile;
  RowIndex, ColIndex: Integer;
  ListItem: TListItem;
  ListColumn: TListColumn;
begin
  inherited;
  ExecFile := aExecFile as TNEFile;
  FileFormatEdit.Text := fdNE;

  // Segment table

  // Prepare ObjectListView's columns
  for ColIndex := 0 to 4 do begin
    ListColumn := ObjectListView.Columns.Add;
    if ColIndex = 0 then
      ListColumn.Width := 50
    else
      ListColumn.Width := 100;
  end;
  // Fill data
  for RowIndex := 0 to ExecFile.Header.SegmentTableEntryNumber - 1 do begin
    ListItem := ObjectListView.Items.Add;
    ListItem.Caption := IntToStr(RowIndex + 1) + '.';
    with ExecFile.SegmentTable[RowIndex] do begin
      ListItem.SubItems.Add(IntToHex(Offset * 16, 8));
      ListItem.SubItems.Add(IntToHex(Size, 4));
      ListItem.SubItems.Add(IntToHex(AllocationSize, 4));
      ListItem.SubItems.Add(IntToHex(Flags, 4));
    end;
  end;
  ObjectListView.Visible := True;

  // Advanced information
  with ExecFile do begin
    AddAdvancedInfo('Initial heap size', IntToHex(header.InitialHeapSize, 4));
    AddAdvancedInfo('Initial stack size', IntToHex(header.InitialStackSize, 4));
    AddAdvancedInfo('CS:IP', IntToHex(header._CS, 4) + ':' + IntToHex(header._IP, 4));
    AddAdvancedInfo('SS:SP', IntToHex(header._SS, 4) + ':' + IntToHex(header._SP, 4));
    AddAdvancedInfo('Automatic data segment number', IntToStr(header.AutomaticDataSegmentNumber));
    AddAdvancedInfo('Shift count', IntToStr(header.ShiftCount));
    AddAdvancedInfo('Target OS', TargetOS);
    AddAdvancedInfo('Flags', IntToHex(header.Flags, 4));
  end;
end;



procedure TNEFileTabFrame.Translate;
begin
  inherited;
  // ObjectListView column captions
  ObjectListView.Columns[0].Caption := Translator.TranslateControl('File', 'SectionIndex');
  ObjectListView.Columns[1].Caption := Translator.TranslateControl('File', 'NESectionOffset');
  ObjectListView.Columns[2].Caption := Translator.TranslateControl('File', 'NESectionSize');
  ObjectListView.Columns[3].Caption := Translator.TranslateControl('File', 'NESectionAllocationSize');
  ObjectListView.Columns[4].Caption := Translator.TranslateControl('File', 'NESectionFlags');
end;

//******************************************************************************
// TPEFileTabFrame class
//******************************************************************************


constructor TPEFileTabFrame.Create(AOwner: TComponent; aExecFile: TExecutableFile);
var
  RowIndex, ColIndex: Integer;
  ListItem: TListItem;
  ExecFile: TPEFile;
  ListColumn: TListColumn;
  CPUTypeStr: string;
begin
  inherited Create(AOwner, aExecFile);
  ExecFile := aExecFile as TPEFile;
  FileFormatEdit.Text := fdPE;


  // Object table

  // Prepare ObjectListView's columns
  ObjectListView.Columns.Clear;
  for ColIndex := 0 to 7 do begin
    ListColumn := ObjectListView.Columns.Add;
    if ColIndex = 0 then
      ListColumn.Width := 50
    else
      ListColumn.Width := 100;
  end;
  // Fill data
  for RowIndex := 0 to ExecFile.Header.ObjectCount - 1 do begin
    ListItem := ObjectListView.Items.Add;
    ListItem.Caption := IntToStr(RowIndex + 1) + '.';
    with ExecFile.ObjectTable[RowIndex] do begin
      ListItem.SubItems.Add(Name);
      ListItem.SubItems.Add(IntToHex(Offset, 8));
      ListItem.SubItems.Add(IntToHex(Size, 8));
      ListItem.SubItems.Add(IntToHex(RVA, 8));
      ListItem.SubItems.Add(IntToHex(RVA + ExecFile.Header.ImageBase, 8));
      ListItem.SubItems.Add(IntToHex(VirtualSize, 8));
      ListItem.SubItems.Add(IntToHex(Flags, 8));
    end;
  end;
  ObjectListView.Visible := True;

  // Advanced information
  with ExecFile do begin
    AddAdvancedInfo('Image base:', IntToHex(Header.ImageBase, 8));
    AddAdvancedInfo('Image size:', IntToHex(Header.Imagesize, 8));
    AddAdvancedInfo('Header size: ', IntToHex(Header.HeaderSize, 8));
    AddAdvancedInfo('Entry point RVA:', IntToHex(Header.EntryPoint, 8));
    AddAdvancedInfo('', '');
    AddAdvancedInfo('Time - Date stamp', IntToHex(header.TimeDateStamp, 8));
    AddAdvancedInfo('Flags: ', IntToHex(Header.Flags, 4) + ' = ' + PEFileType);
    case CPUType of
      cpu386: CPUTypeStr := '80386';
      cpu486: CPUTypeStr := '80486';
      cpu586: CPUTypeStr := '80586';
      cpuUnknown: CPUTypeStr := 'unknown';
    end;
    AddAdvancedInfo('CPU type required: ', CPUTypeStr);
    AddAdvancedInfo('', '');
    AddAdvancedInfo('Object align: ', IntToHex(header.ObjectAlign, 8));
    AddAdvancedInfo('File align: ', IntToHex(header.FileAlign, 8));
    AddAdvancedInfo('File checksum: ', IntToHex(header.FileCheckSum, 8));
  end;
end;



procedure TPEFileTabFrame.Translate;
begin
  inherited;
  // ObjectListView column captions
  ObjectListView.Columns[0].Caption := Translator.TranslateControl('File', 'SectionIndex');
  ObjectListView.Columns[1].Caption := Translator.TranslateControl('File', 'PESectionName');
  ObjectListView.Columns[2].Caption := Translator.TranslateControl('File', 'PESectionFileOffset');
  ObjectListView.Columns[3].Caption := Translator.TranslateControl('File', 'PESectionFileSize');
  ObjectListView.Columns[4].Caption := Translator.TranslateControl('File', 'PESectionRVA');
  ObjectListView.Columns[5].Caption := Translator.TranslateControl('File', 'PESectionMemAddress');
  ObjectListView.Columns[6].Caption := Translator.TranslateControl('File', 'PESectionMemSize');
  ObjectListView.Columns[7].Caption := Translator.TranslateControl('File', 'PESectionFlags');

end;


//******************************************************************************
// TELFFileTabFrame class
//******************************************************************************


constructor TELFFileTabFrame.Create(AOwner: TComponent; aExecFile: TExecutableFile);
begin
  inherited;
  FileFormatEdit.Text := fdELF;
  ObjectListView.Visible := False;
  HideAddvancedInfoPanel;
end;


//******************************************************************************
// TCustomFileTabFrame class
//******************************************************************************


constructor TCustomFileTabFrame.Create(AOwner: TComponent; aExecFile: TExecutableFile);
begin
  inherited;
  FileFormatEdit.Text := fdCustom;

  ObjectListView.Visible := False;

  with aExecFile do begin
    AddAdvancedInfo('Code section offset', IntToHex((Sections[0] as TCodeSection).FileOffset, 8));
    AddAdvancedInfo('Code section size', IntToHex((Sections[0] as TCodeSection).FileSize, 8));
    if (Sections[0] as TCodeSection).Bit32 then
      AddAdvancedInfo('Mode:', '32 bit')
    else
      AddAdvancedInfo('Mode:', '16 bit');
//    AddAdvancedInfo('Entry point', IntToHex(EntryPoint, 8));
  end;
end;



procedure TFileTabFrame.AdvancedInfoPanelResize(Sender: TObject);
begin
  inherited;
  AdvancedInfoGrid.ColWidths[0] := AdvancedInfoGrid.Width div 2;
  AdvancedInfoGrid.ColWidths[1] := AdvancedInfoGrid.Width div 2;
end;

end.
