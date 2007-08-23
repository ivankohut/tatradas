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

  procmat,
  SectionUnit,
  TabFrameTemplateUnit,
  Languages,
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
    Panel2: TPanel;
    AdvancedInfoGrid: TStringGrid;
    Label1: TLabel;

  protected
    function GetSection: TSection; override;
    procedure AddAdvancedInfo(InfoName, InfoValue: string);
  public
    constructor Create(AOwner: TComponent; aExecFile: TExecutableFile);
    procedure Translate(Translator: TTatraDASLanguages); override;
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
//    procedure Translate(Translator: TTatraDASLanguages); override;
  end;


  TPEFileTabFrame = class(TFileTabFrame)
    constructor Create(AOwner: TComponent; aExecFile: TExecutableFile);
//    procedure Translate(Translator: TTatraDASLanguages); override;
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
  Caption:= 'File info';

  // Standard ExecFile file information (FileOverviewGroupBox subitems)
  FileNameEdit.Text:= aExecFile.filename;
  FullPathEdit.Text:= aExecFile.fullpath;
  FileSizeEdit.Text:= IntToStr(aExecFile.FileSize);
end;



procedure TFileTabFrame.AddAdvancedInfo(InfoName, InfoValue: string);
begin
  with AdvancedInfoGrid do begin
    RowCount := RowCount + 1;
    Cells[0, RowCount - 1]:= InfoName;
    Cells[1, RowCount - 1]:= InfoValue;
  end;
end;



procedure TFileTabFrame.Translate(Translator: TTatraDASLanguages);
begin
  // Translate FileOverviewGroupBox and its subitems
  FileOverviewGroupBox.Caption:=Translator.TranslateControl('File','FileOverview');
  FilenameLabel.Caption:=Translator.TranslateControl('File','Filename');
  FilesizeLabel.Caption:=Translator.TranslateControl('File','Filesize');
  FileformatLabel.Caption:=Translator.TranslateControl('File','Fileformat');
  FullpathLabel.Caption:=Translator.TranslateControl('File','Fullpath');
  BytesLabel.Caption:=Translator.TranslateControl('File','Bytes');

  // Translate ObjectListView column captions
  ObjectListView.Columns[1].Caption:=Translator.TranslateControl('File','SectionName');
  ObjectListView.Columns[2].Caption:=Translator.TranslateControl('File','SectionFileOffset');
  ObjectListView.Columns[3].Caption:=Translator.TranslateControl('File','SectionFileSize');
  ObjectListView.Columns[4].Caption:=Translator.TranslateControl('File','SectionMemAddress');
  ObjectListView.Columns[5].Caption:=Translator.TranslateControl('File','SectionMemSize');
  ObjectListView.Columns[6].Caption:=Translator.TranslateControl('File','SectionFlags');
end;


function TFileTabFrame.GetSection: TSection;
begin
  result:=nil;
end;


//******************************************************************************
// TCOMFileTabFrame class
//******************************************************************************


constructor TCOMFileTabFrame.Create(AOwner: TComponent; aExecFile: TExecutableFile);
begin
  inherited;
  FileFormatEdit.Text:= fdCOM;
  ObjectListView.Visible:= false;
  AdvancedInfoGrid.Visible:= false;
end;


//******************************************************************************
// TMZFileTabFrame class
//******************************************************************************


constructor TMZFileTabFrame.Create(AOwner: TComponent; aExecFile: TExecutableFile);
var
  ExecFile: TMZFile;
begin
  inherited;
  ExecFile:= aExecFile as TMZFile;
  FileFormatEdit.Text:= fdMZ;
  ObjectListView.Visible:= false;

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
  end;
end;


//******************************************************************************
// TNEFileTabFrame class
//******************************************************************************


constructor TNEFileTabFrame.Create(AOwner: TComponent; aExecFile: TExecutableFile);
var
  ExecFile: TNEFile;
begin
  inherited;
  ExecFile:= aExecFile as TNEFile;
  FileFormatEdit.Text:= fdNE;
  with ExecFile do begin
    AddAdvancedInfo('Module reference table number', IntToStr(header.ModuleReferenceTableEntryNumber));
    AddAdvancedInfo('Module reference table file offset', IntToHex(header.ModuleReferenceTableRO + NEOffset,8));
    AddAdvancedInfo('Import Module name table file offset', IntToHex(header.ImportedNameTableRO + NEOffset,8));
    // ... add more
  end;
end;


//******************************************************************************
// TPEFileTabFrame class
//******************************************************************************


constructor TPEFileTabFrame.Create(AOwner: TComponent; aExecFile: TExecutableFile);
var
  ColIndex, RowIndex: integer;
  ListItem: TListItem;
  ExecFile: TPEFile;
begin
  inherited Create(AOwner, aExecFile);
  ExecFile:= aExecFile as TPEFile;
  FileFormatEdit.Text:= fdPE;
  // Object table
  for RowIndex:=0 to ExecFile.Header.ObjectCount - 1 do begin
    ListItem:= ObjectListView.Items.Add;
    ListItem.Caption:= IntToStr(RowIndex + 1) + '.';
    with ExecFile.ObjectTable[RowIndex] do begin
      ListItem.SubItems.Add(Name);
      ListItem.SubItems.Add(IntToHex(Offset, 8));
      ListItem.SubItems.Add(IntToHex(Size, 8));
      ListItem.SubItems.Add(IntToHex(RVA, 8));
      ListItem.SubItems.Add(IntToHex(VirtualSize, 8));
      ListItem.SubItems.Add(IntToHex(Flags, 8));
    end;
  end;

  // Advanced information
  with ExecFile do begin
    AddAdvancedInfo('Image base:', IntToHex(Header.ImageBase, 8));
    AddAdvancedInfo('Image size:', IntToHex(Header.Imagesize, 8));
    AddAdvancedInfo('Header size: ', IntToHex(Header.HeaderSize, 8));
    AddAdvancedInfo('Entry point RVA:', IntToHex(Header.EntryPoint, 8));
    AddAdvancedInfo('', '');
    AddAdvancedInfo('Time - Date stamp', IntToHex(header.TimeDateStamp, 8));
    AddAdvancedInfo('Flags: ', IntToHex(Header.Flags, 4) + ' = ' + PEFileType);
    AddAdvancedInfo('CPU type required: ', CPUType);
    AddAdvancedInfo('', '');
    AddAdvancedInfo('Object align: ', IntToHex(header.ObjectAlign, 8));
    AddAdvancedInfo('File align: ', IntToHex(header.FileAlign, 8));
    AddAdvancedInfo('File checksum: ', IntToHex(header.FileCheckSum, 8));
  end;
end;


//******************************************************************************
// TELFFileTabFrame class
//******************************************************************************


constructor TELFFileTabFrame.Create(AOwner: TComponent; aExecFile: TExecutableFile);
begin
  inherited;
  FileFormatEdit.Text:= fdELF;
end;


//******************************************************************************
// TCustomFileTabFrame class
//******************************************************************************


constructor TCustomFileTabFrame.Create(AOwner: TComponent; aExecFile: TExecutableFile);
begin
  inherited;
  FileFormatEdit.Text:= fdCustom;

  with aExecFile do begin
    AddAdvancedInfo('Code section offset', IntToHex((Sections[0] as TCodeSection).FileOffset, 8));
    AddAdvancedInfo('Code section size', IntToHex((Sections[0] as TCodeSection).FileSize, 8));
    if (Sections[0] as TCodeSection).Bit32 then
      AddAdvancedInfo('Mode:', '32 bit')
    else
      AddAdvancedInfo('Mode:', '16 bit');
    AddAdvancedInfo('Entry point', IntToHex(EntryPoint, 8));
  end;
end;


end.




