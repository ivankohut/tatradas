{ TODO:
    - medzi segmentove call a jmp - relokacia (napr. WINSOCK.DLL, sekcia 1, 0x00000020 a 0x00000026)
    - resources

}

unit NEFileUnit;

{$INCLUDE 'delver.inc'}

interface

uses
  Classes,
  SysUtils,

  StringRes,
  procmat,
  ExecFileUnit,
  SectionUnit,
  CodeSectionUnit,
  ImportSectionUnit,
  ExportSectionUnit;

const
  btConstant = $FE;
  btMovable = $FF;

type
  TNEHeader = record
    Sign: word;
    LinkerVersion, LinkerRevision: byte;
    EntryTableRO: word;
    EntryTableSize: word;
    _reserved1: cardinal;
    Flags: word;
    AutomaticDataSegmentNumber: word;
    InitialHeapSize: word;
    InitialStackSize: word;
    _IP,_CS: word;
    _SP,_SS: word;
    SegmentTableEntryNumber: word;
    ModuleReferenceTableEntryNumber: word;
    NonResidentNameTableSize: word;
    SegmentTableRO: word;
    ResourceTableRO: word;
    ResidentNameTableRO: word;
    ModuleReferenceTableRO: word;
    ImportedNameTableRO: word;
    NonResidentNameTableOffset: cardinal;
    MoveableEntrypointsNumber: word;
    ShiftCount: word;
    ResourceSegmentNumber: word;
    TargetOS: byte;
    AdditionalInfo: byte;
    FLAOffset, FLASize: word;
    _reserved2: word;
    ExpectedVersionNumber: word
  end;

  TSegmentTableEntry = record
    Offset: word;
    Size: word;
    Flags: word;
    AllocationSize: word;
  end;

  TResourceInformationBlock = record
    TypeID: word;
    count: word;
    reserved: cardinal;
  end;

  TResourceEntry = record
    Offset: word;
    Size: word;
    Flags: word;
    resID: word;
    reserved: cardinal;
  end;


  TNEFile = class(TExecutableFile)
  private
    fHeader: TNEHeader;
    fSegmentTable: array of TSegmentTableEntry;
    function GetSegmentTableEntry(Index: integer): TSegmentTableEntry;
    function GetTargetOS: string;

  public
    constructor Create(InputFile: TStream; aFileName: TFileName); overload; override;
    constructor Create; overload; override;
    destructor Destroy; override;
    function SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean; override;
    function LoadFromFile(DHF: TStream; var DAS: TextFile): boolean; override;

  // Fields and methods specific to PE File
    property Header: TNEHeader read fHeader;
    property TargetOS: string read GetTargetOS;
    property SegmentTable[Index: integer]: TSegmentTableEntry read GetSegmentTableEntry;
  end;


implementation


const
  c_NEFileAdvancedInfoCount = 3;



constructor TNEFile.Create(InputFile: TStream; aFileName: TFileName);

  function IsExecutableSegment(Index: Integer): boolean;
  begin
    result:= (fSegmentTable[Index].Flags and word(1)) = 0;
  end;

var
  SegmentIndex: integer;
  CodeSection: TCodeSection;
  SegmentSectorSize: cardinal;

  SegmentImports: array of TNESectionImports;
  SegmentImportsCount: integer;
  EntryTable: array of TEntryTableEntry;

  BundleIndex: integer;
  SegmentMovement: byte;
  BundleEntriesCount: byte;
  ModuleNameLength: byte;
  NEOffset: cardinal;

begin
  inherited;

  // Get NE header address
  InputFile.Seek(60, 0);
  InputFile.Read(NEOffset, 4);

  // Read NE header
  InputFile.Seek(NEOffset, 0);
  InputFile.Read(fHeader, SizeOf(TNEHeader));

  SegmentSectorSize:= 1 shl fHeader.ShiftCount;

  // Read Segment Table
  SetLength(fSegmentTable, Header.SegmentTableEntryNumber);
  for SegmentIndex:= 0 to Header.SegmentTableEntryNumber - 1 do
    InputFile.Read(fSegmentTable[SegmentIndex], SizeOf(TSegmentTableEntry));

  // Set file Regions
  fRegions.Add('MZ header', 0, SizeOf(TMZHeader));
  fRegions.Add('NE header', 60, SizeOf(TNEHeader));
  for SegmentIndex:= 0 to Header.SegmentTableEntryNumber - 1 do begin
    with fSegmentTable[SegmentIndex] do
      if Size > 0 then
        fRegions.Add('Segment ' + IntToStr(SegmentIndex + 1), Offset * SegmentSectorSize, Size);
  end;
  fRegions.Finish;


  // Read Entry Table
  InputFile.Position:= header.EntryTableRO + NEOffset;
  BundleIndex:=0;
  InputFile.Read(BundleEntriesCount, 1);
  while BundleEntriesCount <> 0 do begin
    SetLength(EntryTable, BundleIndex + 1);
    EntryTable[BundleIndex].Count:= BundleEntriesCount;
    InputFile.Read(SegmentMovement, 1);
    case SegmentMovement of
      0: EntryTable[BundleIndex].typ:= etEmpty;

      // Movable
      $FF: begin
        EntryTable[BundleIndex].Typ:= etMovable;
        SetLength(EntryTable[BundleIndex].Movable, BundleEntriesCount);
        InputFile.Read(EntryTable[BundleIndex].Movable[0], BundleEntriesCount * SizeOf(TMovableBundle));
      end

      // Fixed
      else begin
        EntryTable[BundleIndex].Typ:= etFixed;
        SetLength(EntryTable[BundleIndex].Fixed, BundleEntriesCount);
        InputFile.Read(EntryTable[BundleIndex].Fixed[0], BundleEntriesCount*SizeOf(TFixedBundle));
      end;
    end;
    Inc(BundleIndex);
    InputFile.Read(BundleEntriesCount, 1);
  end;


  // Read Code segments and create code sections
  for SegmentIndex:= 0 to Length(fSegmentTable) - 1 do
    if IsExecutableSegment(SegmentIndex) then begin

    // vecny problem: velkost segmentu v subore InputFile pamati, obcas je pamat vacsia, ale aj kod v nej, ale subor je mensi, t.j. treba asi doplnit nulami
    //      neviem, ci subor hoci je mensi, predsa obsahuje chybajuce data (niekedy ano)
      CodeSection:=TCodeSection.Create(InputFile, false, fSegmentTable[SegmentIndex].offset*SegmentSectorSize, fSegmentTable[SegmentIndex].Size, 0, fSegmentTable[SegmentIndex].AllocationSize, fCodeSectionsCount, 'N/InputFile - code', self);
      Inc(fCodeSectionsCount);
      if SegmentIndex = header._CS - 1 then
        CodeSection.EntryPointAddress:= header._IP;
      Sections.Add(CodeSection);

      // Segment has relocations
      SegmentImportsCount:= 0;
      if (fSegmentTable[SegmentIndex].Flags and $100) <> 0 then begin
        Inc(SegmentImportsCount);
        SetLength(SegmentImports, SegmentImportsCount);
        SegmentImports[SegmentImportsCount - 1].Offset:= fSegmentTable[SegmentIndex].offset*SegmentSectorSize + fSegmentTable[SegmentIndex].Size;
        SegmentImports[SegmentImportsCount - 1].SectionIndex:= CodeSection.SectionIndex;
        SegmentImports[SegmentImportsCount - 1].SectionFileOffset:= fSegmentTable[SegmentIndex].offset*SegmentSectorSize;
      end;
    end;

  // Read Import section
  if Header.ModuleReferenceTableEntryNumber > 0 then begin
    fImportSection:= TImportSection.CreateFromNEFile(InputFile, NEOffset + Header.ModuleReferenceTableRO, NEOffset + Header.ImportedNameTableRO, Header.ModuleReferenceTableEntryNumber, Header.EntryTableRO-header.ImportedNameTableRO, SegmentImports, '_IMPORT', self);
    Sections.Add(ImportSection);
  end;

  // Read Export section
  InputFile.Position:= NEOffset + header.ResidentNameTableRO;
  InputFile.Read(ModuleNameLength, 1);
  if ModuleNameLength <> 0 then begin
    fExportSection:= TExportSection.CreateFromNEFile(InputFile, NEOffset + Header.ResidentNameTableRO, Header.NonResidentNameTableOffset, Header.NonResidentNameTableSize, EntryTable, '_EXPORT', self);
    Sections.Add(ExportSection);
  end;

  fExecFormat := ffNE;
end;



constructor TNEFile.Create;
begin
  inherited;
  fExecFormat:= ffNE;
end;



function TNEFile.SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean;
var
  SegmentIndex: integer;
begin
  result:= inherited SaveToFile(DHF, DAS, SaveOptions);
  if soProject in SaveOptions then begin
    DHF.Write(fHeader, SizeOf(fHeader));
    for SegmentIndex := 0 to fHeader.SegmentTableEntryNumber - 1 do
      DHF.Write(fSegmentTable[SegmentIndex], SizeOf(TSegmentTableEntry));
  end;
end;



function TNEFile.LoadFromFile(DHF: TStream; var DAS: TextFile): boolean;
var
  SegmentIndex: integer;
begin
  result:= inherited LoadFromFile(DHF, DAS);
  DHF.Read(fHeader, SizeOf(header));
  SetLength(fSegmentTable, header.SegmentTableEntryNumber);
  for SegmentIndex := 0 to fHeader.SegmentTableEntryNumber - 1 do
    DHF.Read(fSegmentTable[SegmentIndex], sizeof(TSegmentTableEntry));
end;



destructor TNEFile.Destroy;
begin
  inherited;
end;



function TNEFile.GetSegmentTableEntry(Index: integer): TSegmentTableEntry;
begin
  result:= fSegmentTable[Index];
end;



function TNEFile.GetTargetOS: string;
begin
  case header.TargetOS of
    0: result:= 'Unknown';
    1: result:= '_reserved_value_';
    2: result:= 'MS Windows';
    3: result:= '_reserved_value_';
    4: result:= '_reserved_value_';
  else
    result:= '_undefined_value_';
  end;
end;



end.