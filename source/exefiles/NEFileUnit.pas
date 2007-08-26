{ TODO:
    - detekcia sekci pre HexEditor nie je este OK
    - AdvancedInfo
    - medzi segmentove call a jmp - relokacia (napr. WINSOCK.DLL, sekcia 1, 0x00000020 a 0x00000026)
    - treba packed record ?
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
    FLAOffset,FLASize: word;
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

  TEntryTableEntryType = (etEmpty,etFixed,etMovable);


  TFixedBundle = packed record
    Flag: byte;
    Offset: word;
  end;

  TMovableBundle = packed record
    Flag: byte;
    Int3F: word;
    Segment: byte;
    Offset: word;
  end;

  TEntryTableEntry = record
     count: byte;
     typ: TEntryTableEntryType;
     Fixed: array of TFixedBundle;
     Movable: array of TMovableBundle;
  end;

  TNEFile = class(TExecutableFile)
  private
    fHeader: TNEHeader;
    fNEOffset: cardinal;
    SegmentTable: array of TSegmentTableEntry;
//       ResourceTable:
//       RsidentNameTable:
  public
    EntryTable: array of TEntryTableEntry;

    constructor Create(InputFile: TStream; aFileName: TFileName); overload; override;
    constructor Create; overload; override;
    destructor Destroy; override;
    function SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean; override;
    function LoadFromFile(DHF: TStream; var DAS: TextFile): boolean; override;

    function GetSegmentType(Index: integer): TSectionType;

    property Header: TNEHeader read fHeader;
    property NEOffset: cardinal read fNEOffset;
  end;


implementation


const
  c_NEFileAdvancedInfoCount = 3;


  
constructor TNEFile.Create(InputFile: TStream; aFileName: TFileName);

  function IsExecutableSegment(Index: Integer): boolean;
  begin
    result:= (SegmentTable[Index].Flags and word(1)) = 0;
  end;

var
  SegmentIndex: integer;
  CodeSection: TCodeSection;
  SectionIndex: integer;

  SegmentImports: array of TNESectionImports;

  BundleIndex: integer;
  SegmentMovement: byte;
  BundleEntriesCount: byte;
  ModuleNameLength: byte;
  
begin
  inherited;

  // Get NE header address
  InputFile.Seek(60, 0);
  InputFile.Read(fNEOffset, 4);

  // Read NE header
  InputFile.Seek(fNEOffset, 0);
  InputFile.Read(fHeader, SizeOf(TNEHeader));

  // Read Segment Table
  SetLength(SegmentTable, Header.SegmentTableEntryNumber);
  for SegmentIndex:= 0 to Header.SegmentTableEntryNumber - 1 do
    InputFile.Read(SegmentTable[SegmentIndex], SizeOf(TSegmentTableEntry));


  // Read Entry Table
  InputFile.Position:= header.EntryTableRO + fNEOffset;
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
  for SegmentIndex:= 0 to Length(SegmentTable) - 1 do
    if IsExecutableSegment(SegmentIndex) then begin

    // vecny problem: velkost segmentu v subore InputFile pamati, obcas je pamat vacsia, ale aj kod v nej, ale subor je mensi, t.j. treba asi doplnit nulami
    //      neviem, ci subor hoci je mensi, predsa obsahuje chybajuce data (niekedy ano)
      CodeSection:=TCodeSection.Create(InputFile, false, SegmentTable[SegmentIndex].offset*16, SegmentTable[SegmentIndex].Size, 0, SegmentTable[SegmentIndex].AllocationSize, fCodeSectionsCount, 'N/InputFile - code', self);
      Inc(fCodeSectionsCount);
      if SegmentIndex = header._CS - 1 then
        CodeSection.EntryPointAddress:= header._IP;
      Sections.Add(CodeSection);

      // Segment has relocations
      if (SegmentTable[SegmentIndex].Flags and $100) <> 0 then begin
        SetLength(SegmentImports, Length(SegmentImports) + 1);
        SegmentImports[High(SegmentImports)].Offset:= SegmentTable[SegmentIndex].offset*16 + SegmentTable[SegmentIndex].Size;
        SegmentImports[High(SegmentImports)].SectionIndex:= CodeSection.SectionIndex;
        SegmentImports[High(SegmentImports)].SectionFileOffset:= SegmentTable[SegmentIndex].offset*16;
      end;
    end;

  // Read Import section
  if Header.ModuleReferenceTableEntryNumber > 0 then begin
    fImportSection:= TImportSection.CreateFromNEFile(InputFile, fNEOffset + Header.ModuleReferenceTableRO, fNEOffset + Header.ImportedNameTableRO, Header.ModuleReferenceTableEntryNumber, Header.EntryTableRO-header.ImportedNameTableRO, SegmentImports, '_IMPORT', self);
    Sections.Add(ImportSection);
  end;

  // Read Export section
  InputFile.Position:= fNEOffset + header.ResidentNameTableRO;
  InputFile.Read(ModuleNameLength, 1);
  if ModuleNameLength <> 0 then begin
    fExportSection:= TExportSection.CreateFromNEFile(InputFile, fNEOffset + Header.ResidentNameTableRO, Header.NonResidentNameTableOffset, Header.NonResidentNameTableSize, '_EXPORT', self);
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
  i:integer;
begin
  if soProject in SaveOptions then begin
    DHF.Write(header, SizeOf(Header));
    for i:=0 to header.SegmentTableEntryNumber-1 do
      DHF.Write(SegmentTable[i], SizeOf(TSegmentTableEntry));
  end;
  result:=inherited SaveToFile(DHF, DAS, SaveOptions);
end;



function TNEFile.LoadFromFile(DHF: TStream; var DAS: TextFile): boolean;
var i: integer;
begin
  DHF.Read(fHeader,SizeOf(header));
  SetLength(SegmentTable, header.SegmentTableEntryNumber);
  for i:=0 to fHeader.SegmentTableEntryNumber-1 do                                // Object Table
    DHF.Read(SegmentTable[i], sizeof(TSegmentTableEntry));
  result:=inherited LoadFromFile(DHF, DAS);
end;



destructor TNEFile.Destroy;
begin
  inherited;
end;



function TNEFile.GetSegmentType(Index: integer): TSectionType;
begin

  // Code segment
  if (SegmentTable[Index].Flags and word(1)) = 0 then begin
    result:=stCode;
    Exit;
  end;

  // Import
  if header.ModuleReferenceTableEntryNumber > 0 then
    if (SegmentTable[Index].Offset <= header.ModuleReferenceTableRO + fNEOffset) and (Header.ModuleReferenceTableRO + fNEOffset <= SegmentTable[Index].Offset + SegmentTable[Index].Size) then begin
      result:=stImport;
      Exit;
    end;


  result:= stDummy;
end;



end.