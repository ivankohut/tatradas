unit PEFileUnit;

{$INCLUDE 'delver.inc'}

interface

uses
  SysUtils,
  Classes,
  // project units
  procmat,
  StringRes,
  ExecFileUnit,
  SectionUnit,
  CodeSectionUnit,
  ImportSectionUnit,
  ExportSectionUnit;

type

  TPEHeader = record
    Sign: Cardinal;
    Machine: Word;
    ObjectCount: Word;
    TimeDateStamp: Cardinal;
    res1a: Cardinal;
    res2a: Cardinal;
    NTHDRsize: Word;
    Flags: Word;
    res1b: Word;
    LMajor, LMinor: Byte;
//    res2b:word;  - velke zistenie, vdaka freepascalu
    res2b: Cardinal;
    res3a: Cardinal;
    res4a: Cardinal;
    EntryPoint: Cardinal;
    res5a: Cardinal;
    res6a: Cardinal;
    ImageBase: Cardinal;
    ObjectAlign, FileAlign: Cardinal;
    OsMajor, OsMinor: Word;
    UserMajor, UserMinor: Word;
    SubSysMajor, SubSysMinor: Word;
    res7a: Cardinal;
    Imagesize, HeaderSize: Cardinal;
    FileCheckSum: Cardinal;
    SubSystem, DLLFlags: Word;
    StackReserveSize, StackCommitSize: Cardinal;
    HeapReserveSize, HeapCommitSize: Cardinal;
    res8a: Cardinal;
    InterestingCount: Cardinal;
  end;

  TInterestingRVAs = record
    ExportTableRVA, ExportDataSize: Cardinal;
    ImportTableRVA, ImportDataSize: Cardinal;
    ResourceTableRVA, ResourceDataSize: Cardinal;
    ExceptionTableRVA, ExceptionDataSize: Cardinal;
    SecurityTableRVA, SecurityDataSize: Cardinal;
    FixupTableRVA, FixupDataSize: Cardinal;
    DebugTableRVA, DebugDataSize: Cardinal;
    ImageDescriptionRVA, DescriptionSize: Cardinal;
    MachineSpecificRVA, MachineSpecificSize: Cardinal;
    TLSRVA, TLSSize: Cardinal;
  end;

  TObjectTableEntry = record
    internal_name: array[1..8] of Char;
    VirtualSize, RVA: Cardinal;
    Size, Offset: Cardinal;
    RelocOffset, LineNumberOffset: Cardinal;
    RelocCount, LineNumberCount: Word;
    Flags: Cardinal;
    Name: string;
  end;

  TCPUType = (cpuUnknown, cpu386, cpu486, cpu586);

  TPeFile = class(TExecutableFile)
  private
    fHeader: TPEHeader;
    fObjectTable: array of TObjectTableEntry;

    function GetCPUType: TCPUType;
    function GetPEFileType: string;

    function IsObjectExecutable(ObjectIndex: Integer): Boolean;
    function GetObjectNumberFromRVA(RVA: Cardinal): Integer;
    function GetObjectTableEntry(Index: Integer): TObjectTableEntry;

  // Common fields and methods
  public
    constructor Create; overload; override;
    constructor Create(InputFile: TStream; aFileName: TFileName); overload; override;
    destructor Destroy; override;
    procedure SaveToFile(DHF: TStream; var DAS: TextFile); override;
    procedure LoadFromFile(DHF: TStream; var DAS: TextFile); override;

  // Fields and methods specific to PE File
  public
    function GetSectionNumberFromRVA(RVA: Cardinal): Integer;
    property Header: TPEHeader read fHeader;
    property ObjectTable[Index: Integer]: TObjectTableEntry read GetObjectTableEntry;
    property PEFileType: string read GetPEFileType;
    property CPUType: TCPUType read GetCPUType;
  end;


implementation

uses
  ExceptionsUnit;

const
  c_ExecutableSection = $20000000;



constructor TPEFile.Create;
begin
  inherited;
  fExecFormat := ffPE;
end;



destructor TPEFile.Destroy;
begin
  inherited;
end;



constructor TPEFile.Create(InputFile: TStream; aFileName: TFileName);
var
  i: Integer;
  PEHeaderOffset: Cardinal;
  CodeSection: TCodeSection;
  ObjectIndex: Integer;
  InterestingRVAs: TInterestingRVAs;

begin
  inherited;

  // Seek and read PE header
  InputFile.Seek(60, 0);
  InputFile.Read(PEHeaderOffset, 4);
  InputFile.Seek(PEHeaderOffset, 0);
  InputFile.Read(fHeader, SizeOf(TPEHeader));

  // Read Interesting RVA/Sizes table
  InputFile.Read(InterestingRVAs, 4 * (Header.InterestingCount));

  // Read Object Table
  InputFile.Seek(PEHeaderOffset + 248, 0);
  SetLength(fObjectTable, header.objectcount);
  for i := 0 to Header.ObjectCount - 1 do begin
    InputFile.Read(fObjectTable[i], 40);
    fObjectTable[i].Name := PChar(@fObjectTable[i].internal_name[1]);
  end;

  // Set file regions
  fRegions.Add('MZ header', 0, SizeOf(TMZHeader));
  fRegions.Add('PE header', 60, header.HeaderSize);
  for ObjectIndex := 0 to header.ObjectCount - 1 do begin
    with ObjectTable[ObjectIndex] do
      if size > 0 then
        fRegions.Add(Name, Offset, Size);
  end;
  fRegions.Finish;

  // Create code sections
  for i := 0 to Header.ObjectCount - 1 do begin
    if IsObjectExecutable(i) then begin
      CodeSection := TCodeSection.Create(InputFile, True, ObjectTable[i].Offset, ObjectTable[i].Size, ObjectTable[i].RVA + Header.ImageBase, ObjectTable[i].VirtualSize, fCodeSectionsCount, ObjectTable[i].Name, self);
      Inc(fCodeSectionsCount);
      if CodeSection.IsInSection(Header.EntryPoint + Header.ImageBase) then
        CodeSection.EntryPointAddress := Header.EntryPoint + Header.ImageBase - (ObjectTable[i].RVA + Header.ImageBase);

      Sections.Add(CodeSection);
    end;
  end;

  // Create Import section
  if InterestingRVAs.ImportTableRVA <> 0 then begin
    i := GetObjectNumberFromRVA(InterestingRVAs.ImportTableRVA);
    fImportSection := TImportSection.CreateFromPEFile(InputFile, InterestingRVAs.ImportTableRVA, Header.ImageBase, ObjectTable[i].Offset, ObjectTable[i].Size, ObjectTable[i].RVA + Header.ImageBase, ObjectTable[i].VirtualSize, ObjectTable[i].Name, self);
    Sections.Add(ImportSection);
  end;

  // Create Export section
  if InterestingRVAs.ExportTableRVA <> 0 then begin                // Spracovanie Exportu
    i := GetObjectNumberFromRVA(InterestingRVAs.ExportTableRVA);
    fExportSection := TExportSection.CreateFromPEFile(InputFile, ObjectTable[i].Offset + (InterestingRVAs.ExportTableRVA - ObjectTable[i].RVA), InterestingRVAs.ExportTableRVA, InterestingRVAs.ExportDataSize, header.ImageBase, ObjectTable[i].Name, self);
    Sections.Add(ExportSection);
  end;
{
  if InterestingRVAs.ResourceTableRVA <> 0 then begin
    i:= GetObjectNumberFromRVA(InterestingRVAs.ResourceTableRVA);
    Sections.Add( TResourceSection.Create(InputFile, ObjectTable[i].name, ObjectTable[i].offset, ObjectTable[i].size, ObjectTable[i].rva + header.ImageBase, ObjectTable[i].virtualsize, InterestingRVAs.ResourceTableRVA, i, self) );
  end;
}
  fExecFormat := ffPE;
end;



procedure TPEFile.SaveToFile(DHF: TStream; var DAS: TextFile);
var
  i: Integer;
begin
  inherited SaveToFile(DHF, DAS);
  DHF.Write(Header, SizeOf(Header));                                        // PE hlavicka
  for i := 0 to Header.ObjectCount - 1 do
    DHF.Write(fObjectTable[i], SizeOf(TObjectTableEntry) - 4);
end;



procedure TPEFile.LoadFromFile(DHF: TStream; var DAS: TextFile);
var
  i: Integer;
begin
  inherited LoadFromFile(DHF, DAS);
  DHF.Read(fHeader, SizeOf(TPEHeader));
  SetLength(fObjectTable, fHeader.ObjectCount);
  for i := 0 to fHeader.ObjectCount - 1 do begin                             // Object Table
    DHF.Read(fObjectTable[i], SizeOf(TObjectTableEntry) - 4);
    fObjectTable[i].Name := PChar(@fObjectTable[i].internal_name[1]);
  end;
end;



function TPEFile.GetSectionNumberFromRVA(RVA: Cardinal): Integer;
var
  i: Integer;
  Offset: Cardinal;
begin
  Offset := RVA + Header.ImageBase;
  for i := 0 to Sections.Count - 1 do
    if Sections[i].Typ = stCode then
      with Sections[i] as TCodeSection do
        if (Offset >= MemOffset) and (Offset < MemOffset + MemSize) then begin
          Result := i;
          Exit;
        end;
  Result := -1;
end;



function TPEFile.GetObjectTableEntry(Index: Integer): TObjectTableEntry;
begin
  if Index < Header.ObjectCount then
    Result := fObjectTable[Index]
  else
    raise EIllegalState.Create('Array Range check error in PEFile ObjectTable');
end;



function TPEFILE.IsObjectExecutable(ObjectIndex: Integer): Boolean;
begin
  Result := (c_ExecutableSection and ObjectTable[ObjectIndex].Flags) > 0;
end;



// Virtual addresses of objects should be in ascending order according to PE spec.
function TPEFile.GetObjectNumberFromRVA(RVA: Cardinal): Integer;
var
  ObjectIndex: Integer;
begin
  Result := -1;
  if fHeader.ObjectCount = 0 then
    Exit;
  if RVA < ObjectTable[0].RVA then
    Exit;

  ObjectIndex := fHeader.ObjectCount - 1;
  while RVA < ObjectTable[ObjectIndex].RVA do
    Dec(ObjectIndex);
  if RVA < ObjectTable[ObjectIndex].RVA + ObjectTable[ObjectIndex].VirtualSize then
    Result := ObjectIndex
  else
    Result := -1;
end;



function TPefile.GetCPUType: TCPUType;
begin
  case Header.Machine of
    $014c: Result := cpu386;
    $014d: Result := cpu486;
    $014e: Result := cpu586;
    else
      Result := cpuUnknown;
  end;
end;



function TPEFIle.GetPEFileType: string;
begin
  if fHeader.Flags = 0 then
    Result := 'program'
  else begin
    if ($0002 and fHeader.Flags) = $0002 then
      Result := 'executable';
    if ($0200 and fHeader.Flags) = $0200 then
      Result := Result + ' fixed';
    if ($2000 and fHeader.Flags) = $2000 then
      Result := ' library';
  end;
  if Result = '' then
    Result := 'unknown';
end;



end.
