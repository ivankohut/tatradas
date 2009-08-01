unit PEFileUnit;

{$INCLUDE 'delver.inc'}

interface

uses
  SysUtils,
  Classes,

  procmat,
  StringRes,
  ExecFileUnit,
  SectionUnit,
  CodeSectionUnit,
  ImportSectionUnit,
  ExportSectionUnit;

type

  TPEHeader = record
    Sign: cardinal;
    Machine: word;
    ObjectCount: word;
    TimeDateStamp: cardinal;
    res1a: cardinal;
    res2a: cardinal;
    NTHDRsize: word;
    Flags: word;
    res1b: word;
    LMajor, LMinor: byte;
//    res2b:word;  - velke zistenie, vdaka freepascalu
    res2b: cardinal;
    res3a: cardinal;
    res4a: cardinal;
    EntryPoint: cardinal;
    res5a: cardinal;
    res6a: cardinal;
    ImageBase:cardinal;
    ObjectAlign, FileAlign: cardinal;
    OsMajor, OsMinor: word;
    UserMajor, UserMinor:word;
    SubSysMajor, SubSysMinor: word;
    res7a: cardinal;
    Imagesize, HeaderSize: cardinal;
    FileCheckSum: cardinal;
    SubSystem,DLLFlags: word;
    StackReserveSize, StackCommitSize: cardinal;
    HeapReserveSize, HeapCommitSize: cardinal;
    res8a: cardinal;
    InterestingCount: cardinal;
  end;

  TInterestingRVAs = record
    ExportTableRVA,      ExportDataSize: cardinal;
    ImportTableRVA,      ImportDataSize: cardinal;
    ResourceTableRVA,    ResourceDataSize: cardinal;
    ExceptionTableRVA,   ExceptionDataSize: cardinal;
    SecurityTableRVA,    SecurityDataSize: cardinal;
    FixupTableRVA,       FixupDataSize: cardinal;
    DebugTableRVA,       DebugDataSize: cardinal;
    ImageDescriptionRVA, DescriptionSize: cardinal;
    MachineSpecificRVA,  MachineSpecificSize: cardinal;
    TLSRVA,              TLSSize: cardinal;
  end;

  TObjectTableEntry = record
    internal_name: array[1..8] of char;
    VirtualSize, RVA: cardinal;
    Size, Offset: cardinal;
    RelocOffset, LineNumberOffset: cardinal;
    RelocCount, LineNumberCount: word;
    Flags: cardinal;
    Name: string;
  end;

  TCPUType = (cpuUnknown, cpu386, cpu486, cpu586);

  TPeFile = class(TExecutableFile)
  private
    fHeader: TPEHeader;
    fObjectTable: array of TObjectTableEntry;

    function GetCPUType: TCPUType;
    function GetPEFileType: string;

    function IsObjectExecutable(ObjectIndex: integer): boolean;
    function GetObjectNumberFromRVA(RVA: cardinal): integer;
    function GetObjectTableEntry(Index: integer): TObjectTableEntry;

  // Common fields and methods
  public
    constructor Create; overload; override;
    constructor Create(InputFile: TStream; aFileName: TFileName); overload; override;
    destructor Destroy; override;
    procedure SaveToFile(DHF: TStream; var DAS: TextFile); override;
    procedure LoadFromFile(DHF: TStream; var DAS: TextFile); override;

  // Fields and methods specific to PE File
  public
    function GetSectionNumberFromRVA(RVA: cardinal): integer;
    property Header: TPEHeader read fHeader;
    property ObjectTable[Index: integer]: TObjectTableEntry read GetObjectTableEntry;
    property PEFileType: string read GetPEFileType;
    property CPUType: TCPUType read GetCPUType;
  end;


implementation


const
  c_ExecutableSection =  $20000000;


constructor TPEFile.Create;
begin
  inherited;
  fExecFormat:= ffPE;
end;



destructor TPEFile.Destroy;
begin
  inherited;
end;



constructor TPEFile.Create(InputFile: TStream; aFileName: TFileName);
var
  i: integer;
  PEHeaderOffset: cardinal;
  CodeSection: TCodeSection;
  ObjectIndex: integer;
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
    fObjectTable[i].Name := Pchar(@fObjectTable[i].internal_name[1]);
  end;

  // Set file regions
  fRegions.Add('MZ header', 0, SizeOf(TMZHeader));
  fRegions.Add('PE header', 60, header.HeaderSize);
  for ObjectIndex:= 0 to header.ObjectCount - 1 do begin
    with ObjectTable[ObjectIndex] do
      if size > 0 then
        fRegions.Add(Name, Offset, Size);
  end;
  fRegions.Finish;

  // Create code sections
  for i := 0 to Header.ObjectCount-1 do begin
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
    fExportSection := TExportSection.CreateFromPEFile(InputFile, ObjectTable[i].Offset + (InterestingRVAs.ExportTableRVA - ObjectTable[i].RVA), InterestingRVAs.ExportTableRVA, InterestingRVAs.ExportDataSize, header.ImageBase, ObjectTable[i].name, self);
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
    fObjectTable[i].Name := Pchar(@fObjectTable[i].internal_name[1]);
  end;
end;



function TPEFile.GetSectionNumberFromRVA(RVA: Cardinal): integer;
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
  Result := (c_ExecutableSection and ObjectTable[ObjectIndex].Flags) > 0
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
    Result := -1
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
    if ($0002 and fHeader.Flags) = $0002 then Result := 'executable';
    if ($0200 and fHeader.Flags) = $0200 then Result := Result + ' fixed';
    if ($2000 and fHeader.Flags) = $2000 then Result := ' library';
  end;
  if Result = '' then
    Result := 'unknown';
end;



end.
