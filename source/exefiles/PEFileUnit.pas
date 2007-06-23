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
  ExportSectionUnit,
  DummySectionUnit,
  ResourceSectionUnit;

const
  c_ExecutableSection =  $20000000;

  c_PEFileAdvancedInfoCount = 12;

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
    res7a:cardinal;
    Imagesize, HeaderSize: cardinal;
    filechecksum:cardinal;
    subsystem,dllflags:word;
    stackreservesize,stackcommitsize:cardinal;
    heapreservesize,heapcommitsize:cardinal;
    res8a: cardinal;
    InterestingCount: cardinal;
  end;

  TInterestingRVA = record
    ExportTableRVA,      ExportDataSize: cardinal;
    ImportTableRVA,      ImportDataSize: cardinal;
    ResourceTableRVA,    ResourceDataSize: cardinal;
    ExceptionTableRVA,   ExceptionDataSize: cardinal;
    SecurityTableRVA,    SecurityDataSize: cardinal;
    FixupTableRVA,       FixupDataSize: cardinal;
    DebugTableRVA,       DebugDataSize: cardinal;
    ImageDescriptionRVA, DescriptionSize: cardinal;
    MachineSpecificRVA,  MachineSpecificSize: cardinal;
    TLSRVA,              TLSSize: cardinal;                 //Thread local storage
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


  TPeFile = class(TExecutableFile)
  private
    Header: TPEHeader;
    InterRVA: TInterestingRVA;
    ObjectTable: array of TObjectTableEntry;

//    function GetObjectTableEntry(Index: integer): TObjectTableEntry;
    function GetCPUType: string;
    function GetPEFileType(peflags:word):string;

    function IsObjectExecutable(ObjectIndex: integer): boolean;
    function GetObjectNumberFromRVA(RVA: cardinal): integer;

  public
    constructor Create(InputFile: TStream; aFileName: TFileName); overload; override;
    destructor Destroy; override;

    function SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean; override;
    function LoadFromFile(DHF: TStream; var DAS: TextFile): boolean; override;

    function GetSectionNumberFromRVA(RVA: cardinal): integer;
  end;



implementation



destructor TPEFile.Destroy;
begin
  inherited;
end;



function TPEFile.GetSectionNumberFromRVA(RVA: cardinal): integer;
var
  i: integer;
  Offset: cardinal;
begin
  Offset:= RVA + Header.ImageBase;
  for i:= 0 to Sections.Count - 1 do
    if Sections[i].Typ = stCode then
      with Sections[i] as TCodeSection do
        if (Offset >= MemOffset) and (Offset < MemOffset + MemSize) then begin
          result:= i;
          Exit;
        end;
  result:=-1;
end;



constructor TPEFile.Create(InputFile: TStream; aFileName: TFileName);
var
  i: integer;
  PEHeaderOffset: cardinal;
  CodeSection: TCodeSection;
  ObjectIndex: integer;
begin
  inherited;

  // Seek and read PE header
  InputFile.Seek(60, 0);
  InputFile.Read(PEHeaderOffset, 4);
  InputFile.Seek(PEHeaderOffset, 0);
  InputFile.Read(header, SizeOf(TPEHeader));

  // Read Interesting RVA/Sizes table
  InputFile.Read(interRVA, 4*(header.interestingcount));

  // Read Object Table
  InputFile.Seek(PEHeaderOffset + 248, 0);
  SetLength(ObjectTable, header.objectcount);
  for i:=0 to header.objectcount-1 do begin
    InputFile.Read(ObjectTable[i], 40);
    ObjectTable[i].name:= pchar(@ObjectTable[i].internal_name[1]);
  end;

  // Set file regions
  fRegions.Add('MZ header', 0, 60, -1);
  fRegions.Add('PE header', 60, header.HeaderSize, -1);
  for ObjectIndex:= 0 to header.ObjectCount - 1 do begin
    with ObjectTable[ObjectIndex] do
      if size > 0 then
        fRegions.Add(Name, Offset, Size, 0);
  end;

  // Create code sections
  for i:=0 to header.objectcount-1 do begin
    if IsObjectExecutable(i) then begin
      CodeSection:= TCodeSection.Create(InputFile, true, ObjectTable[i].offset, ObjectTable[i].size, ObjectTable[i].rva + header.ImageBase, ObjectTable[i].virtualsize, fCodeSectionsCount, ObjectTable[i].Name, self);
      Inc(fCodeSectionsCount);
// treba pridat do constructora
//      CodeSection.CodeSectionIndex:=CodeSectionsCount-1;
      if CodeSection.InSection(Header.EntryPoint + Header.ImageBase) then
        CodeSection.EntryPointAddress:=header.EntryPoint + header.ImageBase - (ObjectTable[i].rva + header.ImageBase);

      Sections.Add(CodeSection);
    end;
  end;

  // Create Import section
  if InterRVA.ImportTableRVA <> 0 then begin
    i:=GetObjectNumberFromRVA(interrva.ImportTableRVA);
    fImportSection:= TImportSection.CreateFromPEFile(InputFile, InterRVA.ImportTableRVA, header.ImageBase, ObjectTable[i].offset, ObjectTable[i].size, ObjectTable[i].rva + header.ImageBase, ObjectTable[i].virtualsize, ObjectTable[i].name, self);

    if Assigned(OnExecFileCreateSection) then
      OnExecFileCreateSection(ImportSection);
    Sections.Add(ImportSection);

    // Set Import for all code sections
    for i:=0 to Sections.Count-1 do
      if Sections[i].Typ = stCode then
        (Sections[i] as TCodeSection).Import:=ImportSection;
  end;

  // Create Export section
  if InterRVA.ExportTableRVA <> 0 then begin                // Spracovanie Exportu
    i:=GetObjectNumberFromRVA(InterRVA.ExportTableRVA);
    fExportSection:= TExportSection.CreateFromPEFile(InputFile, ObjectTable[i].Offset + (InterRVA.ExportTableRVA - ObjectTable[i].RVA), InterRVA.ExportTableRVA, InterRVA.ExportDataSize, header.ImageBase, ObjectTable[i].name, self);

    if Assigned(OnExecFileCreateSection) then
      OnExecFileCreateSection(ExportSection);
    Sections.Add(ExportSection);

    // Set Export for all code sections
    for i:=0 to Sections.Count-1 do
      if Sections[i].Typ = stCode then
        (Sections[i] as TCodeSection).Exportt:=ExportSection;
  end;


  if interrva.ResourceTableRVA <> 0 then begin
    i:=GetObjectNumberFromRVA(interrva.ResourceTableRVA);
  //  Sections.Add( TResourceSection.Create(InputFile, ObjectTable[i].name, ObjectTable[i].offset, ObjectTable[i].size, ObjectTable[i].rva + header.ImageBase, ObjectTable[i].virtualsize, interrva.ResourceTableRVA, i, self) );
  end;

  EntryPoint:=header.entrypoint;
  fFormatDescription:='PE - Portable Executable (32-bit)';
  fExecFormat:=ffPE;
end;



function TPefile.GetCPUType: string;
begin
  case Header.Machine of
    $014c: result:= '80386';
    $014d: result:= '80486';
    $014e: result:= '80586';
  end;
end;



function TPEFIle.GetPEFileType(peflags:word):string;
begin
  if peflags = 0 then result:='program'
  else begin
    if ($0002 and peflags) = $0002 then result:='executable';
    if ($0200 and peflags) = $0200 then result:=result + ' fixed';
    if ($2000 and peflags) = $2000 then result:=' library';
  end;
  if result='' then result:='unknown';
end;



function TPEFile.SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean;
var
  i: integer;
begin
  result:= inherited SaveToFile(DHF, DAS, SaveOptions);
  if soProject in SaveOptions then begin
    DHF.Write(Header, SizeOf(Header));                                        // PE hlavicka
    for i:=0 to Header.ObjectCount - 1 do
      DHF.Write(ObjectTable[i], SizeOf(TObjectTableEntry) - 4);
  end;
end;



function TPEFile.LoadFromFile(DHF: TStream; var DAS: TextFile): boolean;
var i: integer;
begin
  result:=inherited LoadFromFile(DHF, DAS);
  DHF.Read(header, SizeOf(header));
  SetLength(ObjectTable, header.ObjectCount);
  for i:=0 to header.ObjectCount-1 do begin                             // Object Table
    DHF.Read(ObjectTable[i], SizeOf(TObjectTableEntry) - 4);
    ObjectTable[i].name:= pchar(@ObjectTable[i].internal_name[1]);
  end;
end;

{
function TPEFile.GetObjectTableEntry(Index: integer): TObjectTableEntry;
begin
  if Index < SectionCount then
    result:=fObjectTable[Index]
  else
    raise Exception.Create('Array Range check error in PEFile ObjectTable');
end;
}


{
function TPeFile.GetAdvancedInfo: TExecFileAdvancedInfo;
begin
  result:=TExecFileAdvancedInfo.Create(c_PEFileAdvancedInfoCount);
  result.Add('', 'Image base:', IntToHex(header.imagebase,8));
  result.Add('', 'Image size:', IntToHex(header.imagesize,8));
  result.Add('', 'Header size: ', IntToHex(header.headersize,8));
  result.Add('', 'Entry point RVA:', IntToHex(header.entrypoint,8));
  result.Add('', '', '');
  result.Add('', 'Time - Date stamp', IntToHex(header.timedate,8));
  result.Add('', 'Flags: ', IntToHex(header.flags,4) + ' = ' + GetPEFileType(header.flags));
  result.Add('', 'CPU type required: ', GetCPUType);
  result.Add('', '', '');
  result.Add('', 'Object align: ', IntToHex(header.objectalign,8));
  result.Add('', 'File align: ', IntToHex(header.filealign,8));
  result.Add('', 'File checksum: ', IntToHex(header.filechecksum,8));
end;
}





function TPEFILE.IsObjectExecutable(ObjectIndex: integer): boolean;
begin
  result:= (c_ExecutableSection and ObjectTable[ObjectIndex].flags) > 0
end;



// Virtual addresses of objects should be in ascending order according to PE spec.
function TPEFile.GetObjectNumberFromRVA(RVA: cardinal): integer;
var
  ObjectIndex: integer;
begin
  result:= -1;
  if Header.ObjectCount = 0 then Exit;
  if RVA < ObjectTable[0].RVA then Exit;

  ObjectIndex:= Header.ObjectCount - 1;
  while RVA < ObjectTable[ObjectIndex].RVA do
    Dec(ObjectIndex);
  if RVA < ObjectTable[ObjectIndex].RVA + ObjectTable[ObjectIndex].VirtualSize then
    result:= ObjectIndex
  else
    result:= -1
end;



end.
