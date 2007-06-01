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
    sign: cardinal;
    cpu: word;
    objectcount: word;
    timedate: cardinal;
    res1a: cardinal;
    res2a: cardinal;
    NTHDRsize: word;
    flags: word;
    res1b: word;
    LMajor, LMinor: byte;
//    res2b:word;  - velke zistenie, vdaka freepascalu
    res2b: cardinal;
    res3a: cardinal;
    res4a: cardinal;
    entrypoint:cardinal;
    res5a: cardinal;
    res6a: cardinal;
    imagebase:cardinal;
    objectalign, filealign:cardinal;
    OsMajor, OsMinor: word;
    UserMajor, UserMinor:word;
    SubSysMajor, SubSysMinor: word;
    res7a:cardinal;
    Imagesize, HeaderSize: cardinal;
    filechecksum:cardinal;
    subsystem,dllflags:word;
    stackreservesize,stackcommitsize:cardinal;
    heapreservesize,heapcommitsize:cardinal;
    res8a:cardinal;
    interestingcount:cardinal;
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
    name: array[1..8] of char;
    virtualsize,rva: cardinal;
    size,offset: cardinal;
    relocoffset,linenumberoffset: cardinal;
    reloccount,linenumbercount: word;
    flags: cardinal;
  end;


  TPeFile = class(TExecutableFile)
  private

    Header: TPEHeader;
    InterRVA: TInterestingRVA;
    ObjectTable: array of TObjectTableEntry;

//    function GetObjectTableEntry(Index: integer): TObjectTableEntry;
    function GetCPUType: string;
    function GetPEFileType(peflags:word):string;
//    property ObjectTable[Index: integer]: TObjectTableEntry read GetObjectTableEntry;

    function IsObjectExecutable(ObjectIndex: integer): boolean;
    function GetObjectNumberFromRVA(a:cardinal): cardinal;


  public
    constructor Create(a: TStream; aFileName: TFileName); overload; override;
    destructor Destroy; override;

    function SaveToFile(var f: TextFile; a:TMemoryStream; SaveOptions: TSaveOptions):boolean; override;
    function LoadFromFile(var f:TextFile; a:TMemoryStream):boolean; overload; override;
    function LoadFromFile(DHF: TFileStream; DAS: TTextFileStream): boolean; overload; override;

    function GetSectionNumberFromRVA(RVA: cardinal): integer;

  end;


  
implementation



destructor TPEFile.Destroy;
begin
  inherited;
end;



function TPEFile.GetSectionNumberFromRVA(RVA: cardinal): integer;
begin
  result:=-1;
  repeat
    inc(result);
  until RVA < (ObjectTable[result].rva + ObjectTable[result].size);
end;



constructor TPEFile.Create(a: TStream; aFileName: TFileName);
var
  i: integer;
  PEHeaderOffset: cardinal;
  CodeSection: TCodeSection;
  CodeSectionsCount: integer;
begin
  inherited;

  // Seek and read PE header
  a.Seek(60, 0);
  a.Read(PEHeaderOffset, 4);
  a.Seek(PEHeaderOffset, 0);                               
  a.Read(header, SizeOf(TPEHeader));

  // Read Interesting RVA/Sizes table
  a.Read(interRVA, 4*(header.interestingcount));

  // Read Object Table
  a.Seek(PEHeaderOffset + 248, 0);
  SetLength(ObjectTable, header.objectcount);
  for i:=0 to header.objectcount-1 do
    a.Read(ObjectTable[i], 40);

  // Create code sections
  CodeSectionsCount:=0;
  for i:=0 to header.objectcount-1 do begin
    if IsObjectExecutable(i) then begin
      Inc(CodeSectionsCount);
      CodeSection:= TCodeSection.Create(a, true, ObjectTable[i].name, ObjectTable[i].offset, ObjectTable[i].size, ObjectTable[i].rva + header.ImageBase, ObjectTable[i].virtualsize, i, self);
      CodeSection.CodeSectionIndex:=CodeSectionsCount-1;
      if CodeSection.InSection(header.entrypoint + header.ImageBase) then
        CodeSection.EntryPointAddress:=header.EntryPoint + header.ImageBase - CodeSection.MemOffset;

      Sections.Add(CodeSection);
    end;
  end;

  // Create Import section
  if interrva.ImportTableRVA <> 0 then begin
    ImportSectionIndex:= Sections.Count;
    i:=GetObjectNumberFromRVA(interrva.ImportTableRVA);
    ImportSection:=TImportSection.CreateFromPEFile(a, interrva.ImportTableRVA, header.ImageBase, ObjectTable[i].name, ObjectTable[i].offset, ObjectTable[i].size, ObjectTable[i].rva + header.ImageBase, ObjectTable[i].virtualsize, self);

    if Assigned(OnExecFileCreateSection) then
      OnExecFileCreateSection(ImportSection);
    Sections.Add(ImportSection);

    // Set Import for all code sections
    for i:=0 to Sections.Count-1 do
      if Sections[i].Typ = stCode then
        (Sections[i] as TCodeSection).Import:=ImportSection;
  end;

  // Create Export section
  if interrva.ExportTableRVA <> 0 then begin                // Spracovanie Exportu
    ExportSectionIndex:= Sections.Count;;
    i:=GetObjectNumberFromRVA(interrva.ExportTableRVA);
    ExportSection:=TPEExportSection.Create(a, interrva.ExportTableRVA, header.ImageBase, ObjectTable[i].name, ObjectTable[i].Offset, ObjectTable[i].size, ObjectTable[i].rva + header.ImageBase, ObjectTable[i].virtualsize, i, self);

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
  //  Sections.Add( TResourceSection.Create(a, ObjectTable[i].name, ObjectTable[i].offset, ObjectTable[i].size, ObjectTable[i].rva + header.ImageBase, ObjectTable[i].virtualsize, interrva.ResourceTableRVA, i, self) );
  end;

  EntryPoint:=header.entrypoint;
  fFormatDescription:='PE - Portable Executable (32-bit)';
  fExecFormat:=PE;
end;



function TPefile.GetCPUType: string;
begin
  case header.cpu of
    $014c:result:='80386';
    $014d:result:='80486';
    $014e:result:='80586';
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



function TPEFile.SaveToFile(var f: TextFile; a:TMemoryStream; SaveOptions: TSaveOptions):boolean;
var
  i: integer;
begin
  if soProject in SaveOptions then begin
    a.Write(Header, SizeOf(Header));                                        // PE hlavicka
    for i:=0 to Header.ObjectCount - 1 do
      a.Write(ObjectTable[i], sizeof(TObjectTableEntry));
  end;
  result:=inherited SaveToFile(f, a, SaveOptions);
end;



function TPEFile.LoadFromFile(var f:TextFile; a:TMemoryStream):boolean;
var i: integer;
begin
  a.Read(header, SizeOf(Header));
  SetLength(ObjectTable, header.objectcount);
  for i:=0 to header.objectcount-1 do                                // Object Table
    a.Read(ObjectTable[i], sizeof(TObjectTableEntry));
  result:=inherited LoadFromFile(f, a);
end;



function TPEFile.LoadFromFile(DHF: TFileStream; DAS: TTextFileStream): boolean;
var i: integer;
begin
  DHF.Read(header, SizeOf(header));
  SetLength(ObjectTable, header.objectcount);
  for i:=0 to header.objectcount-1 do                                // Object Table
    DHF.Read(ObjectTable[i], sizeof(TObjectTableEntry));
  result:=inherited LoadFromFile(DHF, DAS);
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



function TPEFile.GetObjectNumberFromRVA(a:cardinal): cardinal;
var b:integer;
begin
  b:=-1;
  repeat
    inc(b);
  until a<(objecttable[b].rva+objecttable[b].size);
  result:=b;
end;



end.
