unit ELFFileUnit;

{$INCLUDE 'delver.inc'}

interface

uses
  SysUtils,
  Classes,

  procmat,
  StringRes,
  ExecFileUnit,
  SectionUnit,
  ImportSectionUnit,
  CodeSectionUnit;

const
  { sh_type  }
     SHT_NULL = 0;
     SHT_PROGBITS = 1;
     SHT_SYMTAB = 2;
     SHT_STRTAB = 3;
     SHT_RELA = 4;
     SHT_HASH = 5;
     SHT_DYNAMIC = 6;
     SHT_NOTE = 7;
     SHT_NOBITS = 8;
     SHT_REL = 9;
     SHT_SHLIB = 10;
     SHT_DYNSYM = 11;
     SHT_NUM = 12;
     SHT_LOPROC = $70000000;
     SHT_HIPROC = $7fffffff;
     SHT_LOUSER = $80000000;
     SHT_HIUSER = $ffffffff;
  { sh_flags  }
     SHF_WRITE = $1;
     SHF_ALLOC = $2;
     SHF_EXECINSTR = $4;
     SHF_MASKPROC = $f0000000;
  { special section indexes  }
     SHN_UNDEF = 0;
     SHN_LORESERVE = $ff00;
     SHN_LOPROC = $ff00;
     SHN_HIPROC = $ff1f;
     SHN_ABS = $fff1;
     SHN_COMMON = $fff2;
     SHN_HIRESERVE = $ffff;
  { These constants are for the segment types stored in the image headers  }
     PT_NULL = 0;
     PT_LOAD = 1;
     PT_DYNAMIC = 2;
     PT_INTERP = 3;
     PT_NOTE = 4;
     PT_SHLIB = 5;
     PT_PHDR = 6;
     PT_LOOS = $60000000;
     PT_HIOS = $6fffffff;
     PT_LOPROC = $70000000;
     PT_HIPROC = $7fffffff;
     PT_GNU_EH_FRAME = $6474e550;
  { This is the info that is needed to parse the dynamic section of the file  }
     DT_NULL = 0;
     DT_NEEDED = 1;
     DT_PLTRELSZ = 2;
     DT_PLTGOT = 3;
     DT_HASH = 4;
     DT_STRTAB = 5;
     DT_SYMTAB = 6;
     DT_RELA = 7;
     DT_RELASZ = 8;
     DT_RELAENT = 9;
     DT_STRSZ = 10;
     DT_SYMENT = 11;
     DT_INIT = 12;
     DT_FINI = 13;
     DT_SONAME = 14;
     DT_RPATH = 15;
     DT_SYMBOLIC = 16;
     DT_REL = 17;
     DT_RELSZ = 18;
     DT_RELENT = 19;
     DT_PLTREL = 20;
     DT_DEBUG = 21;
     DT_TEXTREL = 22;
     DT_JMPREL = 23;
     DT_LOPROC = $70000000;
     DT_HIPROC = $7fffffff;
  { This info is needed when parsing the symbol table  }
     STB_LOCAL = 0;
     STB_GLOBAL = 1;
     STB_WEAK = 2;
     STT_NOTYPE = 0;
     STT_OBJECT = 1;
     STT_FUNC = 2;
     STT_SECTION = 3;
     STT_FILE = 4;


type
  TELFHeader = record
    e_ident: array [0..15] of char;
    e_type: word;
    e_machine: word;
    e_version: cardinal;
    e_entry: cardinal;
    e_phoff: cardinal;
    e_shoff: cardinal;
    e_flags: cardinal;
    e_ehsize: word;
    e_phentsize: word;
    e_phnum: word;
    e_shentsize: word;
    e_shnum: word;
    e_shstrndx: word;
  end;


  TSectionHeaderTableEntry = record
    sh_name: cardinal;
    sh_type: cardinal;
    sh_flags: cardinal;
    sh_addr: cardinal;
    sh_offset: cardinal;
    sh_size: cardinal;
    sh_link: cardinal;
    sh_info: cardinal;
    sh_addraling: cardinal;
    sh_entsize: cardinal;
  end;

  TSectionHeader = record
    sh_name: cardinal;
    sh_type: cardinal;
    sh_flags: cardinal;
    sh_addr: cardinal;
    sh_offset: cardinal;
    sh_size: cardinal;
    sh_link: cardinal;
    sh_info: cardinal;
    sh_addraling: cardinal;
    sh_entsize: cardinal;
    name: string;
  end;

  TProgramHeader = record
    p_type: cardinal;
    p_offset: cardinal;
    p_vaddr: cardinal;
    p_paddr: cardinal;
    p_filesz: cardinal;
    p_memsz: cardinal;
    p_flags: cardinal;
    p_align: cardinal;
  end;

  TELFFile = class(TExecutableFile)
    header: TELFHeader;
    SectionHeaders: array of TSectionHeader;
    ProgramHeaders: array of TProgramHeader;
    constructor Create(a: TStream; aFileName: TFileName); overload; virtual;       // otvaranie suboru

//    function GetSectionFromOffset(Offset: cardinal): string; override;
//    function GetSectionNumberFromOffset(Offset: cardinal): integer; override;
//    function GetSectionOffsetFromOffset(Offset: cardinal): cardinal; override;
    function SaveToFile(var f: TextFile; a:TMemoryStream; SaveOptions: TSaveOptions):boolean; override;
    function LoadFromFile(var f:TextFile; a:TMemoryStream):boolean; override;

//    destructor Destroy; virtual;

  end;

implementation

constructor TELFFile.Create(a: TStream; aFileName: TFileName);       // otvaranie suboru
var i,StrTabIndex:integer;
    section: TCodeSection;
    imsection: TSection;
begin
  inherited;
  a.Position:=0;
  a.Read(header,SizeOf(TELFHeader));

  SetLength(ProgramHeaders,header.e_phnum);
  a.Position:=header.e_phoff;
  a.Read(ProgramHeaders[0],SizeOf(TProgramHeader)*header.e_phnum);
{  for i:=0 to header.e_phnum-1 do
    if ProgramHeaders[i].p_type = PT_INTERP then
      showmessage(inttostr(ProgramHeaders[i].p_filesz));
}
  SetLength(SectionHeaders,header.e_shnum);
  a.Position:=header.e_shoff;
  for i:=0 to header.e_shnum do a.Read(SectionHeaders[i],SizeOf(TSectionHeaderTableEntry));

  for i:=0 to header.e_shnum-1 do begin
// najdeme kodove sekcie
    if (SectionHeaders[i].sh_type = SHT_PROGBITS) then
      if SectionHeaders[i].sh_flags = (SHF_ALLOC or SHF_EXECINSTR) then begin
//        Inc(CodeSectionsCount);
////        SetLength(fSections,SectionCou+1);
////        section:= TCodeSection.Create(a,SectionHeaders[i].sh_offset,SectionHeaders[i].sh_size,SectionHeaders[i].sh_addr,SectionHeaders[i].sh_size,true,self);
////        section.SectionNumber:=CodeSectionsCount-1;
        if section.InSection(header.e_entry) then begin
          section.EntryPointAddress:=header.e_entry-Section.MemOffset;
        end;
//        fSections[i]:=section;
      end;

// najdeme sekciu dynamicky linkovanych symbolov
{
    if (SectionHeaders[i].sh_type = SHT_DYNSYM) then
      if SectionHeaders[i].sh_flags = SHF_ALLOC then begin
        SetLength(Sections,Length(Sections)+1);
        Sections[High(Sections)]:= TELFImportSection.Create(a,SectionHeaders[i].sh_offset,SectionHeaders[i].sh_size,SectionHeaders[SectionHeaders[i].sh_link].sh_offset,SectionHeaders[SectionHeaders[i].sh_link].sh_size,self,ctrls);
      end;
}      
  end;
// najdeme mena sekcii
  for i:=0 to header.e_shnum-1 do begin
//    SectionHeaders[e_shstrndx].sh_offset
    ReadStringFromStream(a, SectionHeaders[header.e_shstrndx].sh_offset + SectionHeaders[i].sh_name,SectionHeaders[i].name);
  end;



  EntryPoint:=header.e_entry;
  fFormatDescription:='ELF - Executable and Linkable Format';
  fExecFormat:=ELF;
end;


{
  for i:=0 to header.e_phnum-1 do Ctrls.Combo.Items.Add(inttostr(i+1)+'. ');
  setlength(Ctrls.PObjectTableData^,header.e_phnum);
  for i:=0 to header.e_phnum-1 do begin
    with Ctrls.PObjectTableData^[i] do begin
      virtualsize:=ProgramHeaders[i].p_memsz;
      rva:=ProgramHeaders[i].p_vaddr;
      size:=ProgramHeaders[i].p_filesz;
      offset:=ProgramHeaders[i].p_offset;
      flags:=ProgramHeaders[i].p_type;
    end;
  end;
}

{
function TELFFile.GetSectionFromOffset(Offset: cardinal): string;
var i,j:integer;
begin
  for i:=0 to header.e_shnum-1 do
    if (Offset >= SectionHeaders[i].sh_offset) and (Offset < SectionHeaders[i].sh_offset + SectionHeaders[i].sh_size) then begin
      result:= Trim(SectionHeaders[i].name);

      for j:=0 to SectionCount-1 do
        if Sections[j] is TCodeSection then
          if (Sections[j] as TCodeSection).InSection(SectionHeaders[i].sh_addr) then result:=result+' - '+CodeSectionStr+IntToStr((Sections[j] as TCodeSection).SectionNumber);
      Exit;
    end;
  result:='not section';
end;

function TELFFile.GetSectionNumberFromOffset(Offset: cardinal): integer;
var i:integer;
begin
// dorobit kompatibilitu s definiciou tejto funkcie v ExecFileUnit
  result:=-1;
  for i:=0 to header.e_shnum-1 do
    if (Offset >= SectionHeaders[i].sh_offset) and (Offset < SectionHeaders[i].sh_offset + SectionHeaders[i].sh_size) then begin
      result:=i;
      break;
    end;
end;

function TELFFile.GetSectionOffsetFromOffset(Offset: cardinal): cardinal;
var index: integer;
begin
  Index:=GetSectionNumberFromOffset(Offset);
  if Index <> -1 then Result:=Offset - SectionHeaders[Index].sh_offset
  else result:=$FFFFFFFF;
end;
}

function TELFFile.SaveToFile(var f: TextFile; a:TMemoryStream; SaveOptions: TSaveOptions):boolean;
var i:integer;
begin
  if soProject in SaveOptions then begin
    a.Write(header,SizeOf(TELFHeader));                                        // PE hlavicka
//    a.Write(header.e_shnum,2);                                                // pocet sekcii
    for i:=0 to header.e_shnum-1 do begin
      a.Write(SectionHeaders[i],SizeOf(TSectionHeaderTableEntry));
      a.Write(pchar(SectionHeaders[i].name)^,Length(SectionHeaders[i].name)+1);
    end;  
  end;
  result:=inherited SaveToFile(f,a,SaveOptions);
end;

function TELFFile.LoadFromFile(var f:TextFile; a:TMemoryStream):boolean;
var i: integer;
    SectionType: TSectionType;
begin
  a.Read(header,SizeOf(TELFHeader));
//  a.Read(objectcount,4);
  SetLength(SectionHeaders,header.e_shnum);
  for i:=0 to header.e_shnum-1 do begin
    a.Read(SectionHeaders[i],SizeOf(TSectionHeaderTableEntry));
    ReadStringFromStream(a,a.Position,SectionHeaders[i].name);
  end;
  result:=inherited LoadFromFile(f,a);
end;


end.
