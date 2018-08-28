unit ELFFileUnit;

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


  EI_CLASS = 4;
  EI_DATA = 5;

  ELFCLASS32 = 1;

  ELFDATA2LSB = 1;

type
  TELFHeader = record
    e_ident: array [0..15] of Char; // ELF Identification
    e_type: Word;
    e_machine: Word;
    e_version: Cardinal;
    e_entry: Cardinal;
    e_phoff: Cardinal;
    e_shoff: Cardinal; // File offset of Section Header Table
    e_flags: Cardinal;
    e_ehsize: Word;
    e_phentsize: Word;
    e_phnum: Word;
    e_shentsize: Word;
    e_shnum: Word; // number of sections (section headers)
    e_shstrndx: Word;
  end;


  TSectionHeaderTableEntry = record
    sh_name: Cardinal;
    sh_type: Cardinal;
    sh_flags: Cardinal;
    sh_addr: Cardinal;
    sh_offset: Cardinal;
    sh_size: Cardinal;
    sh_link: Cardinal;
    sh_info: Cardinal;
    sh_addraling: Cardinal;
    sh_entsize: Cardinal;
  end;

  TSectionHeader = record
    sh_name: Cardinal;
    sh_type: Cardinal;
    sh_flags: Cardinal;
    sh_addr: Cardinal;
    sh_offset: Cardinal;
    sh_size: Cardinal;
    sh_link: Cardinal;
    sh_info: Cardinal;
    sh_addraling: Cardinal;
    sh_entsize: Cardinal;
    Name: string;
  end;

  TProgramHeader = record
    p_type: Cardinal;
    p_offset: Cardinal;
    p_vaddr: Cardinal;
    p_paddr: Cardinal;
    p_filesz: Cardinal;
    p_memsz: Cardinal;
    p_flags: Cardinal;
    p_align: Cardinal;
  end;

  TELFFile = class(TExecutableFile)
  private
    Header: TELFHeader;
    SectionHeaders: array of TSectionHeader;
    ProgramHeaders: array of TProgramHeader;
  public
    constructor Create; overload; override;
    constructor Create(InputFile: TStream; aFileName: TFileName); overload; override;

    procedure SaveToFile(DHF: TStream; var DAS: TextFile); override;
    procedure LoadFromFile(DHF: TStream; var DAS: TextFile); override;
  end;


implementation



constructor TELFFile.Create;
begin
  inherited;
  fExecFormat := ffELF;
end;



constructor TELFFile.Create(InputFile: TStream; aFileName: TFileName);
var
  i: Integer;
  CodeSection: TCodeSection;
begin
  inherited;
  InputFile.Position := 0;
  InputFile.Read(Header, SizeOf(TELFHeader));

  // check if file is 32
  if Ord(Header.e_ident[EI_CLASS]) <> ELFCLASS32 then
    raise Exception.Create('Format error');

  // check if file is Little Endian
  if Ord(Header.e_ident[EI_DATA]) <> ELFDATA2LSB then
    raise Exception.Create('Format error');


  // Read Program header table
  SetLength(ProgramHeaders, Header.e_phnum);
  InputFile.Position := Header.e_phoff;
  InputFile.Read(ProgramHeaders[0], SizeOf(TProgramHeader) * Header.e_phnum);
{  for i:=0 to header.e_phnum-1 do
    if ProgramHeaders[i].p_type = PT_INTERP then
      showmessage(inttostr(ProgramHeaders[i].p_filesz));
}

  // Read Section header table
  SetLength(SectionHeaders, Header.e_shnum);
  InputFile.Position := Header.e_shoff;
  for i := 0 to header.e_shnum - 1 do
    InputFile.Read(SectionHeaders[i], SizeOf(TSectionHeaderTableEntry));

  for i := 0 to header.e_shnum - 1 do begin
    ReadStringFromStream(InputFile, SectionHeaders[Header.e_shstrndx].sh_offset + SectionHeaders[i].sh_name, SectionHeaders[i].Name);

    // Create code sections
    if (SectionHeaders[i].sh_type = SHT_PROGBITS) then
      if SectionHeaders[i].sh_flags = (SHF_ALLOC or SHF_EXECINSTR) then begin
        CodeSection := TCodeSection.Create(InputFile, True, SectionHeaders[i].sh_offset, SectionHeaders[i].sh_size, SectionHeaders[i].sh_addr, SectionHeaders[i].sh_size, fCodeSectionsCount, SectionHeaders[i].Name, self);
        Inc(fCodeSectionsCount);
        if CodeSection.IsInSection(Header.e_entry) then begin
          CodeSection.EntryPointAddress := Header.e_entry - CodeSection.MemOffset;
        end;
        Sections.Add(CodeSection);
      end;

    // Create Import section
    if (SectionHeaders[i].sh_type = SHT_DYNSYM) and (SectionHeaders[i].sh_flags = SHF_ALLOC) then begin
      fImportSection := TImportSection.CreateFromELFFile(InputFile, SectionHeaders[i].sh_offset, SectionHeaders[i].sh_size, SectionHeaders[SectionHeaders[i].sh_link].sh_offset, SectionHeaders[SectionHeaders[i].sh_link].sh_size, SectionHeaders[i].Name, self);
      Sections.Add(ImportSection);
      if Assigned(OnExecFileCreateSection) then
        OnExecFileCreateSection(ImportSection);
    end;

  end;

  fExecFormat := ffELF;
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


procedure TELFFile.SaveToFile(DHF: TStream; var DAS: TextFile);
var
  i: Integer;
begin
  inherited SaveToFile(DHF, DAS);
  DHF.Write(Header, SizeOf(TELFHeader));
  for i := 0 to header.e_shnum - 1 do begin
    DHF.Write(SectionHeaders[i], SizeOf(TSectionHeaderTableEntry));
    StreamWriteAnsiString(DHF, SectionHeaders[i].Name);
  end;
end;



procedure TELFFile.LoadFromFile(DHF: TStream; var DAS: TextFile);
var
  i: Integer;
begin
  inherited LoadFromFile(DHF, DAS);
  DHF.Read(Header, SizeOf(TELFHeader));
  SetLength(SectionHeaders, Header.e_shnum);
  for i := 0 to Header.e_shnum - 1 do begin
    DHF.Read(SectionHeaders[i], SizeOf(TSectionHeaderTableEntry));
    SectionHeaders[i].Name := StreamReadAnsiString(DHF);
  end;
end;



end.
