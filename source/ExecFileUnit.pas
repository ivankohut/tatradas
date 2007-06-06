{ TODO:

}

unit ExecFileUnit;

{$INCLUDE 'delver.inc'}

interface

uses
  Classes,
  IniFiles,
  SysUtils,

  procmat,
  SectionUnit,
  disassembler,
  CodeSectionUnit,
  ImportSectionUnit,
  ExportSectionUnit,
  ResourceSectionUnit;

type
  TExecFileCreateSectionEvent = procedure (ASection: TSection) of object;

var
  OnExecFileCreateSection: TExecFileCreateSectionEvent;

type
  TMZHeader = record
    Sign:word;
    PageRemainder:word;      // page = 512B
    PageCount:word;
    RelocCount:word;
    HeaderSize:word;
    MinMem, MaxMem:word;
    reloSS:word;
    EXESP:word;
    ChkSum:word;
    EXEIP:word;
    reloCS:word;
    RelocTableOffset:word;
    Overlay:word;
    reserved:array [1..9]of cardinal;
  end;



  TExecutableFile = class
   private
    fFullPath: TFileName;
    fFileName: TFileName;
    fFileSize: cardinal;
    fIsDisassembled: boolean;

   protected
    fSections: TSections;
    fFormatDescription: string;
    fExecFormat: TExecFileFormat;

   public
    EntryPoint: cardinal;                                    // adresa Entry Pointu z hlavicky suboru
    EntryPointOffset: cardinal;                              // adresa Entry Pointu v subore
    EntryPointCodeAddress: cardinal;                         // adresa Entry Pointu v kode

    ImportSectionIndex: integer;
    ExportSectionIndex: integer;
    ImportSection: TImportSection;
    ExportSection: TExportSection;

    constructor Create; overload; virtual;
    constructor Create(a: TStream; aFileName:TFileName); overload; virtual;       // otvaranie suboru

    destructor Destroy; override;

    // preferred
    function SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean; overload; virtual;

    // deprecated
{
    function LoadFromFile(DHF: TFileStream; DAS: TTextFileStream):boolean; overload; virtual;
    function LoadFromFile(var f: TextFile; a:TMemoryStream):boolean; overload; virtual;
    function LoadFromFileEx(var f: TextFile; a:TMemoryStream):boolean; virtual;
}
    // preferred
    function LoadFromFile(DHF: TStream; var DAS: TextFile): boolean; virtual;

    function Disassemble():boolean; virtual;

    property FileName: TFileName read fFileName;
    property FullPath: TFileName read fFullPath;
    property FileSize: cardinal read fFileSize;
    property FormatDescription: string read fFormatDescription;
    property ExeFormat: TExecFileFormat read fExecFormat;

    property Sections: TSections read fSections;
    property IsDisassembled: boolean read fIsDisassembled;
  end;



implementation



constructor TExecutableFile.Create;
begin
  fSections:= TSections.Create;
end;



constructor TExecutableFile.Create(a: TStream; aFileName:TFileName);
begin
  fFileSize:=a.Size;
  fFullPath:=aFileName;
  fFileName:=ExtractFileName(aFileName);
  fSections:= TSections.Create;
end;



destructor TExecutableFile.Destroy;
begin
  Sections.Free;
end;



function TExecutableFile.SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean;
var
  SectionIndex: integer;
  StreamSectionCount: integer;
begin
  result:=false;

  // Save as project
  if soProject in SaveOptions then begin

    DHF.Write(fFileSize, 4);
    StreamWriteAnsiString(DHF, fFullPath);

    StreamSectionCount:=Sections.Count;
    DHF.Write(StreamSectionCount, 4);
    for SectionIndex:=0 to Sections.Count-1 do begin
      DHF.Write(Sections[SectionIndex].typ, sizeof(TSectionType));
      if not Sections[SectionIndex].SaveToFile(DHF, DAS, SaveOptions) then Exit;
    end;
  end

  // Save disassembled code sections
  else
    for SectionIndex:=0 to Sections.Count-1 do
      if Sections[SectionIndex].typ = stCode then
        if not Sections[SectionIndex].SaveToFile(DHF, DAS, SaveOptions) then
          Exit;

  result:=true;
end;


{
function TExecutableFile.LoadFromFile(var f: TextFile; a:TMemoryStream):boolean;
var i:integer;
    SectionType: TSectionType;
    StreamSectionCount: integer;
    Section: TSection;
begin
  result:=false;
  a.Read(StreamSectionCount,4);
  for i:=0 to StreamSectionCount-1 do begin
    a.Read(SectionType,sizeOf(TSectionType));
    case SectionType of
      stCode: begin
        Section:=TCodeSection.Create(self);
        //inc(CodeSectionsCount);
      end;
      stImport: begin
        Section:=TImportSection.Create(self);
        ImportSection:=Section as TImportSection;
      end;
      stExport: begin
        Section:=TExportSection.Create(self);
        ExportSection:=Sections[i] as TExportSection;
      end;
      stResource: begin
        Section:=TResourceSection.Create(self);
      end;
    end;
    if not Section.LoadFromFile(f,a) then
      Exit;
    if Assigned(OnExecFileCreateSection) then
      OnExecFileCreateSection(Section);
    Sections.Add(Section);
  end;
  for i:=0 to Sections.Count-1 do begin
    if Sections[i].typ = stCode then begin
      (Sections[i] as TCodeSection).Exportt:=ExportSection;
      (Sections[i] as TCodeSection).Import:=ImportSection;
    end;
  end;
//  Status:=tsProject;
  result:=true;
end;



function TExecutableFile.LoadFromFileEx(var f: TextFile; a:TMemoryStream):boolean;
var i:integer;
    SectionType: TSectionType;
    StreamSectionCount: integer;
    Section: TSection;

begin
  result:=false;
  a.Read(fFileSize,4);
  ReadStringFromStream(a,a.Position,string(fFullPath));
  fFileName:=ExtractFileName(fFullpath);

  a.Read(StreamSectionCount,4);                                     // Sections
  for i:=0 to StreamSectionCount-1 do begin
    a.Read(SectionType,sizeOf(TSectionType));
    case SectionType of
      stCode: begin
        Section:=TCodeSection.Create(self);
        //inc(CodeSectionsCount);
      end;
      stImport: begin
        Section:=TImportSection.Create(self);
        ImportSection:=Section as TImportSection;
      end;
      stExport: begin
        Section:=TExportSection.Create(self);
        ExportSection:=Sections[i] as TExportSection;
      end;
      stResource: begin
        Section:=TResourceSection.Create(self);
      end;
    end;
    if not Section.LoadFromFile(f,a) then
      Exit;
    if Assigned(OnExecFileCreateSection) then
      OnExecFileCreateSection(Section);
    Sections.Add(Section);
  end;
  for i:=0 to Sections.Count-1 do begin
    if Sections[i].typ = stCode then begin
      (Sections[i] as TCodeSection).Exportt:=ExportSection;
      (Sections[i] as TCodeSection).Import:=ImportSection;
    end;
  end;
  fIsDisassembled:=true;
//  Status:=tsProject;
  result:=true;
end;
}


function TExecutableFile.Disassemble():boolean;
var i,j: integer;
    Options: TDisassembleOptions;
begin
  result:=false;

  // Reset code section if file is already disassembled 
  if IsDisassembled then
    for i:=0 to Sections.Count-1 do
      if Sections[i].Typ = stCode then
        (Sections[i] as TCodeSection).ClearDisassembled;

  fIsDisassembled:=false;
//  Status:=tsOpened;
  // Nastavime parametre a disassemblujme
  for i:=0 to Sections.Count-1 do
    if Sections[i].Typ = stCode then
      with Sections[i] as TCodeSection do begin
        Options.address:=EntryPointAddress;
        Options.size:=CodeSize;
        Options.bit32:=Bit32;
        if not DisassembleAll(Options) then begin
          for j:=0 to i do
            if Sections[j] is TCodeSection then (Sections[j] as TCodeSection).ClearDisassembled;
          Exit;
        end;
        if Assigned(OnExecFileCreateSection) then
          OnExecFileCreateSection(Sections[i]);
      end;
  fIsDisassembled:=true;
//  Status:=tsDisassembled;
  result:=true;
end;


{
function TExecutableFile.LoadFromFile(DHF: TFileStream; DAS: TTextFileStream): boolean;
var i:integer;
    SectionType: TSectionType;
    StreamSectionCount: integer;
    Section: TSection;
begin
  result:=false;
  DHF.Read(StreamSectionCount,4);                                     // Sections
  for i:=0 to StreamSectionCount-1 do begin
    DHF.Read(SectionType,sizeOf(TSectionType));
    case SectionType of
      stCode: begin
        Section:=TCodeSection.Create(self);
        //inc(CodeSectionsCount);
      end;
      stImport: begin
        Section:=TImportSection.Create(self);
        ImportSection:=Section as TImportSection;
      end;
      stExport: begin
        Section:=TExportSection.Create(self);
        ExportSection:=Sections[i] as TExportSection;
      end;
      stResource: begin
        Section:=TResourceSection.Create(self);
      end;
    end;
    if not Section.LoadFromFile(DHF,DAS) then
      Exit;
    if Assigned(OnExecFileCreateSection) then
      OnExecFileCreateSection(Section);
    Sections.Add(Section);
  end;
  for i:=0 to Sections.Count-1 do begin
    if Sections[i].typ = stCode then begin
      (Sections[i] as TCodeSection).Exportt:=ExportSection;
      (Sections[i] as TCodeSection).Import:=ImportSection;
    end;
  end;
  fIsDisassembled:=true;
//  Status:=tsProject;
  result:=true;
end;
}



function TExecutableFile.LoadFromFile(DHF: TStream; var DAS: TextFile): boolean;
var i:integer;
    SectionType: TSectionType;
    StreamSectionCount: integer;
    Section: TSection;
begin
  result:=false;

  DHF.Read(fFileSize, 4);
  fFullPath:= StreamReadAnsiString(DHF);
  fFileName:= ExtractFileName(fFullpath);

  DHF.Read(StreamSectionCount,4);                                     // Sections
  for i:=0 to StreamSectionCount-1 do begin
    DHF.Read(SectionType,sizeOf(TSectionType));
    case SectionType of
      stCode: begin
        Section:=TCodeSection.Create(self);
        //inc(CodeSectionsCount);
      end;
      stImport: begin
        Section:=TImportSection.Create(self);
        ImportSection:=Section as TImportSection;
      end;
      stExport: begin
        Section:=TExportSection.Create(self);
        ExportSection:=Sections[i] as TExportSection;
      end;
      stResource: begin
        Section:=TResourceSection.Create(self);
      end;
    end;
    if not Section.LoadFromFile(DHF, DAS) then
      Exit;
    if Assigned(OnExecFileCreateSection) then
      OnExecFileCreateSection(Section);
    Sections.Add(Section);
  end;
  for i:=0 to Sections.Count-1 do begin
    if Sections[i].typ = stCode then begin
      (Sections[i] as TCodeSection).Exportt:=ExportSection;
      (Sections[i] as TCodeSection).Import:=ImportSection;
    end;
  end;
  fIsDisassembled:=true;
//  Status:=tsProject;
  result:=true;
end;


end.
