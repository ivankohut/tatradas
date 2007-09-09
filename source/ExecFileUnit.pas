{ TODO:
  - zracionalizovat premenne ImportSection, ExportSection, ImportSectionIndex a ExportSectionIndex
  - CodeSectionsCount - zapisovat do projektu ?
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
  RegionsUnit,
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
    Sign: word;               // "MZ"
    PageRemainder: word;      // FileSize mod PageSize <=> FileSize mod 512
    PageCount: word;          // Ceil( FileSize/PageSize )
    RelocCount: word;         // Number of Relocations
    HeaderSize: word;
    MinMem, MaxMem: word;
    reloSS: word;
    EXESP: word;
    CheckSum: word;
    EXEIP: word;
    reloCS: word;
    RelocTableOffset: word;
    Overlay: word;
    reserved: array [1..9] of cardinal;
  end;


  TExecutableFile = class
   private
    fFullPath: TFileName;
    fFileName: TFileName;
    fFileSize: cardinal;
    fIsDisassembled: boolean;

   protected
    fRegions: TRegions;
    fSections: TSections;
    fExecFormat: TExecFileFormat;
    fCodeSectionsCount: integer;

    fImportSection: TImportSection;
    fExportSection: TExportSection;

   public
    constructor Create; overload; virtual;
    constructor Create(InputFile: TStream; aFileName: TFileName); overload; virtual;
    destructor Destroy; override;

    function Disassemble(): boolean; virtual;

    function SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean; overload; virtual;
    function LoadFromFile(DHF: TStream; var DAS: TextFile): boolean; virtual;

    property FileName: TFileName read fFileName;
    property FullPath: TFileName read fFullPath;
    property FileSize: cardinal read fFileSize;

    property ExeFormat: TExecFileFormat read fExecFormat;
    property Regions: TRegions read fRegions;
    property Sections: TSections read fSections;

    property ImportSection: TImportSection read fImportSection;
    property ExportSection: TExportSection read fExportSection;
    property IsDisassembled: boolean read fIsDisassembled;
  end;


implementation


//******************************************************************************
// TExecutableFile class
//******************************************************************************


constructor TExecutableFile.Create;
begin
  fSections:= TSections.Create;
end;



constructor TExecutableFile.Create(InputFile: TStream; aFileName: TFileName);
begin
  fFileSize:= InputFile.Size;
  fFullPath:= aFileName;
  fFileName:= ExtractFileName(aFileName);
  fSections:= TSections.Create;
  fRegions:= TRegions.Create(FileSize);
end;



destructor TExecutableFile.Destroy;
begin
  Sections.Free;
  Regions.Free;
end;



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

  // Nastavime parametre a disassemblujme
  for i:=0 to Sections.Count-1 do
    if Sections[i].Typ = stCode then
      with Sections[i] as TCodeSection do begin
        Options.Address:= EntryPointAddress;
        Options.Size:= CodeSize;
        Options.Bit32:= Bit32;
        if not DisassembleAll(Options) then begin
          for j:=0 to i do
            if Sections[j] is TCodeSection then (Sections[j] as TCodeSection).ClearDisassembled;
          Exit;
        end;
      end;
  fIsDisassembled:=true;
  result:=true;
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

    fRegions.SaveToFile(DHF);

    StreamSectionCount:=Sections.Count;
    DHF.Write(StreamSectionCount, 4);
    WriteLn(DAS, 'DisASsembled file, Original file: ' + FileName + '  ' + TatraDASFullNameVersion + ', Ivan Kohut (c) 2007');
    Writeln(DAS);
    for SectionIndex:=0 to Sections.Count-1 do begin
      DHF.Write(Sections[SectionIndex].typ, sizeof(TSectionType));
      if not Sections[SectionIndex].SaveToFile(DHF, DAS, SaveOptions) then
        Exit;
    end;
  end

  // Save disassembled code sections
  else begin
    WriteLn(DAS, 'DisASsembled file, Original file: ' + FileName + '  ' + TatraDASFullNameVersion + ', Ivan Kohut (c) 2007');
    Writeln(DAS);
    for SectionIndex:=0 to Sections.Count-1 do
      if Sections[SectionIndex].typ = stCode then
        if not Sections[SectionIndex].SaveToFile(DHF, DAS, SaveOptions) then
          Exit;
  end;
  result:=true;
end;



function TExecutableFile.LoadFromFile(DHF: TStream; var DAS: TextFile): boolean;
var i:integer;
    SectionType: TSectionType;
    StreamSectionCount: integer;
    Section: TSection;
begin
  result:= false;

  DHF.Read(fFileSize, 4);
  fFullPath:= StreamReadAnsiString(DHF);
  fFileName:= ExtractFileName(fFullpath);

  // Load Regions
  fRegions:= TRegions.Create(fFileSize);
  fRegions.LoadFromFile(DHF);

  // Load Sections
  DHF.Read(StreamSectionCount,4);
  for i := 0 to StreamSectionCount - 1 do begin
    DHF.Read(SectionType, sizeOf(TSectionType));
    case SectionType of
      stCode: begin
        Section:= TCodeSection.Create(self);
        Inc(fCodeSectionsCount);
      end;
      stImport: begin
        Section:= TImportSection.Create(self);
        fImportSection:= Section as TImportSection;
      end;
      stExport: begin
        Section:= TExportSection.Create(self);
        fExportSection:= Section as TExportSection;
      end;
      stResource: begin
        Section:= TResourceSection.Create(self);
      end;
      else
        raise Exception.Create('Bad section type (Executable.LoadFromFile)');
    end;

    if not Section.LoadFromFile(DHF, DAS) then
      Exit;
    Sections.Add(Section);
  end;
  fIsDisassembled:= true;
  result:= true;
end;


end.
