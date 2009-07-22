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
  DisassemblerUnit,
  DisassemblerTypes,
  CodeSectionUnit,
  ImportSectionUnit,
  ExportSectionUnit,
  ResourceSectionUnit;

type
  TExecFileCreateSectionEvent = procedure (ASection: TSection) of object;

  TExecFileFormat = (ffUnknown, ffCustom, ffPE, ffMZ, ffCOM, ffNE, LE, LX, ffELF);

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

    procedure Disassemble; virtual;
    procedure ClearDisassembled;

    procedure SaveToFile(DHF: TStream; var DAS: TextFile); overload; virtual;
    procedure LoadFromFile(DHF: TStream; var DAS: TextFile); virtual;

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

uses StringUtilities;


//******************************************************************************
// TExecutableFile class
//******************************************************************************


constructor TExecutableFile.Create;
begin
  fSections := TSections.Create;
end;



constructor TExecutableFile.Create(InputFile: TStream; aFileName: TFileName);
begin
  fFileSize := InputFile.Size;
  fFullPath := aFileName;
  fFileName := ExtractFileName(aFileName);
  fSections := TSections.Create;
  fRegions := TRegions.Create(FileSize);
end;



destructor TExecutableFile.Destroy;
begin
  Sections.Free;
  Regions.Free;
end;



procedure TExecutableFile.ClearDisassembled;
var
  SectionIndex: Integer;
begin
  // Reset all code sections if file is already disassembled
  if fIsDisassembled then begin
    for SectionIndex := 0 to Sections.Count - 1 do
      if Sections[SectionIndex].Typ = stCode then
        (Sections[SectionIndex] as TCodeSection).ClearDisassembled;
    fIsDisassembled := false;
  end;
end;



procedure TExecutableFile.Disassemble;
var
  SectionIndex, ExceptionSectionIndex: Integer;
  Options: TDisassembleOptions;
begin
  ClearDisassembled;

  // Nastavime parametre a disassemblujme
  for SectionIndex := 0 to Sections.Count - 1 do
    if Sections[SectionIndex].Typ = stCode then
      with Sections[SectionIndex] as TCodeSection do begin
        Options.Address := EntryPointAddress;
        Options.Size := CodeSize;
        Options.Bit32 := Bit32;
        try
          DisassembleAll(Options);
        except
          for ExceptionSectionIndex := 0 to SectionIndex do
            if Sections[ExceptionSectionIndex].Typ = stCode then
              (Sections[ExceptionSectionIndex] as TCodeSection).ClearDisassembled;
          raise;
        end;
      end;
  fIsDisassembled := True;
end;



procedure TExecutableFile.SaveToFile(DHF: TStream; var DAS: TextFile);
var
  SectionIndex: Integer;
  StreamSectionCount: Integer;
begin
  // Write ExecFile info
  DHF.Write(fFileSize, 4);
  StreamWriteAnsiString(DHF, fFullPath);
  fRegions.SaveToFile(DHF);
  StreamSectionCount := Sections.Count;
  DHF.Write(StreamSectionCount, 4);
  WriteLn(DAS, InjectStr(DASFileFirstLine, [FileName]));
  Writeln(DAS);

  // Write sections
  for SectionIndex := 0 to Sections.Count - 1 do begin
    DHF.Write(Sections[SectionIndex].typ, SizeOf(TSectionType));
    Sections[SectionIndex].SaveToFile(DHF, DAS);
  end;
end;



procedure TExecutableFile.LoadFromFile(DHF: TStream; var DAS: TextFile);
var
  i: Integer;
  SectionType: TSectionType;
  StreamSectionCount: Integer;
  Section: TSection;
begin
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
        Section := TCodeSection.Create(self);
        Inc(fCodeSectionsCount);
      end;
      stImport: begin
        Section := TImportSection.Create(self);
        fImportSection := Section as TImportSection;
      end;
      stExport: begin
        Section := TExportSection.Create(self);
        fExportSection := Section as TExportSection;
      end;
      stResource: begin
        Section := TResourceSection.Create(self);
      end;
      else
        raise EIllegalState.Create('Executable.LoadFromFile: Bad section type');
    end;

    Section.LoadFromFile(DHF, DAS);
    Sections.Add(Section);
  end;
  fIsDisassembled := True;
end;


end.
