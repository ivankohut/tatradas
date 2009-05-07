{ TODO:
  MemOffset
}

unit CustomFileUnit;

{$INCLUDE 'delver.inc'}

interface

uses
  Classes,
  SysUtils,

  procmat,
  StringRes,
  ExecFileUnit,
  SectionUnit,
  CodeSectionUnit;

type

  // Code section parameters
  TCustomFileParameters = record
    EntrypointOffset: cardinal; // Entry point offset - relative to the beginning of the file
    FileOffset: cardinal; // Code section file offset - relative to the beginning of the file
    Size: cardinal; // File or Mem Size, currently the same size
    Bit32: boolean; // true = 32 bit, false = 16 bit
  end;


  TCustomFile = class(TExecutableFile)
  public
    constructor Create(InputFile: TStream; aFileName: TFileName; Parameters: TCustomFileParameters); overload;
    destructor Destroy; override;

    procedure SaveToFile(DHF: TStream; var DAS: TextFile); override;
    procedure LoadFromFile(DHF: TStream; var DAS: TextFile); override;
  end;


implementation


constructor TCustomFile.Create(InputFile: TStream; aFileName: TFileName; Parameters: TCustomFileParameters);
var
  CodeSection: TCodeSection;
begin
  inherited Create(InputFile, aFileName);
  fExecFormat := ffCustom;

  fRegions.Add('Code', Parameters.FileOffset, Parameters.Size);
  fRegions.Finish;

  InputFile.Seek(0, 0);
  CodeSection := TCodeSection.Create(InputFile, Parameters.bit32, Parameters.FileOffset, Parameters.Size, Parameters.FileOffset, Parameters.Size, 0, 'N/A', self);
  CodeSection.EntryPointAddress := Parameters.EntrypointOffset - Parameters.FileOffset;
  Sections.Add(CodeSection);
end;



procedure TCustomFile.SaveToFile(DHF: TStream; var DAS: TextFile);
begin
  inherited SaveToFile(DHF, DAS);
end;



procedure TCustomFile.LoadFromFile(DHF: TStream; var DAS: TextFile);
begin
  inherited LoadFromFile(DHF, DAS);
end;



destructor TCustomFile.Destroy;
begin
  inherited;
end;


end.
