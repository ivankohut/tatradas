unit COMFileUnit;

{$INCLUDE 'delver.inc'}

interface

uses
  Classes,
  SysUtils,
  // project units
  procmat,
  StringRes,
  ExecFileUnit,
  SectionUnit,
  CodeSectionUnit;

type

  TCOMFile = class(TExecutableFile)
  public
    constructor Create; overload; override;
    constructor Create(InputFile: TStream; aFileName: TFileName); overload; override;
    destructor Destroy; override;
    procedure SaveToFile(DHF: TStream; var DAS: TextFile); override;
    procedure LoadFromFile(DHF: TStream; var DAS: TextFile); override;
  end;


implementation



constructor TCOMFile.Create;
begin
  inherited;
  fExecFormat := ffCOM;
end;



constructor TCOMFile.Create(InputFile: TStream; aFileName: TFileName);
var
  CodeSection: TCodeSection;
begin
  inherited;
  fExecFormat := ffCOM;

  fRegions.Add('COM file', 0, FileSize);
  fRegions.Finish;

  CodeSection := TCodeSection.Create(InputFile, False, 0, FileSize, $100, FileSize, 0, 'N/A', self);
  CodeSection.EntryPointAddress := 0;
  Sections.Add(CodeSection);
end;



procedure TCOMFile.SaveToFile(DHF: TStream; var DAS: TextFile);
begin
  inherited SaveToFile(DHF, DAS);
end;



procedure TCOMFile.LoadFromFile(DHF: TStream; var DAS: TextFile);
begin
  inherited LoadFromFile(DHF, DAS);
end;



destructor TCOMFile.Destroy;
begin
  inherited;
end;



end.
