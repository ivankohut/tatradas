{ TODO:

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

  TCustomFileParameters = record
    Entrypoint: cardinal;
    StartOffset: cardinal;
    Size: cardinal;
    Bit32: boolean;
  end;


  TCustomFile = class(TExecutableFile)
    constructor Create(InputFile: TStream; aFileName: TFileName; Parameters: TCustomFileParameters); overload;
    destructor Destroy(); override;

    function SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean; override;
    function LoadFromFile(DHF: TStream; var DAS: TextFile): boolean; override;
  end;

  
implementation


constructor TCustomFile.Create(InputFile: TStream; aFileName: TFileName; Parameters: TCustomFileParameters);
var
  CodeSection: TCodeSection;
begin
  inherited Create(InputFile, aFileName);
  fExecFormat:= ffCustom;

  InputFile.Seek(0, 0);
  CodeSection:= TCodeSection.Create(InputFile, Parameters.bit32, Parameters.StartOffset, Parameters.Size, Parameters.StartOffset, Parameters.Size, 0, 'N/A', self);
  CodeSection.EntryPointAddress:= Parameters.Entrypoint - Parameters.StartOffset;
  Sections.Add(CodeSection);
end;



function TCustomFile.SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean; 
begin
  result:=inherited SaveToFile(DHF, DAS, SaveOptions);
end;



function TCustomFile.LoadFromFile(DHF: TStream; var DAS: TextFile): boolean; 
begin
  result:=inherited LoadFromFile(DHF, DAS);
end;



destructor TCustomFile.Destroy;
begin
  inherited;
end;


end.
