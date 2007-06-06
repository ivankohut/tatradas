unit COMFileUnit;

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

  TCOMFile = class(TExecutableFile)
    constructor Create(InputFile: TStream; aFileName: TFileName); overload; override;
    destructor Destroy(); override;
    function SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean; override;
//    function LoadFromFile(var f:TextFile; a:TMemoryStream):boolean; override;
    function LoadFromFile(DHF: TStream; var DAS: TextFile): boolean; override;
//    function GetAdvancedInfo: TExecFileAdvancedInfo; override;
  end;

implementation



constructor TCOMFile.Create(InputFile: TStream; aFileName: TFileName);
var
  CodeSection: TCodeSection;
begin
  inherited;
  fExecFormat:=COM;
  CodeSection:=TCodeSection.Create(InputFile, false, '', 0, InputFile.Size, 0, InputFile.Size, 0, self);
  CodeSection.EntryPointAddress:=0;
  Sections.Add(CodeSection);
  EntryPoint:=0;
  fFormatDescription:='COM (16-bit)';
end;



function TCOMFile.SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean;
begin
  result:=inherited SaveToFile(DHF, DAS, SaveOptions);
end;



function TCOMFile.LoadFromFile(DHF: TStream; var DAS: TextFile): boolean;
begin
  result:=inherited LoadFromFile(DHF, DAS);
end;



destructor TCOMFile.Destroy();
begin
  inherited;
end;


{
function TCOMFile.GetAdvancedInfo: TExecFileAdvancedInfo;
begin

end;
}


end.