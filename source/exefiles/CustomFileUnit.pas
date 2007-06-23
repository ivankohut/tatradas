{ TODO:
  premenovat na CustomFileUnit
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

  TCustomFile= class(TExecutableFile)
    constructor Create(a: TStream; aFileName: TFileName; Parameters: TCustomFileParameters); overload;
    destructor Destroy(); override;

    function SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean; override;
    function LoadFromFile(DHF: TStream; var DAS: TextFile): boolean; override;
//    function LoadFromFile(var f:TextFile; a:TMemoryStream):boolean; override;

//    function GetAdvancedInfo: TExecFileAdvancedInfo; override;
  end;

  
implementation

const
  c_UnknownFileAdvancedInfoCount = 4;



constructor TCustomFile.Create(a: TStream; aFileName: TFileName; Parameters: TCustomFileParameters);
var
  CodeSection: TCodeSection;
begin
  inherited Create(a, aFileName);
  fExecFormat:= ffCustom;
  EntryPoint:= Parameters.Entrypoint;
  fFormatDescription:= 'Unknown or not executable file format!';

  a.Seek(0, 0);
  CodeSection:= TCodeSection.Create(a, Parameters.bit32, Parameters.StartOffset, Parameters.Size, Parameters.StartOffset, Parameters.Size, 0, 'N/A', self);
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


{
function TCustomFile.GetAdvancedInfo: TExecFileAdvancedInfo;
begin
  result:=TExecFileAdvancedInfo.Create(c_UnknownFileAdvancedInfoCount);
  result.Add('', 'Code section offset', IntToHex((Sections[0] as TCodeSection).FileOffset,8));
  result.Add('', 'Code section size', IntToHex((Sections[0] as TCodeSection).FileSize,8));
  if (Sections[0] as TCodeSection).Bit32 then
    result.Add('', 'Mode:', '32 bit')
  else
    result.Add('', 'Mode:', '16 bit');
  result.Add('', 'Entry point', IntToHex(EntryPoint,8));
end;
}


end.
