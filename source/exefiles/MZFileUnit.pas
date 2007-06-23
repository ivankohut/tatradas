unit MZFileUnit;

{$INCLUDE 'delver.inc'}

interface

uses
  Classes,
  SysUtils,

  procmat,
  ExecFileUnit,
  CodeSectionUnit;

type

  TRelocationEntry = record
    Offset: word;
    Segment: word;
  end;

  TMZFile = class(TExecutableFile)
  private
    fHeader: TMZHeader;
    fRelocations: array of TRelocationEntry;
  public
    constructor Create; overload; override;
    constructor Create(InputFile: TStream; aFileName: TFileName); overload; override;
    destructor Destroy(); override;
    function SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean; override;
    function LoadFromFile(DHF: TStream; var DAS: TextFile): boolean; override;
//    function GetAdvancedInfo: TExecFileAdvancedInfo; override;
  end;

implementation

const
  c_MZFileAdvancedInfoCount = 10;


//******************************************************************************
// TMZFile class
//******************************************************************************


constructor TMZFile.Create;
begin
  inherited;
  fExecFormat:= ffMZ;
  fFormatDescription:= 'MZ - DOS executable (16-bit)';
end;



constructor TMZFile.Create(InputFile: TStream; aFileName: TFileName);
var
  CodeSection: TCodeSection;
  CodeSize: cardinal;
begin
  inherited;
  fExecFormat:= ffMZ;
  fFormatDescription:= 'MZ - DOS executable (16-bit)';

  InputFile.Seek(0, 0);
  InputFile.Read(fHeader, 40);
  CodeSize:= (fHeader.PageCount-1)*512 + fHeader.PageRemainder - fHeader.HeaderSize*16;

  // Code section
  if CodeSize > 0 then begin
    CodeSection:= TCodeSection.Create(InputFile, false, fHeader.HeaderSize*16, CodeSize, fHeader.HeaderSize*16, CodeSize, 0, 'N/A', self);
    CodeSection.EntryPointAddress:= word(fHeader.ReloCS*16 + fHeader.EXEIP);
    Sections.Add(CodeSection);
  end;

  // Read relocations
  if fHeader.RelocCount > 0 then begin
    SetLength(fRelocations, fHeader.RelocCount);
    InputFile.Seek(fHeader.RelocTableOffset, 0);
    InputFile.Read(fRelocations[0], fHeader.RelocCount * SizeOf(TRelocationEntry));
  end;

end;



function TMZFile.SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean;
begin
  result:= inherited SaveToFile(DHF, DAS, SaveOptions);
  if soProject in SaveOptions then begin
    DHF.Write(fHeader, SizeOf(TMZHeader));
    DHF.Write(fRelocations[0], fHeader.RelocCount * SizeOf(TRelocationEntry));
  end;
end;



function TMZFile.LoadFromFile(DHF: TStream; var DAS: TextFile): boolean;
begin
  result:= inherited LoadFromFile(DHF, DAS);
  DHF.Read(fHeader, SizeOf(TMZHeader));
  SetLength(fRelocations, fHeader.RelocCount);
  DHF.Read(fRelocations[0], fHeader.RelocCount * SizeOf(TRelocationEntry));
end;



destructor TMZFile.Destroy;
begin
  inherited;
end;


{
function TMZFile.GetAdvancedInfo: TExecFileAdvancedInfo;
begin
  result:=TExecFileAdvancedInfo.Create(c_MZFileAdvancedInfoCount);
  result.Add('', 'Page remainder:', IntToHex(header.pageremainder,4));
  result.Add('', 'Page count:', IntToStr(header.pagecount)+'(dec)');
  result.Add('', 'Relocations:', IntToStr(header.RelocCount)+'(dec)');
  result.Add('', 'Header size:', IntToHex(header.minmem,4));
  result.Add('', 'MinMem:', IntToHex(header.minmem,4));
  result.Add('', 'MaxMem:', IntToHex(header.maxmem,4));
  result.Add('', 'SS:SP', IntToHex(header.reloSS,4)+':'+IntToHex(header.EXESP,4));
  result.Add('', 'Check sum:', IntToHex(header.ChkSum,4));
  result.Add('', 'CS:IP', IntToHex(header.reloCS,4)+':'+IntToHex(header.EXEIP,4));
  result.Add('', 'Overlay:', IntToHex(header.Overlay,4));
end;
}


end.