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
  public
    property Header: TMZHeader read fHeader;
  end;


implementation


//******************************************************************************
// TMZFile class
//******************************************************************************


constructor TMZFile.Create;
begin
  inherited;
  fExecFormat:= ffMZ;
end;



constructor TMZFile.Create(InputFile: TStream; aFileName: TFileName);
var
  CodeSection: TCodeSection;
  CodeSize: cardinal;
begin
  inherited;
  fExecFormat:= ffMZ;

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


end.
