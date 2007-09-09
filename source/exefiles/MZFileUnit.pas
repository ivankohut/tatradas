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
    function GetRelocationEntry(Index: integer): TRelocationEntry;
    function GetRelocationsCount: integer;
  public
    constructor Create; overload; override;
    constructor Create(InputFile: TStream; aFileName: TFileName); overload; override;
    destructor Destroy(); override;
    function SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean; override;
    function LoadFromFile(DHF: TStream; var DAS: TextFile): boolean; override;
  public
    property Header: TMZHeader read fHeader;
    property Relocations[Index: integer]: TRelocationEntry read GetRelocationEntry;
    property RelocationsCount: integer read GetRelocationsCount;
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

  // Set Regions
  Regions.Add('MZ Header', 0, 40);
  Regions.Add('Code', fHeader.HeaderSize*16, CodeSize);
  Regions.Add('Relocation table', fHeader.RelocTableOffset, fHeader.RelocCount * SizeOf(TRelocationEntry));
  Regions.Finish;

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



destructor TMZFile.Destroy;
begin
  inherited;
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



function TMZFile.GetRelocationEntry(Index: integer): TRelocationEntry;
begin
  if (Index < 0) or (Index >= Length(fRelocations)) then begin
    result.Offset:= 0;
    result.Segment:= 0;
  end
  else
    result:= fRelocations[Index];
end;



function TMZFile.GetRelocationsCount: integer;
begin
  result:= Length(fRelocations);
end;


end.
