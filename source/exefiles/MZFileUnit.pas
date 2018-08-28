unit MZFileUnit;

{$INCLUDE 'delver.inc'}

interface

uses
  Classes,
  SysUtils,
  // project units
  procmat,
  ExecFileUnit,
  CodeSectionUnit;

type

  TRelocationEntry = record
    Offset: Word;
    Segment: Word;
  end;


  TMZFile = class(TExecutableFile)
  private
    fHeader: TMZHeader;
    fRelocations: array of TRelocationEntry;
    function GetRelocationEntry(Index: Integer): TRelocationEntry;
    function GetRelocationsCount: Integer;
  public
    constructor Create; overload; override;
    constructor Create(InputFile: TStream; aFileName: TFileName); overload; override;
    destructor Destroy; override;
    procedure SaveToFile(DHF: TStream; var DAS: TextFile); override;
    procedure LoadFromFile(DHF: TStream; var DAS: TextFile); override;
  public
    property Header: TMZHeader read fHeader;
    property Relocations[Index: Integer]: TRelocationEntry read GetRelocationEntry;
    property RelocationsCount: Integer read GetRelocationsCount;
  end;


implementation


//******************************************************************************
// TMZFile class
//******************************************************************************


constructor TMZFile.Create;
begin
  inherited;
  fExecFormat := ffMZ;
end;



constructor TMZFile.Create(InputFile: TStream; aFileName: TFileName);
var
  CodeSection: TCodeSection;
  CodeSize: Cardinal;
begin
  inherited;
  fExecFormat := ffMZ;

  InputFile.Seek(0, 0);
  InputFile.Read(fHeader, 40);
  CodeSize := (fHeader.PageCount - 1) * 512 + fHeader.PageRemainder - fHeader.HeaderSize * 16;

  // Set Regions
  Regions.Add('MZ Header', 0, 40);
  Regions.Add('Code', fHeader.HeaderSize * 16, CodeSize);
  Regions.Add('Relocation table', fHeader.RelocTableOffset, fHeader.RelocCount * SizeOf(TRelocationEntry));
  Regions.Finish;

  // Code section
  if CodeSize > 0 then begin
    CodeSection := TCodeSection.Create(InputFile, False, fHeader.HeaderSize * 16, CodeSize, fHeader.HeaderSize * 16, CodeSize, 0, 'N/A', self);
    CodeSection.EntryPointAddress := Word(fHeader.ReloCS * 16 + fHeader.EXEIP);
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



procedure TMZFile.SaveToFile(DHF: TStream; var DAS: TextFile);
begin
  inherited SaveToFile(DHF, DAS);
  DHF.Write(fHeader, SizeOf(TMZHeader));
  DHF.Write(fRelocations[0], fHeader.RelocCount * SizeOf(TRelocationEntry));
end;



procedure TMZFile.LoadFromFile(DHF: TStream; var DAS: TextFile);
begin
  inherited LoadFromFile(DHF, DAS);
  DHF.Read(fHeader, SizeOf(TMZHeader));
  SetLength(fRelocations, fHeader.RelocCount);
  DHF.Read(fRelocations[0], fHeader.RelocCount * SizeOf(TRelocationEntry));
end;



function TMZFile.GetRelocationEntry(Index: Integer): TRelocationEntry;
begin
  if (Index < 0) or (Index >= Length(fRelocations)) then begin
    Result.Offset := 0;
    Result.Segment := 0;
  end
  else
    Result := fRelocations[Index];
end;



function TMZFile.GetRelocationsCount: Integer;
begin
  Result := Length(fRelocations);
end;


end.
