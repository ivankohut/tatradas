unit DisassemblerUnit;

{$INCLUDE 'delver.inc'}

interface

uses
  Classes,
  SysUtils,

  procmat,
  LoggerUnit,
  StringUtilities,
  MyLists,
  CallsAndJumpsTableUnit,
  DisassembledBlocksUnit,
  DisassemblerTypes,
  DisassemblerUtils,
  StrUtils
  ;


type

  // Ancestor of all disassemblers (currently of just one :) )

  TDisassembler = class
  private
    fBlocks: TDisassembledBlocks;
    fUndefinedOpcodes: array of TUndefinedOpcodeItem;
    function GetBlocksCount: integer;
    function GetDisassembledBlock(Index: integer): TDisassembledBlock;
    function GetUndefinedOpcodeItem(Index: integer): TUndefinedOpcodeItem;
    function GetUndefinedOpcodesCount: Integer;
  protected
    fStatistics: TStatistics;
    fCodeSize: cardinal;
    fImportCandidates: TCardinalList;
    procedure DisassemblerCycle;
    function DisassembleBlock(Start, Finish: cardinal): boolean; virtual; abstract;
    procedure AddBlock(AAddress, ASize: cardinal);
    procedure AddReference(const ReferencingAddress, ReferencedAddress: cardinal; const ReferenceType: TReferenceType);
    procedure AddUndefinedOpcode(AAddress, AParsedSize: cardinal);


  public
    CAJ: TCallsAndJumps;

    Disassembled: array of TDisassembledItem;

    constructor Create;
    destructor Destroy; override;

    procedure DisassembleAll; virtual; abstract;
    procedure Disassemble(Recursive: boolean);

    property ImportCandidates: TCardinalList read fImportCandidates;
    property UndefinedOpcodes[Index: Integer]: TUndefinedOpcodeItem read GetUndefinedOpcodeItem;
    property UndefinedOpcodesCount: Integer read GetUndefinedOpcodesCount;
    property Statistics: TStatistics read fStatistics;
    property Blocks[Index: integer]: TDisassembledBlock read GetDisassembledBlock;
    property BlockCount: integer read GetBlocksCount;
  end;


implementation


constructor TDisassembler.Create;
begin
  fBlocks := TDisassembledBlocks.Create;
  fImportCandidates := TCardinalList.Create;
end;



destructor TDisassembler.Destroy;
begin
  fBlocks.Free;
  fImportCandidates.Free;
end;



function TDisassembler.GetDisassembledBlock(Index: integer): TDisassembledBlock;
begin
  result := fBlocks[Index];
end;



function TDisassembler.GetBlocksCount: integer;
begin
  result := fBlocks.Count;
end;



procedure TDisassembler.AddBlock(AAddress, ASize: cardinal);
begin
  if ASize > 0 then
    fBlocks.Add(AAddress, ASize);
end;



procedure TDisassembler.DisassemblerCycle;
var
  CAJIndex: Integer;
begin
  while CAJ.Count > 0 do begin
    for CAJIndex := 0 to CAJ.Count - 1 do begin
//      if CAJ[CAJIndex].start < CAJ[CAJIndex].finish then
        DisassembleBlock(CAJ[CAJIndex].start, CAJ[CAJIndex].finish);
    end;
    CAJ.Process(fCodeSize);
  end;
end;



procedure TDisassembler.Disassemble(Recursive: boolean);
begin
  if Recursive then
    DisassemblerCycle
  else
    DisassembleBlock(CAJ[0].start, CAJ[0].finish);
end;



procedure TDisassembler.AddReference(const ReferencingAddress, ReferencedAddress: cardinal; const ReferenceType: TReferenceType);
begin
  with Disassembled[ReferencedAddress] do begin
    Inc(ReferencesCount);
    SetLength(References, ReferencesCount);
    References[ReferencesCount - 1].Address := ReferencingAddress;
    References[ReferencesCount - 1].Typ := ReferenceType;
  end;
  Inc(fStatistics.References);
end;



procedure TDisassembler.AddUndefinedOpcode(AAddress, AParsedSize: cardinal);
begin
  SetLength(fUndefinedOpcodes, Length(fUndefinedOpcodes) + 1);
  with fUndefinedOpcodes[High(fUndefinedOpcodes)] do begin
    Address := AAddress;
    ParsedSize := AParsedSize;
  end;
end;



function TDisassembler.GetUndefinedOpcodeItem(Index: integer): TUndefinedOpcodeItem;
begin
  result := fUndefinedOpcodes[Index];
end;



function TDisassembler.GetUndefinedOpcodesCount: Integer;
begin
  result := Length(fUndefinedOpcodes);
end;



end.
