unit DisassemblerUnit;

{$INCLUDE 'delver.inc'}

interface

uses
  Classes,
  SysUtils,

  procmat,
  LoggerUnit,
  StringUtilities,
  ListsUnit,
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
    function GetBlocksCount: Integer;
    function GetDisassembledBlock(Index: Integer): TDisassembledBlock;
    function GetUndefinedOpcodeItem(Index: Integer): TUndefinedOpcodeItem;
    function GetUndefinedOpcodesCount: Integer;
  protected
    fStatistics: TStatistics;
    fCodeSize: Cardinal;
    fImportCandidates: TCardinalList;
    procedure DisassemblerCycle;
    function DisassembleBlock(Start, Finish: Cardinal): Boolean; virtual; abstract;
    procedure AddBlock(AAddress, ASize: Cardinal);
    procedure AddReference(const ReferencingAddress, ReferencedAddress: Cardinal; const ReferenceType: TReferenceType);
    procedure AddUndefinedOpcode(AAddress, AParsedSize: Cardinal);


  public
    CAJ: TCallsAndJumps;

    Disassembled: array of TDisassembledItem;

    constructor Create;
    destructor Destroy; override;

    procedure DisassembleAll; virtual; abstract;
    procedure Disassemble(Recursive: Boolean);

    property ImportCandidates: TCardinalList read fImportCandidates;
    property UndefinedOpcodes[Index: Integer]: TUndefinedOpcodeItem read GetUndefinedOpcodeItem;
    property UndefinedOpcodesCount: Integer read GetUndefinedOpcodesCount;
    property Statistics: TStatistics read fStatistics;
    property Blocks[Index: Integer]: TDisassembledBlock read GetDisassembledBlock;
    property BlockCount: Integer read GetBlocksCount;
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



function TDisassembler.GetDisassembledBlock(Index: Integer): TDisassembledBlock;
begin
  Result := fBlocks[Index];
end;



function TDisassembler.GetBlocksCount: integer;
begin
  Result := fBlocks.Count;
end;



procedure TDisassembler.AddBlock(AAddress, ASize: Cardinal);
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



procedure TDisassembler.Disassemble(Recursive: Boolean);
begin
  if Recursive then
    DisassemblerCycle
  else
    DisassembleBlock(CAJ[0].start, CAJ[0].finish);
end;



procedure TDisassembler.AddReference(const ReferencingAddress, ReferencedAddress: Cardinal; const ReferenceType: TReferenceType);
begin
  with Disassembled[ReferencedAddress] do begin
    Inc(ReferencesCount);
    SetLength(References, ReferencesCount);
    References[ReferencesCount - 1].Address := ReferencingAddress;
    References[ReferencesCount - 1].Typ := ReferenceType;
  end;
  Inc(fStatistics.References);
end;



procedure TDisassembler.AddUndefinedOpcode(AAddress, AParsedSize: Cardinal);
begin
  SetLength(fUndefinedOpcodes, Length(fUndefinedOpcodes) + 1);
  with fUndefinedOpcodes[High(fUndefinedOpcodes)] do begin
    Address := AAddress;
    ParsedSize := AParsedSize;
  end;
end;



function TDisassembler.GetUndefinedOpcodeItem(Index: Integer): TUndefinedOpcodeItem;
begin
  Result := fUndefinedOpcodes[Index];
end;



function TDisassembler.GetUndefinedOpcodesCount: Integer;
begin
  Result := Length(fUndefinedOpcodes);
end;



end.
