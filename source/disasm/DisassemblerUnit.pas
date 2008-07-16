unit DisassemblerUnit;

{$INCLUDE 'delver.inc'}

interface

uses
  Classes,
  SysUtils,

  procmat,
  LoggerUnit,
  StringUtilities,
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
    function GetBlocksCount: integer;

  protected
    fStatistics: TStatistics;
    fCodeSize: cardinal;
    constructor Create;
    procedure DisassemblerCycle;
    function DisassembleBlock(Start, Finish: cardinal): boolean; virtual; abstract;
    function GetDisassembledBlock(Index: integer): TDisassembledBlock;
    procedure AddBlock(AAddress, ASize: cardinal);

  public
    CAJ: TCallsAndJumps;

    Disassembled: array of TDisassembledItem;
    Imported: array of cardinal;

    destructor Destroy; override;

    procedure DisassembleAll; virtual; abstract;
    procedure Disassemble(Recursive: boolean);

    property Statistics: TStatistics read fStatistics;
    property Blocks[Index: integer]: TDisassembledBlock read GetDisassembledBlock;
    property BlockCount: integer read GetBlocksCount;
  end;


Implementation


constructor TDisassembler.Create;
begin
  fBlocks := TDisassembledBlocks.Create;
end;



destructor TDisassembler.Destroy;
begin
  fBlocks.Free;
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
  fBlocks.Add(AAddress, ASize);
end;



procedure TDisassembler.DisassemblerCycle;
var
  CAJIndex: Integer;
begin
  while CAJ.Count > 0 do begin
    for CAJIndex := 0 to CAJ.Count - 1 do begin
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



end.
