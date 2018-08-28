unit DisassembledBlocksUnit;

interface

type
  TDisassembledBlock = record
    Address: Cardinal;
    Size: Cardinal;
  end;

  TDisassembledBlocks = class
  private
    fCount: Integer;
    fCapacity: Integer;
    fBlocks: array of TDisassembledBlock;
    function GetBlock(Index: Integer): TDisassembledBlock;
    function GetBlockCount: Integer;
  public
    procedure Clear;
    procedure Add(Address, Size: Cardinal);
    property Items[Index: Integer]: TDisassembledBlock read GetBlock; default;
    property Count: Integer read GetBlockCount;
  end;


implementation

uses
  SysUtils;



procedure TDisassembledBlocks.Add(Address, Size: Cardinal);
begin
  Inc(fCount);
  if fCount > fCapacity then begin
    fCapacity := fCapacity + 10;
    SetLength(fBlocks, fCapacity);
  end;
  fBlocks[fCount - 1].Address := Address;
  fBlocks[fCount - 1].Size := Size;
end;



function TDisassembledBlocks.GetBlock(Index: Integer): TDisassembledBlock;
begin
  if (Index >= 0) and (Index < fCount) then
    Result := fBlocks[Index];
end;



function TDisassembledBlocks.GetBlockCount: Integer;
begin
  Result := fCount;
end;



procedure TDisassembledBlocks.Clear;
begin
  SetLength(fBlocks, 10);
  fCapacity := 10;
  fCount := 0;
end;



end.
