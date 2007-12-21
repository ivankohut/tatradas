unit DisassembledBlocksUnit;

interface

type

  TDisassembledBlock = record
    Address: cardinal;
    Size: cardinal;
  end;

  TDisassembledBlocks = class
  private
    fCount: integer;
    fCapacity: integer;
    fBlocks: array of TDisassembledBlock;
    function GetBlock(Index: integer): TDisassembledBlock;
    function GetBlockCount: integer;
  public
    procedure Clear;
  //  function Add(Address, Size: cardinal): integer;
    procedure Add(Address, Size: cardinal);
    property Items[Index: integer]: TDisassembledBlock read GetBlock; default;
    property Count: integer read GetBlockCount;
  end;


implementation

uses
  SysUtils;

//function TDisassembledBlocks.Add(Address, Size: cardinal): integer;
procedure TDisassembledBlocks.Add(Address, Size: cardinal);
begin
  Inc(fCount);
  if fCount > fCapacity then begin
    fCapacity:= fCapacity + 10;
    SetLength(fBlocks, fCapacity);
  end;
  fBlocks[fCount - 1].Address:= Address;
  fBlocks[fCount - 1].Size:= Size;
end;



function TDisassembledBlocks.GetBlock(Index: integer): TDisassembledBlock;
begin
  if (Index >= 0) and (Index < fCount) then
    result:= fBlocks[Index]
  else
    raise Exception.Create('Index out of bounds');
end;



function TDisassembledBlocks.GetBlockCount: integer;
begin
  result:= fCount;
end;



procedure TDisassembledBlocks.Clear;
begin
  SetLength(fBlocks, 10);
  fCapacity:= 10;
  fCount:= 0;
end;



end.