{$ifdef fpc}
  {$mode delphi}
{$endif fpc}

type

  tcajentry = record
    start,finish: cardinal;
  end;

TCallsAndJumps = class
 private
  data: array of tcajentry;
  function GetCapacity: integer;
  function GetData(index: integer):tcajentry;
  function GetStart(index: integer):cardinal;
 public
  procedure Add(address: cardinal);
  procedure Process;
  property CaJ[index: integer]: tcajentry read GetData; default;
  property start[index: integer]: cardinal read GetStart;
  property Count: Integer read fCount;
end;


TDisassembler = class
 public
  constructor Create;
  function Disassemble:boolean;
 private
  CAS: TCallsAndJumps;
  function DisassembleBlock(start,finish: cardinal):boolean;
end;



function TDisassembler.Disassemble():boolean;
var i: integer;
begin
  while CAS.count > 0 do begin
    for i:=1 to CAS.Count do
      DisassembleBlock(CAS[i].start,CAS[i].finish);
    CAS.Process;
  end;
end;


begin

end.