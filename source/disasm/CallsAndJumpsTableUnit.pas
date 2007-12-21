unit CallsAndJumpsTableUnit;

interface

uses
  Classes,
  SysUtils,
  Math,
  Types,

  DisassemblerTypes,
  procmat;


type

{ TCallsAndJumps }

  TCaJEntry = record
    start,finish: cardinal;
  end;

  TCallsAndJumps = class
  private
    Current: array of TCaJEntry;
    Next: array of cardinal;
    fNextCount: integer;
    fDisassemblerMap: TByteDynArray;
    function GetCount: integer;
    function GetCapacity: integer;
    function GetData(index: integer):tcajentry;
    procedure SetCapacity(aCapacity: integer);
  public
    constructor Create(var aDisassemblerMap: TByteDynArray); overload;

    procedure Add(address: cardinal);                              // pridanie do buducich
    procedure Process(lastAddress: cardinal);                      // spracovanie buducich -> na aktualne
    property CAJ[index: integer]: TCAJEntry read GetData; default; // aktualne skoky
    property Count: Integer read GetCount;                         // Current Count
    property Capacity: Integer read GetCapacity write SetCapacity; // Next Capacity
  end;

implementation

{ TCallsAndJumps }

constructor TCallsAndJumps.Create(var aDisassemblerMap: TByteDynArray);
begin
  fDisassemblerMap:=aDisassemblerMap;
end;



function TCallsAndJumps.GetCount: integer;
begin
  result:=Length(current);
end;



function TCallsAndJumps.GetCapacity: integer;
begin
  result:=Length(next);
end;



procedure TCallsAndJumps.SetCapacity(aCapacity: integer);
begin
  SetLength(next,aCapacity);
end;



function TCallsAndJumps.GetData(index: integer): tcajentry;
begin
  result:=current[index];
end;



procedure TCallsAndJumps.Add(address: cardinal);
begin
  if (fDisassemblerMap[address] and dfPart) <> 0 then
    Exit;

  if Capacity <= fNextCount then
    Capacity:=Capacity + 10;

  next[fNextCount]:=address;
  Inc(fNextCount);
end;



procedure TCallsAndJumps.Process(lastAddress: cardinal);
var
  pom: TCardinalDynArray;

  procedure MergeCAS(a1,a2,b1,b2:cardinal);
  var zac,i,pocet: integer;
  begin
    zac:=a1;
    pocet:=0;
    repeat
      if next[a1]>next[b1] then begin
        pom[pocet]:=next[b1];
        inc(b1);
      end
      else begin
        pom[pocet]:=next[a1];
        inc(a1);
      end;
      inc(pocet);
    until (a1 > a2) or (b1 > b2);

    if a1 > a2 then
      for i:=b1 to b2 do begin
        pom[pocet]:=next[i];
        inc(pocet);
      end
    else
      for i:=a1 to a2 do begin
        pom[pocet]:=next[i];
        inc(pocet);
      end;


  //  if b1 > b2 then  for i:=0 to a2 - a1 do next[b1 - (a2 - a1 + 1) + i]:=next[a1 + i]; // b1 = b2 - 1

    for i:=0 to pocet-1 do next[i+zac]:=pom[i];

  end;

  procedure MergeSortCAS(zac, kon: cardinal);
  var stred: cardinal;
  begin
    stred:=(zac+kon) div 2;
    if zac < stred then MergeSortCAS(zac, stred);
    if (stred+1) < kon then MergeSortCAS(stred+1, kon);
    MergeCAS(zac, stred, Min(stred+1, kon), kon);
  end;



var i,j:integer;
begin
  if fNextCount = 0 then begin
    SetLength(current, 0);
    SetLength(pom, 0);
    Exit;
  end;
  if fNextCount > 1 then begin
    SetLength(pom, fNextCount);
    MergeSortCAS(0, fNextCount - 1);
  end;
  j:=0;
  SetLength(current, 1);
  current[0].start:=next[0];

  for i:=1 to fNextCount-1 do begin
    if next[i] <> next[i-1] then begin
      Inc(j);
      setlength(current, j+1);
      current[j].start:=next[i];
      current[j-1].finish:=next[i]-1;
    end;
  end;
  current[j].finish:=lastAddress-1;
  SetLength(next,0);
  fNextCount:=0;
  SetLength(pom,0);
end;





end.
