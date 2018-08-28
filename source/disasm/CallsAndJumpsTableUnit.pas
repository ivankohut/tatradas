unit CallsAndJumpsTableUnit;

interface

uses
  Classes,
  SysUtils,
  Math,
  Types,
  // project units
  DisassemblerTypes;

type

{ TCallsAndJumps }

  TCaJEntry = record
    start, finish: Cardinal;
  end;

  TCallsAndJumps = class
  private
    Current: array of TCaJEntry;
    Next: array of Cardinal;
    fNextCount: Integer;
    fDisassemblerMap: TByteDynArray;
    function GetCount: Integer;
    function GetCapacity: Integer;
    function GetData(Index: Integer): TCaJEntry;
    procedure SetCapacity(aCapacity: Integer);
  public
    constructor Create(var aDisassemblerMap: TByteDynArray); overload;

    procedure Add(Address: Cardinal);                              // pridanie do buducich
    procedure Process(LastAddress: Cardinal);                      // spracovanie buducich -> na aktualne
    property CAJ[index: Integer]: TCAJEntry read GetData; default; // aktualne skoky
    property Count: Integer read GetCount;                         // Current Count
    property Capacity: Integer read GetCapacity write SetCapacity; // Next Capacity
  end;

implementation

{ TCallsAndJumps }

constructor TCallsAndJumps.Create(var aDisassemblerMap: TByteDynArray);
begin
  fDisassemblerMap := aDisassemblerMap;
end;



function TCallsAndJumps.GetCount: Integer;
begin
  Result := Length(current);
end;



function TCallsAndJumps.GetCapacity: Integer;
begin
  Result := Length(Next);
end;



procedure TCallsAndJumps.SetCapacity(aCapacity: Integer);
begin
  SetLength(Next, aCapacity);
end;



function TCallsAndJumps.GetData(Index: Integer): TCaJEntry;
begin
  Result := current[Index];
end;



procedure TCallsAndJumps.Add(Address: Cardinal);
begin
  if (fDisassemblerMap[address] and dfPart) <> 0 then
    Exit;

  if Capacity <= fNextCount then
    Capacity := Capacity + 10;

  Next[fNextCount] := Address;
  Inc(fNextCount);
end;



procedure TCallsAndJumps.Process(LastAddress: Cardinal);
var
  pom: TCardinalDynArray;

  procedure MergeCAS(a1, a2, b1, b2: Cardinal);
  var
    zac, i, pocet: Integer;
  begin
    zac := a1;
    pocet := 0;
    repeat
      if Next[a1] > Next[b1] then begin
        pom[pocet] := Next[b1];
        Inc(b1);
      end
      else begin
        pom[pocet] := Next[a1];
        Inc(a1);
      end;
      Inc(pocet);
    until (a1 > a2) or (b1 > b2);

    if a1 > a2 then
      for i := b1 to b2 do begin
        pom[pocet] := Next[i];
        Inc(pocet);
      end
    else
      for i := a1 to a2 do begin
        pom[pocet] := Next[i];
        Inc(pocet);
      end;


  //  if b1 > b2 then  for i:=0 to a2 - a1 do next[b1 - (a2 - a1 + 1) + i]:=next[a1 + i]; // b1 = b2 - 1

    for i := 0 to pocet - 1 do
      Next[i + zac] := pom[i];

  end;

  procedure MergeSortCAS(zac, kon: Cardinal);
  var
    stred: Cardinal;
  begin
    stred := (zac + kon) div 2;
    if zac < stred then
      MergeSortCAS(zac, stred);
    if (stred + 1) < kon then
      MergeSortCAS(stred + 1, kon);
    MergeCAS(zac, stred, Min(stred + 1, kon), kon);
  end;

var
  i, j: Integer;
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
  j := 0;
  SetLength(current, 1);
  current[0].start := Next[0];

  for i := 1 to fNextCount - 1 do begin
    if Next[i] <> Next[i - 1] then begin
      Inc(j);
      setlength(current, j + 1);
      current[j].start := Next[i];
      current[j - 1].finish := Next[i] - 1;
    end;
  end;
  current[j].finish := lastAddress - 1;
  SetLength(Next, 0);
  fNextCount := 0;
  SetLength(pom, 0);
end;



end.
