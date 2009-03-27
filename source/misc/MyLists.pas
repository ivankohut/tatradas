unit MyLists;

interface

type
  TCardinalList = class
  private
    fCount: Integer;
    fCapacity: Integer;
    fItems: array of Cardinal;
    function GetItem(Index: Integer): cardinal;
    procedure SetItem(Index: Integer; Value: cardinal);
  public
    constructor Create;
    procedure Add(const Value: Cardinal);
    procedure Clear;
    property Count: Integer read fCount;
    property Items[Index: Integer]: Cardinal read GetItem write SetItem; default;
  end;

implementation


constructor TCardinalList.Create;
begin
  SetLength(fItems, 10);
  fCapacity := 10;
  fCount := 0;
end;



function TCardinalList.GetItem(Index: Integer): cardinal;
begin
  result := fItems[Index];
end;



procedure TCardinalList.SetItem(Index: Integer; Value: cardinal);
begin
  fItems[Index] := Value;
end;



procedure TCardinalList.Add(const Value: Cardinal);
begin
  if fCount >= fCapacity then begin
    fCapacity := 2 * fCapacity;
    SetLength(fItems, fCapacity);
  end;
  fItems[fCount] := Value;
  Inc(fCount);
end;



procedure TCardinalList.Clear;
begin
  SetLength(fItems, 10);
  fCount := 0;
  fCapacity := 10;
end;



end.