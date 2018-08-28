unit ListsUnit;

interface

type
  TCardinalList = class
  private
    fCount: Integer;
    fCapacity: Integer;
    fItems: array of Cardinal;
    function GetItem(Index: Integer): Cardinal;
    procedure SetItem(Index: Integer; Value: Cardinal);
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



function TCardinalList.GetItem(Index: Integer): Cardinal;
begin
  Result := fItems[Index];
end;



procedure TCardinalList.SetItem(Index: Integer; Value: Cardinal);
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
