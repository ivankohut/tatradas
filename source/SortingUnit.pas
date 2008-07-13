unit SortingUnit;

interface

type

  TCompareFun = function (Index1, Index2: Integer): Integer of object;
//  TExchangeProc = procedure (Index1, Index2: Integer);
  TGetItemFun = function (Index: Integer): TObject of object;
  TSetItemProc = procedure (Index: Integer; Obj: TObject) of object;

procedure MergeSortObjects(CompareFun: TCompareFun; GetItemFun: TGetItemFun; SetItemProc: TSetItemProc; BeginIndex, EndIndex: Integer);

implementation

uses
  Math, Types, Classes, SysUtils;

procedure MergeSortObjects(CompareFun: TCompareFun; GetItemFun: TGetItemFun; SetItemProc: TSetItemProc; BeginIndex, EndIndex: Integer);

  procedure Merge(const Start1, Finish1, Start2, Finish2: Integer);
  var
    Temp: array of TObject;
    TempIndex: integer;
    ItemsCount: Integer;
    Index1, Index2: Integer;
    RestStart, RestFinish, RestIndex: Integer;
    SortedIndex: Integer;
  begin
    ItemsCount := Finish1 - Start1 + 1 + Finish2 - Start2 + 1;
    SetLength(Temp, ItemsCount);
    Index1 := Start1;
    Index2 := Start2;

    // Filling the temporary sorted array until one of the input arrays becomes used
    TempIndex := 0;
    repeat
      if CompareFun(Index1, Index2) = 1 then begin
        Temp[TempIndex] := GetItemFun(Index2);
        Inc(Index2);
      end
      else begin
        Temp[TempIndex] := GetItemFun(Index1);
        Inc(Index1);
      end;
      Inc(TempIndex);
    until (Index1 > Finish1) or (Index2 > Finish2);

    // Fill the rest by items from the not totally used array
    if Index1 > Finish1 then begin
      RestStart := Index2;
      RestFinish := Finish2;
    end
    else begin
      RestStart := Index1;
      RestFinish := Finish1;
    end;
    for RestIndex := RestStart to RestFinish do begin
      Temp[TempIndex]:= GetItemFun(RestIndex);
      Inc(TempIndex);
    end;

    // Move sorted items from temporary array to the destination
    for SortedIndex := 0 to ItemsCount - 1 do
      SetItemProc(Start1 + SortedIndex, Temp[SortedIndex]);
  end;


  procedure MergeSort(BeginIndex, EndIndex: Integer);
  var
    MiddleIndex: Integer;
  begin
    MiddleIndex := (BeginIndex + EndIndex) div 2;
    if BeginIndex < MiddleIndex then
      MergeSort(BeginIndex, MiddleIndex);
    if (MiddleIndex + 1) < EndIndex then
      MergeSort(MiddleIndex + 1, EndIndex);

    Merge(BeginIndex, MiddleIndex, Min(MiddleIndex + 1, EndIndex), EndIndex);
  end;

  
begin
  MergeSort(BeginIndex, EndIndex);
end;




end.
