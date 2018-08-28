{ TODO:
}

unit RegionsUnit;

interface

uses
  SysUtils,
  Classes,
  Contnrs,
  // project units
  procmat;

type

  TRegion = class
    Name: string;
    Offset: Cardinal;
    Size: Cardinal;
    constructor Create(AName: string; AOffset, ASize: Cardinal);
  end;

  TRegions = class
  private
    fFileSize: Cardinal;
    fRegions: TObjectList;
    fFinished: Boolean;
    function GetRegion(Index: Integer): TRegion;
    function GetCount: Integer;
  public
    constructor Create(FileSize: Cardinal);
    destructor Destroy; override;

    procedure Add(Name: string; Offset, Size: Cardinal);
    procedure Finish;

    procedure SaveToFile(DHF: TStream);
    procedure LoadFromFile(DHF: TStream);

    function GetIndexFromOffset(Offset: Cardinal): Integer;
    property Regions[Index: Integer]: TRegion read GetRegion; default;
    property Count: Integer read GetCount;
  end;


implementation

uses
  ExceptionsUnit;


//******************************************************************************
// TRegion class
//******************************************************************************


constructor TRegion.Create(AName: string; AOffset, ASize: Cardinal);
begin
  Name := AName;
  Offset := AOffset;
  Size := ASize;
end;



function CompareRegions(Region1, Region2: Pointer): Integer;
begin
  Result := CompareValue(TRegion(Region1).Offset, TRegion(Region2).Offset);
end;


//******************************************************************************
// TRegions class
//******************************************************************************


constructor TRegions.Create(FileSize: Cardinal);
begin
  fFileSize := FileSize;
  fRegions := TObjectList.Create;
  fRegions.OwnsObjects := True;
end;



destructor TRegions.Destroy;
begin
  fRegions.Free;
end;



function TRegions.GetRegion(Index: Integer): TRegion;
begin
  Result := fRegions[Index] as TRegion;
end;



function TRegions.GetCount: Integer;
begin
  Result := fRegions.Count;
end;



procedure TRegions.Add(Name: string; Offset, Size: Cardinal);
begin
  if fFinished then
    raise EIllegalState.Create('Regions already finished');

  fRegions.Add(TRegion.Create(Name, Offset, Size));
end;



procedure TRegions.Finish;
begin
  fFinished := True;
  fRegions.Sort(CompareRegions);
end;



procedure TRegions.SaveToFile(DHF: TStream);
var
  RegionIndex: Integer;
  RegionsCount: Integer;
begin
  RegionsCount := fRegions.Count;
  DHF.Write(RegionsCount, 4);
  for RegionIndex := 0 to Count - 1 do begin
    with (fRegions[RegionIndex] as TRegion) do begin
      StreamWriteAnsiString(DHF, Name);
      DHF.Write(Offset, 4);
      DHF.Write(Size, 4);
    end;
  end;
end;



procedure TRegions.LoadFromFile(DHF: TStream);
var
  RegionIndex: Integer;
  Name: string;
  Offset, Size: Cardinal;
  RegionsCount: Integer;
begin
  DHF.Read(RegionsCount, 4);
  for RegionIndex := 0 to RegionsCount - 1 do begin
    Name := StreamReadAnsiString(DHF);
    Offset := DHF.Read(Offset, 4);
    Size := DHF.Read(Size, 4);
    fRegions.Add(TRegion.Create(Name, Offset, Size));
  end;
  fFinished := True;
end;



function TRegions.GetIndexFromOffset(Offset: Cardinal): Integer;
var
  RegionIndex: Integer;
begin
  if Count = 0 then begin
    Result := -1;
    Exit;
  end;
  RegionIndex := Count - 1;
  if Offset < fFileSize then begin
    while Offset < (fRegions[RegionIndex] as TRegion).Offset do
      Dec(RegionIndex);
    if Offset < (fRegions[RegionIndex] as TRegion).Offset + (fRegions[RegionIndex] as TRegion).Size then
      Result := RegionIndex
    else
      Result := -1;
  end
  else
    raise EIllegalState.Create('Offset higher than file size');
end;


end.
