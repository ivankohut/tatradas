{ TODO:
}

unit RegionsUnit;

interface

uses
  SysUtils,
  Classes,
  Contnrs,
  Math,

  procmat;

type

  TRegion = class
    Name: string;
    Offset: cardinal;
    Size: cardinal;
    constructor Create(AName: string; AOffset, ASize: cardinal);
  end;

  TRegions = class
  private
    fFileSize: cardinal;
    fRegions: TObjectList;
    fFinished: boolean;
    function GetRegion(Index: integer): TRegion;
    function GetCount: integer;
  public
    constructor Create(FileSize: cardinal);
    destructor Destroy; override;

    procedure Add(Name: string; Offset, Size: cardinal);
    procedure Finish;

    procedure SaveToFile(DHF: TStream);
    procedure LoadFromFile(DHF: TStream);

    function GetIndexFromOffset(Offset: cardinal): integer;
    property Regions[Index: integer]: TRegion read GetRegion; default;
    property Count: Integer read GetCount;
  end;


implementation


//******************************************************************************
// TRegion class
//******************************************************************************


constructor TRegion.Create(AName: string; AOffset, ASize: cardinal);
begin
  Name:= AName;
  Offset:= AOffset;
  Size:= ASize;
end;



function CompareRegions(Region1, Region2: Pointer): integer;
begin
  result:= CompareValue(TRegion(Region1).Offset, TRegion(Region2).Offset);
end;


//******************************************************************************
// TRegions class
//******************************************************************************


constructor TRegions.Create(FileSize: cardinal);
begin
  fFileSize:= FileSize;
  fRegions:= TObjectList.Create;
  fRegions.OwnsObjects:= true;
end;



destructor TRegions.Destroy;
begin
  fRegions.Free;
end;



function TRegions.GetRegion(Index: integer): TRegion;
begin
  result:= fRegions[Index] as TRegion;
end;



function TRegions.GetCount: integer;
begin
  result:= fRegions.Count;
end;



procedure TRegions.Add(Name: string; Offset, Size: cardinal);
begin
  if fFinished then
    raise Exception.Create('Regions already finished');

  fRegions.Add(TRegion.Create(Name, Offset, Size));
end;



procedure TRegions.Finish;
begin
  fFinished:= true;
  fRegions.Sort(CompareRegions);
end;



procedure TRegions.SaveToFile(DHF: TStream);
var i: integer;
begin
  DHF.Write(fRegions.Count, 4);
  for i:= 0 to Count - 1 do begin
    with (fRegions[i] as TRegion) do begin
      StreamWriteAnsiString(DHF, Name);
      DHF.Write(Offset, 4);
      DHF.Write(Size, 4);
    end;
  end;
end;



procedure TRegions.LoadFromFile(DHF: TStream);
var
  RegionIndex: integer;
  Name: string;
  Offset, Size: cardinal;
  RegionsCount: integer;
begin
  DHF.Read(RegionsCount, 4);
  for RegionIndex:= 0 to RegionsCount - 1 do begin
    Name:= StreamReadAnsiString(DHF);
    Offset:= DHF.Read(Offset, 4);
    Size:= DHF.Read(Size, 4);
    fRegions.Add(TRegion.Create(Name, Offset, Size))
  end;
  fFinished:= true;
end;



function TRegions.GetIndexFromOffset(Offset: cardinal): integer;
var
  RegionIndex: integer;
begin
  if Count = 0 then begin
    result:= -1;
    Exit;
  end;
  RegionIndex:= Count - 1;
  if Offset < fFileSize then begin
    while Offset < (fRegions[RegionIndex] as TRegion).Offset do
      Dec(RegionIndex);
    if Offset <  (fRegions[RegionIndex] as TRegion).Offset + (fRegions[RegionIndex] as TRegion).Size then
      result:= RegionIndex
    else
      result:= -1
  end
  else
    raise Exception.Create('Offset higher than file size');
end;


end.
