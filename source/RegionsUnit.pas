{ TODO:
  - pridavanie regionov predpoklada ich usporiadanie

}
unit RegionsUnit;

interface

uses 
  SysUtils,
  Classes,
  
  procmat;
  
type

  TRegion = record
    Name: string;
    Offset: cardinal;
    Size: cardinal;
//    SectionIndex: integer; - do we need SectionIndex ?
  end;

  TRegions = class
  private
    fFileSize: cardinal;
    fRegions: array of TRegion;
    fCount: integer;
    fFinished: boolean;
    function GetRegion(Index: integer): TRegion;
  public
    constructor Create(FileSize: cardinal);

    procedure Add(Name: string; Offset, Size: cardinal; SectionIndex: integer);
    procedure Finish;

    procedure SaveToFile(DHF: TStream);
    procedure LoadFromFile(DHF: TStream);

    function GetIndexFromOffset(Offset: cardinal): integer;
    property Regions[Index: integer]: TRegion read GetRegion; default;
    property Count: Integer read fCount;
  end;


implementation 


//******************************************************************************
// TRegions class
//******************************************************************************


constructor TRegions.Create(FileSize: cardinal);
begin
  fFileSize:= FileSize;
end;



function TRegions.GetRegion(Index: integer): TRegion;
begin
  result:= fRegions[Index];
end;



procedure TRegions.Add(Name: string; Offset, Size: cardinal; SectionIndex: integer);
begin
  if fFinished then
    raise Exception.Create('Regions already finished');
  Inc(fCount);
  SetLength(fRegions, fCount);
  fRegions[fCount-1].Name:= Name;
  fRegions[fCount-1].Offset:= Offset;
  fRegions[fCount-1].Size:= Size;
//  fRegions[fCount-1].SectionIndex:= SectionIndex;
end;



procedure TRegions.Finish;
begin
  fFinished:= true;
end;



procedure TRegions.SaveToFile(DHF: TStream);
var i: integer;
begin
  DHF.Write(fCount, 4);
  for i:=0 to fCount - 1 do begin
    with fRegions[i] do begin
      StreamWriteAnsiString(DHF, Name);
      DHF.Write(Offset, 4);
      DHF.Write(Size, 4);
//      DHF.Write(SectionIndex, 4);
    end;
  end;
end;



procedure TRegions.LoadFromFile(DHF: TStream);
var i: integer;
begin
  DHF.Read(fCount, 4);
  SetLength(fRegions, fCount);
  for i:=0 to fCount - 1 do begin
    with fRegions[i] do begin
      Name:=StreamReadAnsiString(DHF);
      DHF.Read(Offset, 4);
      DHF.Read(Size, 4);
//      DHF.Read(SectionIndex, 4);
    end;
  end;
  fFinished:= true;
end;



function TRegions.GetIndexFromOffset(Offset: cardinal): integer;
var
  RegionIndex: integer;
begin
  RegionIndex:= fCount - 1;
  if Offset < fFileSize then begin
    while Offset < fRegions[RegionIndex].Offset do
      Dec(RegionIndex);
    if Offset <  fRegions[RegionIndex].Offset + fRegions[RegionIndex].Size then
      result:= RegionIndex
    else
      result:= -1
  end
  else
    raise Exception.Create('Offset higher than file size');
end;

end.
