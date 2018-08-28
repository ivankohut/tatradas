unit VersionUnit;

interface

uses
  Math,
  SysUtils,
  Types;

type

  TVersion = class
  private
    fMajor: Integer;
    fMinor: Integer;
    fFix: Integer;
  public
    constructor Create(AVersion: string); overload;
    constructor Create(Maj, Min, Fix: Integer); overload;

    function Compare(AVersion: string): TValueRelationship; overload;
    function Compare(AVersion: TVersion): TValueRelationship; overload;

    property Major: Integer read fMajor;
    property Minor: Integer read fMinor;
    property Fix: Integer read fFix;
  end;


implementation



constructor TVersion.Create(Maj, Min, Fix: Integer);
begin
  fMajor := Maj;
  fMinor := Min;
  fFix := Fix;
end;



constructor TVersion.Create(AVersion: string);
var
  MajorVerStr: string;
  MinorVerStr: string;
  FixVerStr: string;
  Index: Integer;
begin
  Index := 1;
  MajorVerStr := '';
  MinorVerStr := '';
  FixVerStr := '';

  // Read major version number
  while Index <= Length(AVersion) do begin
    if AVersion[Index] = '.' then
      Break;
    MajorVerStr := MajorVerStr + AVersion[Index];
    Inc(Index);
  end;
  Inc(Index);
  fMajor := StrToIntDef(MajorVerStr, 0);

  // Read minor version number
  while Index <= Length(AVersion) do begin
    if AVersion[Index] = '.' then
      Break;
    MinorVerStr := MinorVerStr + AVersion[Index];
    Inc(Index);
  end;
  Inc(Index);
  fMinor := StrToIntDef(MinorVerStr, 0);

  // Read fix version number
  while Index <= Length(AVersion) do begin
    FixVerStr := FixVerStr + AVersion[Index];
    Inc(Index);
  end;
  fFix := StrToIntDef(FixVerStr, 0);
end;



function TVersion.Compare(AVersion: string): TValueRelationship;
var
  Version: TVersion;
begin
  Version := TVersion.Create(AVersion);
  Result := self.Compare(Version);
  Version.Free;
end;



function TVersion.Compare(AVersion: TVersion): TValueRelationship;
begin
  Result := CompareValue(AVersion.Major, self.Major);
  if Result = EqualsValue then
    Result := CompareValue(AVersion.Minor, self.Minor);
  if Result = EqualsValue then
    Result := CompareValue(AVersion.Fix, self.Fix);
end;


end.
