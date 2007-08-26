unit VersionUnit;

//{$DEFINE TESTING}


interface

uses
  {$IFDEF TESTING}
  TestFramework,
  {$ENDIF}

  Math,
  SysUtils,
  Types,

  procmat
  ;

type

  TVersion = class
  private
    fMajor: integer;
    fMinor: integer;
    fFix: integer;
  public
    constructor Create(AVersion: string); overload;
    constructor Create(Maj, Min, Fix: integer); overload;

    function Compare(AVersion: string): TValueRelationship; overload;
    function Compare(AVersion: TVersion): TValueRelationship; overload;

    property Major: integer read fMajor;
    property Minor: integer read fMinor;
    property Fix: integer read fFix;
  end;

  {$IFDEF TESTING}
  TTestVersion = class(TTestCase)
  published
    procedure testCreate;
    procedure testCompare;
  end;
  {$ENDIF}

var
  TatraDAS_Version: TVersion;


implementation



constructor TVersion.Create(Maj, Min, Fix: integer);
begin
  fMajor:= Maj;
  fMinor:= Min;
  fFix:= Fix;
end;




constructor TVersion.Create(AVersion: string);
var
  MajorVerStr: string;
  MinorVerStr: string;
  FixVerStr: string;
  Index: integer;
begin
  Index:= 1;

  // Read major version number
  while Index <= Length(AVersion) do begin
    if AVersion[Index] = '.' then Break;
    MajorVerStr:=MajorVerStr + AVersion[Index];
    Inc(Index);
  end;
  Inc(Index);
  fMajor:= StrToIntDef(MajorVerStr, 0);

  // Read minor version number
  while Index <= Length(AVersion) do begin
    if AVersion[Index] = '.' then Break;
    MinorVerStr:=MinorVerStr + AVersion[Index];
    Inc(Index);
  end;
  Inc(Index);
  fMinor:= StrToIntDef(MinorVerStr, 0);

  // Read fix version number
  while Index <= Length(AVersion) do begin
    FixVerStr:=FixVerStr + AVersion[Index];
    Inc(Index);
  end;
  fFix:= StrToIntDef(FixVerStr, 0);
end;



function TVersion.Compare(AVersion: string): TValueRelationship;
var Version: TVersion;
begin
  Version:= TVersion.Create(AVersion);
  result:= self.Compare(Version);
  Version.Free;
end;



function TVersion.Compare(AVersion: TVersion): TValueRelationship;
begin
  result:=CompareValue(AVersion.Major, self.Major);
  if result = EqualsValue then
    result:=CompareValue(AVersion.Minor, self.Minor);
    if result = EqualsValue then
      result:=CompareValue(AVersion.Fix, self.Fix);
end;


//******************************************************************************
// TTestVersion Class
//******************************************************************************


{$IFDEF TESTING}
procedure TTestVersion.testCreate;
var Version: TVersion;
begin
  Version:= TVersion.Create('2.9.7');
  check(Version.Major = 2, 'Major = '+IntToStr(Version.Major) + ', Minor = ' + IntToStr(Version.Minor) + ', Fix = ' + IntToStr(Version.Fix));
  check(Version.Minor = 9, 'Major = '+IntToStr(Version.Major) + ', Minor = ' + IntToStr(Version.Minor) + ', Fix = ' + IntToStr(Version.Fix));
  check(Version.Fix = 7, 'Major = '+IntToStr(Version.Major) + ', Minor = ' + IntToStr(Version.Minor) + ', Fix = ' + IntToStr(Version.Fix));
  Version.Free;

  Version:= TVersion.Create(2, 9, 7);
  check(Version.Major = 2, 'Major = '+IntToStr(Version.Major) + ', Minor = ' + IntToStr(Version.Minor) + ', Fix = ' + IntToStr(Version.Fix));
  check(Version.Minor = 9, 'Major = '+IntToStr(Version.Major) + ', Minor = ' + IntToStr(Version.Minor) + ', Fix = ' + IntToStr(Version.Fix));
  check(Version.Fix = 7, 'Major = '+IntToStr(Version.Major) + ', Minor = ' + IntToStr(Version.Minor) + ', Fix = ' + IntToStr(Version.Fix));
  Version.Free;
end;

procedure TTestVersion.testCompare;
var Version: TVersion;
begin
  Version:= TVersion.Create('2.9.7');
  check(Version.Compare('2.9.7') = EqualsValue, 'Verzie sa maju rovnat.');
  check(Version.Compare('2.11.6') = GreaterThanValue, 'Verzia ma byt vacsia.');
  check(Version.Compare('4.0.2') = GreaterThanValue, 'Verzia ma byt vacsia.');
  check(Version.Compare('1.10.7') = LessThanValue, 'Verzia ma byt mensia.');
  check(Version.Compare('2.3.9') = LessThanValue, 'Verzia ma byt mensia.');
  check(Version.Compare('2.9.6') = LessThanValue, 'Verzia ma byt mensia.');
  Version.Free;
end;


initialization
  TestFramework.RegisterTest(TTestVersion.Suite);
{$ENDIF}

initialization
  TatraDAS_Version:= TVersion.Create(ShortTatraDASVersion);

end.
