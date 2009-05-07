unit VersionTests;

interface

uses
  TatraDASTestFramework;

type
  TTestVersion = class(TTestCase)
  published
    procedure testCreate;
    procedure testCompare;
  end;


implementation

uses
  VersionUnit, Math, SysUtils, Types;


procedure TTestVersion.testCreate;
var
  Version: TVersion;
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
var
  Version: TVersion;
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
  RegisterTest(TTestVersion);

end.
