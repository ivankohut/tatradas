unit procmatTests;

interface

uses
  TatraDASTestFramework;

type
  TprocmatTests = class(TTestCase)
  published
    procedure TestWriteLnToStream;
  end;


implementation

uses
  SysUtils, Classes, Types, StrUtils,
  procmat;

{ TprocmatTests }


procedure TprocmatTests.TestWriteLnToStream;
var
  stream: TStream;
  sl: TStrings;
begin
  stream := TMemoryStream.Create;
  WriteLnToStream(stream, '');
  WriteLnToStream(stream, 'aaa');
  WriteLnToStream(stream, 'bbb'#13#10'ccc');
  WriteLnToStream(stream);
  stream.Position := 0;
  sl := TStringList.Create;
  sl.LoadFromStream(stream);
  Check(sl[0] = '');
  Check(sl[1] = 'aaa');
  Check(sl[2] = 'bbb');
  Check(sl[3] = 'ccc');
  Check(sl[4] = '');
end;

initialization
  RegisterTest(TprocmatTests);

end.