unit CodeSectionTests;

interface

uses
  TatraDASTestFramework,
  CodeSectionUnit;

type

  TCodeSectionChild = class(TCodeSection)
  end;

  TCodeSectionTests = class(TTestCase)
  published
    procedure TestGetLineAddress;
    procedure TestGetLineBytes;
    procedure TestReplaceFirstLastLine;
    procedure TestGetLineData;
    procedure TestPrepareInnerReplaceLines;
    procedure TestGetLineType;
    procedure TestGetReplacingLines;
    procedure TestGetTargetAddress;
  end;


implementation

uses
  Types, Classes, SysUtils, LoggerUnit,
  procmat;

procedure TCodeSectionTests.TestGetLineAddress;
begin
  Check(GetLineAddress('12345678faklwejfsjfdioaweoijklsadjf') = $12345678);
end;



procedure TCodeSectionTests.TestGetLineBytes;
begin
  // Normalne instrukcie alebo jednoduche data
  Check(GetLineBytes('12345678 A8') = 1);
  Check(GetLineBytes('12345678 A836 qwwjqof sdkjfha isdfhu sid') = 2);

  // Retazce
  Check(GetLineBytes('1B00F4F8 bytes: 0000010A(hex)') = 266);
  Check(GetLineBytes('1B00F4F8 bytes: 0000000A(hex)     cstring ''AAAAAAAAAA''') = 10);
end;



procedure TCodeSectionTests.TestGetLineData;
var
  LineData: TByteDynArray;
begin
  // Riadky s datami
  LineData := GetLineData('12345678 A8');
  Check((Length(LineData) = 1) and (LineData[0] = $A8));
  LineData := GetLineData('12345678 A836 qwwjqof sdkjfha isdfhu sid');
  Check((Length(LineData) = 2) and (LineData[0] = $A8) and (LineData[1] = $36));
  LineData := GetLineData('1B00F4F8 bytes: 00000005(hex)     cstring ''abcd''');
  Check((Length(LineData) = 5) and (LineData[0] = Ord('a')) and (LineData[1] = Ord('b')) and (LineData[2] = Ord('c')) and (LineData[3] = Ord('d')) and (LineData[4] = 0));
  LineData := GetLineData('1B00F4F8 bytes: 00000005(hex)     pstring ''abcd''');
  Check((Length(LineData) = 5) and (LineData[0] = 4) and (LineData[1] = Ord('a')) and (LineData[2] = Ord('b')) and (LineData[3] = Ord('c')) and (LineData[4] = Ord('d')));

  // Riadky bez dat
  LineData := GetLineData('Jump from 0x12345678');
  Check((Length(LineData) = 0));
  LineData := GetLineData('; asda sd a');
  Check((Length(LineData) = 0));
  LineData := GetLineData('');
  Check((Length(LineData) = 0));
end;



procedure TCodeSectionTests.TestGetLineType;
begin
  Check(GetLineType('') = ltEmpty);
  Check(GetLineType(';bla bla') = ltComment);
  Check(GetLineType('Imported bla bla') = ltImportRef);
  Check(GetLineType('Exported bla bla') = ltExportRef);
  Check(GetLineType('Program exntry point') = ltEntryPointRef);
  Check(GetLineType('Call from 0x00000001') = ltCallRef);
  Check(GetLineType('Jump from 0x00000001') = ltJumpRef);
  Check(GetLineType('Loop from 0x00000001') = ltLoopRef);
  Check(GetLineType('CADE0012 bla bla') = ltInstruction);
  Check(GetLineType('05782EF7 bla bla') = ltInstruction);
end;



procedure TCodeSectionTests.TestPrepareInnerReplaceLines;
var
  OldLines, NewLines, ResultLines: TStrings;
  OldLastLineIndex: Integer;
  i: Integer;
begin
  // 1. Adresy zaciatocnej starej a zaciatocnej novej instrukcie su rozne
  OldLines := TStringList.Create;
  OldLines.Add('');
  OldLines.Add(';');
  OldLines.Add('10000000 21222324');
  OldLines.Add('Jump from 0x12345678');
  OldLines.Add('10000004 2122');
  OldLines.Add('10000006 212223242526');

  NewLines := TStringList.Create;
  NewLines.Add('; janko hrasko');
  NewLines.Add('10000003 3132');
  NewLines.Add('');
  NewLines.Add('10000005 313233');

  ResultLines := TCodeSectionChild.PrepareInnerReplaceLines(4, OldLines, NewLines, OldLastLineIndex);
  Check(ResultLines.Count = 4);
//  Check(ResultLines[0]  = 'Jump from 0x12345678');
  Check(ResultLines[0]  = '; janko hrasko');
  Check(ResultLines[1]  = '10000003 3132');
  Check(ResultLines[2]  = '');
  Check(ResultLines[3]  = '10000005 313233');
  ResultLines.Free;

  // 2. Adresy zaciatocnej starej a zaciatocnej novej instrukcie su rovnake
  OldLines := TStringList.Create;
  OldLines.Add('');
  OldLines.Add(';');
  OldLines.Add('10000000 212223');
  OldLines.Add('Jump from 0x12345678');
  OldLines.Add('10000003 2122');
  OldLines.Add('10000006 212223242526');

  NewLines := TStringList.Create;
  NewLines.Add('; janko hrasko');
  NewLines.Add('10000003 3132');
  NewLines.Add('');
  NewLines.Add('10000005 313233');

  ResultLines := TCodeSectionChild.PrepareInnerReplaceLines(4, OldLines, NewLines, OldLastLineIndex);
  Check(ResultLines.Count = 4);
//  Check(ResultLines[0]  = 'Jump from 0x12345678');
  Check(ResultLines[0]  = '; janko hrasko');
  Check(ResultLines[1]  = '10000003 3132');
  Check(ResultLines[2]  = '');
  Check(ResultLines[3]  = '10000005 313233');
  ResultLines.Free;


  // 3.
 OldLines := TStringList.Create;
  with OldLines do begin
    Add('00000000 AA');
    Add(';aaa');
    Add('00000001 BB');
  end;

  NewLines := TStringList.Create;
  NewLines.Add('00000000 FF');

  ResultLines := TCodeSectionChild.PrepareInnerReplaceLines(0, OldLines, NewLines, OldLastLineIndex);
  for i := 0 to ResultLines.Count - 1 do
    Logger.Debug(ResultLines[i]);
  Check(ResultLines.Count = 1);
  Check(ResultLines[0]  = '00000000 FF');
end;




procedure TCodeSectionTests.TestReplaceFirstLastLine;
var
  NewLines: TStrings;
begin
  // First line
  NewLines := TCodeSectionChild.ReplaceFirstLastLine('10000000 21222324', $10000000, True);
  Check(NewLines.Count = 0);

  NewLines := TCodeSectionChild.ReplaceFirstLastLine('10000000 21222324', $10000003, True);
  Check(NewLines.Count = 3);
  Check(NewLines[0] = '10000000 21                       byte 0x21 ''' + Char($21) + '''');
  Check(NewLines[1] = '10000001 22                       byte 0x22 ''' + Char($22) + '''');
  Check(NewLines[2] = '10000002 23                       byte 0x23 ''' + Char($23) + '''');

  // Last line
  NewLines := TCodeSectionChild.ReplaceFirstLastLine('10000000 21222324', $10000000, False);
  Check(NewLines.Count = 1);
  Check(NewLines[0] = '10000000 21222324');

  NewLines := TCodeSectionChild.ReplaceFirstLastLine('10000000 21222324', $10000001, False);
  Check(NewLines.Count = 3);
  Check(NewLines[0] = '10000001 22                       byte 0x22 ''' + Char($22) + '''');
  Check(NewLines[1] = '10000002 23                       byte 0x23 ''' + Char($23) + '''');
  Check(NewLines[2] = '10000003 24                       byte 0x24 ''' + Char($24) + '''');

  NewLines := TCodeSectionChild.ReplaceFirstLastLine('10000000 21222324', $10000004, False);
  Check(NewLines.Count = 0);
end;



procedure TCodeSectionTests.TestGetReplacingLines;
var
  section: TCodeSectionChild;
  NewLines, ResultLines: TStrings;
  LastLineIndex, i: Integer;
begin
  // 1.

  section := TCodeSectionChild.Create;
  SetLength(section.DisassemblerMap, 10);
  section.SetMemOffset($10000000);
  section.CodeStream := TMyMemoryStream.Create;
  section.SetDisassembled(TTatraDASStringList.Create);
  with section.Disassembled do begin
    Add('');
    Add(';');
    Add('10000000 21222324');
    Add('Jump from 0x12345678');
    Add('10000004 2122');
    Add('10000006 212223242526');
  end;

  NewLines := TStringList.Create;
  NewLines.Add('; janko hrasko');
  NewLines.Add('10000003 3132');
  NewLines.Add('');
  NewLines.Add('10000005 313233');


  ResultLines := section.GetReplacingLines(2, 3, 5, NewLines, LastLineIndex);
  for i := 0 to ResultLines.Count - 1 do
    Logger.Debug(ResultLines[i]);

  Check(ResultLines.Count = 12);
  Check(ResultLines[0]  = '10000000 21                       byte 0x21 ''' + Char($21) + '''');
  Check(ResultLines[1]  = '10000001 22                       byte 0x22 ''' + Char($22) + '''');
  Check(ResultLines[2]  = '10000002 23                       byte 0x23 ''' + Char($23) + '''');
  Check(ResultLines[3]  = 'Jump from 0x12345678');
  Check(ResultLines[4]  = '; janko hrasko');
  Check(ResultLines[5]  = '10000003 3132');
  Check(ResultLines[6]  = '');
  Check(ResultLines[7]  = '10000005 313233');
  Check(ResultLines[8] = '10000008 23                       byte 0x23 ''' + Char($23) + '''');
  Check(ResultLines[9] = '10000009 24                       byte 0x24 ''' + Char($24) + '''');
  Check(ResultLines[10] = '1000000A 25                       byte 0x25 ''' + Char($25) + '''');
  Check(ResultLines[11] = '1000000B 26                       byte 0x26 ''' + Char($26) + '''');
  ResultLines.Free;
//  section.Free;


  // 2.

  section := TCodeSectionChild.Create;
  SetLength(section.DisassemblerMap, 10);
  section.SetMemOffset(0);
  section.CodeStream := TMyMemoryStream.Create;
  section.SetDisassembled(TTatraDASStringList.Create);
  with section.Disassembled do begin
    Add('00000000 AA');
    Add(';aaa');
    Add('00000001 BB');
  end;

  NewLines := TStringList.Create;
  NewLines.Add('00000000 FF');

  ResultLines := section.GetReplacingLines(0, 0, 1, NewLines, LastLineIndex);
  for i := 0 to ResultLines.Count - 1 do
    Logger.Debug(ResultLines[i]);

  Check(ResultLines.Count = 1);
  Check(ResultLines[0]  = '00000000 FF');

  // 3.

  section := TCodeSectionChild.Create;
  SetLength(section.DisassemblerMap, 10);
  section.SetMemOffset(0);
  section.CodeStream := TMyMemoryStream.Create;
  section.SetDisassembled(TTatraDASStringList.Create);
  with section.Disassembled do begin
    Add('00000000 bytes: 00000005(hex)     pstring ''Byte''');
  end;

  NewLines := TStringList.Create;
  NewLines.Add('00000000 04                       byte 0x04');

  ResultLines := section.GetReplacingLines(0, 0, 1, NewLines, LastLineIndex);
  for i := 0 to ResultLines.Count - 1 do
    Logger.Debug(ResultLines[i]);

  Check(ResultLines.Count = 5);
  Check(ResultLines[0]  = '00000000 04                       byte 0x04');
  Check(ResultLines[1]  = '00000001 42                       byte 0x42 ''B''');
  Check(ResultLines[2]  = '00000002 79                       byte 0x79 ''y''');
  Check(ResultLines[3]  = '00000003 74                       byte 0x74 ''t''');
  Check(ResultLines[4]  = '00000004 65                       byte 0x65 ''e''');
end;



procedure TCodeSectionTests.TestGetTargetAddress;
var
  Address: cardinal;
begin
  Check(GetTargetAddress('00000000 04                       CALL 0x00001234', Address));
  Check(Address = $00001234);
  Check(GetTargetAddress('00000000 04                       LOOP 0x00001235', Address));
  Check(Address = $00001235);
  Check(GetTargetAddress('00000000 04                       LOOPE 0x00001235', Address));
  Check(Address = $00001235);
  Check(GetTargetAddress('00000000 04                       LOOPNE 0x00001235', Address));
  Check(Address = $00001235);
  Check(GetTargetAddress('00000000 04                       JMP 0x00001236', Address));
  Check(Address = $00001236);
  Check(GetTargetAddress('00000000 04                       JE 0x00001237', Address));
  Check(Address = $00001237);

  Check(not GetTargetAddress('00000000 04                       INC 0x00001237', Address));
  Check(not GetTargetAddress('00000000 04                       CALL EAX', Address));
  Check(not GetTargetAddress('00000000 04                       CALL [EAX]', Address));
  Check(not GetTargetAddress('00000000 04                       CALL [0x00001237]', Address));
end;



initialization
  RegisterTest(TCodeSectionTests);

end.
