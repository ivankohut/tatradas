unit ExportersTests;

interface

uses
  TatraDASTestFramework;

type
  TExportersTests = class(TTestCase)
  protected
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestNasmLineExport;
    procedure TestNasmIsReferenceFromCode;
    //procedure TestNasmDataExportStream;
  end;


implementation

uses
  SysUtils, Classes, Types, StrUtils,
  Exporters, ProgressManagerUnit, procmat;

type
  TExporterChild = class(TExporter)
  end;

var
  ProgressManager: TProgressManager;

{ TExportersTests }


procedure TExportersTests.Setup;
begin
  inherited;
  ProgressManager := TProgressManager.Create(nil);
end;



procedure TExportersTests.TearDown;
begin
  inherited;
  FreeAndNil(ProgressManager);
end;


{
procedure TExportersTests.TestNasmDataExportStream;
var
  sl: TStrings;
  Stream: TStream;
  ResStrings: TStrings;
  Expected: TStrings;
begin
  sl := TStringList.Create;
  sl.Add('0040E000 20                       byte 0x20 '' ''');
  sl.Add('Jump from bla bla');
  sl.Add('Imported bla bla');
  sl.Add('0040E000 20                       byte 0x20 '' ''');
  sl.Add('0040E000 20                       byte 0x20 '' ''');

  Expected := TStringList.Create;
  Expected.Add('BITS 32');
  Expected.Add('');
  Expected.Add(cNasmLineIndent + 'db 0x20');
  Expected.Add(';Jump from bla bla');
  Expected.Add(';Imported bla bla');
  Expected.Add('_0x0040E000:');
  Expected.Add(cNasmLineIndent + 'db 0x20');
  Expected.Add(cNasmLineIndent + 'db 0x20');
  Expected.Add('');

  Stream := TMemoryStream.Create;
  TExporterChild.ExportSectionToNASM(sl, true, Stream);
  ResStrings := TStringList.Create;
  Stream.Position := 0;
  ResStrings.LoadFromStream(Stream);

  Check(ResStrings.Equals(Expected));

  ResStrings.Free;
  Expected.Free;
  sl.Free;
end;
}


procedure TExportersTests.TestNasmIsReferenceFromCode;
begin
  Check(not TExporterChild.NasmIsReferenceFromCode(''));
  Check(not TExporterChild.NasmIsReferenceFromCode(';sf sda'));
  Check(not TExporterChild.NasmIsReferenceFromCode('004010C6 8BF7                     MOV ESI,EDI'));
  Check(not TExporterChild.NasmIsReferenceFromCode('Program entry point'));
  Check(not TExporterChild.NasmIsReferenceFromCode('Imported'));
  Check(not TExporterChild.NasmIsReferenceFromCode('Exported'));
  Check(TExporterChild.NasmIsReferenceFromCode('Jump'));
  Check(TExporterChild.NasmIsReferenceFromCode('Call'));
  Check(TExporterChild.NasmIsReferenceFromCode('Loop'));
end;



procedure TExportersTests.TestNasmLineExport;
begin
  Check(TExporterChild.ExportLineToNASM('') = '');
  Check(TExporterChild.ExportLineToNASM('004010C6 8BF7                     MOV ESI,EDI') = cNasmLineIndent + 'MOV ESI,EDI');
  Check(TExporterChild.ExportLineToNASM('Imported function ''GetCommandLineA'' from ''KERNEL32.dll'' used') = ';Imported function ''GetCommandLineA'' from ''KERNEL32.dll'' used');
  Check(TExporterChild.ExportLineToNASM('Exported bla bla') = ';Exported bla bla');
  Check(TExporterChild.ExportLineToNASM('Program entry point') = ';Program entry point');
  Check(TExporterChild.ExportLineToNASM('Jump bla bla') = ';Jump bla bla');
  Check(TExporterChild.ExportLineToNASM('Call bla bla') = ';Call bla bla');
  Check(TExporterChild.ExportLineToNASM('Loop bla bla') = ';Loop bla bla');
        
  // DB and friends
  Check(TExporterChild.ExportLineToNASM('0040E000 20                       byte 0x20 '' ''') = cNasmLineIndent + 'db 0x20');
  Check(TExporterChild.ExportLineToNASM('0040E001 836C                     word 0x6C83') = cNasmLineIndent + 'dw 0x6C83');
  Check(TExporterChild.ExportLineToNASM('0040E003 FFFF25BC                 dword 0xBC25FFFF') = cNasmLineIndent + 'dd 0xBC25FFFF');
  Check(TExporterChild.ExportLineToNASM('0040E007 B140008BC007B8B4         qword 0xB4B807C08B0040B1') = cNasmLineIndent + 'dq 0xB4B807C08B0040B1');
  Check(TExporterChild.ExportLineToNASM('0040E00F 83                       byte -0x7D ''ƒ''') = cNasmLineIndent + 'db -0x7D');
  Check(TExporterChild.ExportLineToNASM('0040E010 0C32                     word 0x320C') = cNasmLineIndent + 'dw 0x320C');
  Check(TExporterChild.ExportLineToNASM('0040E012 C8B0ACA8                 dword -0x57534F38') = cNasmLineIndent + 'dd -0x57534F38');
  Check(TExporterChild.ExportLineToNASM('0040E016 A40C32C820A09C98         qword -0x67635FDF37CDF35C') = cNasmLineIndent + 'dq -0x67635FDF37CDF35C');
  Check(TExporterChild.ExportLineToNASM('0040E01E 32C82083                 single -4.724959E-37') = cNasmLineIndent + 'dd -4.724959E-37');
  Check(TExporterChild.ExportLineToNASM('0040E022 94908CC820830C5C         double 2.59047310922832E135') = cNasmLineIndent + 'dq 2.59047310922832E135');
  Check(TExporterChild.ExportLineToNASM('0040E02A 8884                     word -0x7B78') = cNasmLineIndent + 'dw -0x7B78');
  Check(TExporterChild.ExportLineToNASM('0040E02C 20830C32807C4C830C32     extended 1.07878820043909804E-1075') = cNasmLineIndent + 'dt 1.07878820043909804E-1075');
  Check(TExporterChild.ExportLineToNASM('00401005 bytes: 00000005(hex)     pstring ''Byte''') = cNasmLineIndent + 'db 0x04,''Byte''');
  Check(TExporterChild.ExportLineToNASM('00401005 bytes: 00000005(hex)     cstring ''Byte''') = cNasmLineIndent + 'db ''Byte'',0x00');
end;




initialization
  RegisterTest(TExportersTests);

end.
