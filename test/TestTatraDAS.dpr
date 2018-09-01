program TestTatraDAS;

{$INCLUDE 'delver.inc'}

uses
  {$IFDEF FPC}
  Classes,
    {$IFDEF GUI}
    Interfaces, Forms, GuiTestRunner,
    {$ELSE}
    ConsoleTestRunner,
    {$ENDIF}
  // Delphi
  {$ELSE}
  TestFrameWork,
    {$IFDEF GUI}
    Windows, Forms, SynEdit,
    GUITestRunner,
    {$ELSE}
    TextTestRunner,
    {$ENDIF}
  {$ENDIF}

  // Unit tests
  procmatTests in 'procmatTests.pas',
  StringUtilitiesTests in 'StringUtilitiesTests.pas',
  VersionTests in 'VersionTests.pas',
  CodeSectionTests in 'CodeSectionTests.pas',
  ExportersTests in 'ExportersTests.pas',
  x86DisassemblerTests in 'x86DisassemblerTests.pas',
  // Project units
  TatraDAS_SynEditStringList in '..\source\misc\TatraDAS_SynEditStringList.pas',
  procmat in '..\source\base\procmat.pas',
  ExceptionsUnit in '..\source\base\ExceptionsUnit.pas',
  ExecFileManagerUnit in '..\source\base\ExecFileManagerUnit.pas',
  ExecFileUnit in '..\source\base\ExecFileUnit.pas',
  CliUnit in '..\source\misc\CliUnit.pas',
  SectionUnit in '..\source\base\SectionUnit.pas',
  RegionsUnit in '..\source\base\RegionsUnit.pas',
  ProgressThreads in '..\source\base\ProgressThreads.pas',
  GlobalsUnit in '..\source\base\GlobalsUnit.pas',
  SortingUnit in '..\source\utils\SortingUnit.pas',
  VersionUnit in '..\source\utils\VersionUnit.pas',
  StringRes in '..\source\res\StringRes.pas',
  FilesUnit in '..\source\utils\FilesUnit.pas',
  LoggerUnit in '..\source\utils\LoggerUnit.pas',
  ListsUnit in '..\source\utils\ListsUnit.pas',
  StringUtilities in '..\source\utils\StringUtilities.pas',
  AbstractProgressManager in '..\source\base\AbstractProgressManager.pas',
  ProgressManagerUnit in '..\source\base\ProgressManagerUnit.pas',
  Exporters in '..\source\base\Exporters.pas',
  // Disassembler units
  DisassemblerUnit in '..\source\disasm\DisassemblerUnit.pas',
  DisassemblerUtils in '..\source\disasm\DisassemblerUtils.pas',
  DisassemblerTypes in '..\source\disasm\DisassemblerTypes.pas',
  x86DisassemblerTypes in '..\source\disasm\x86DisassemblerTypes.pas',
  x86Instructions in '..\source\disasm\x86Instructions.pas',
  x86Disassembler in '..\source\disasm\x86Disassembler.pas',
  DisassembledBlocksUnit in '..\source\disasm\DisassembledBlocksUnit.pas',
  CallsAndJumpsTableUnit in '..\source\disasm\CallsAndJumpsTableUnit.pas',
  // Executable formats' units
  CustomFileUnit in '..\source\exefiles\CustomFileUnit.pas',
  MZFileUnit in '..\source\exefiles\MZFileUnit.pas',
  COMFileUnit in '..\source\exefiles\COMFileUnit.pas',
  LEFileUnit in '..\source\exefiles\LEFileUnit.pas',
  LXFileUnit in '..\source\exefiles\LXFileUnit.pas',
  NEFileUnit in '..\source\exefiles\NEFileUnit.pas',
  PEFileUnit in '..\source\exefiles\PEFileUnit.pas',
  ELFFileUnit in '..\source\exefiles\ELFFileUnit.pas',
  // Sections' units
  CodeSectionUnit in '..\source\sections\CodeSectionUnit.pas',
  ExportSectionUnit in '..\source\sections\ExportSectionUnit.pas',
  ImportSectionUnit in '..\source\sections\ImportSectionUnit.pas',
  ResourceSectionUnit in '..\source\sections\ResourceSectionUnit.pas';

{$IFDEF FPC}
{$IFDEF GUI}
begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.
{$ELSE}
type
  TTatraDASTestRunner = class(TTestRunner)
  end;
var
  Application: TTatraDASTestRunner;
begin
  Application := TTatraDASTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'FPCUnit Console test runner';
  Application.Run;
  Application.Free;
end.
{$ENDIF}

// Delphi
{$ELSE}
begin
{$IFDEF GUI}
  Application.Initialize;
  TGUITestRunner.RunTest(RegisteredTests);
{$ELSE}
  TextTestRunner.RunRegisteredTests;
{$ENDIF}
end.
{$ENDIF}

