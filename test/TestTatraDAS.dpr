program TestTatraDAS;

{$INCLUDE 'delver.inc'}

uses
  {$IFDEF FPC}
    Classes, ConsoleTestRunner,
  {$ELSE}
    TestFrameWork,
    {$IFDEF CONSOLE}
      TextTestRunner,
    {$ELSE}
      GUITestRunner,
    {$ENDIF}
  {$ENDIF}

  // Unit tests
  procmatTests in 'procmatTests.pas',
  StringUtilitiesTests in 'StringUtilitiesTests.pas',
  VersionTests in 'VersionTests.pas',
  CodeSectionTests in 'CodeSectionTests.pas',
  ExportersTests in 'ExportersTests.pas',
  x86DisassemblerTests in 'x86DisassemblerTests.pas',

  {$IFDEF MSWINDOWS}
    {$IFDEF GUI_B}
      Windows,
      Forms,
      SynEdit,
    {$ENDIF}
  {$ENDIF}

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

(*
  TatraDASHighlighter in '..\source\res\TatraDASHighlighter.pas',
  TranslatorUnit in '..\source\TranslatorUnit.pas',
  ButtonsX in '..\source\misc\ButtonsX.pas',
  myedits in '..\source\misc\myedits.pas',
  MainFormUnit in '..\source\MainFormUnit.pas' {MainForm},
  HexEditFormUnit in '..\source\forms\HexEditFormUnit.pas' {HexEditForm},
  CalculatorUnit in '..\source\forms\CalculatorUnit.pas' {Calculator},
  OptionsFormUnit in '..\source\forms\OptionsFormUnit.pas' {OptionsForm},
  AdvancedChangingToDataFormUnit in '..\source\forms\AdvancedChangingToDataFormUnit.pas' {AdvancedChangingToDataForm},
  AdvancedDisassembleFormUnit in '..\source\forms\AdvancedDisassembleFormUnit.pas' {AdvancedDisassembleForm},
  InsertCommentFormUnit in '..\source\forms\InsertCommentFormUnit.pas' {InsertCommentForm},
  AboutBoxUnit in '..\source\forms\AboutBoxUnit.pas' {AboutBox},
  UnknownFileFormUnit in '..\source\forms\UnknownFileFormUnit.pas' {UnknownFileFormatForm},
  SaveOptionsFormUnit in '..\source\forms\SaveOptionsFormUnit.pas' {SaveOptionsForm},
  ProgressFormUnit in '..\source\forms\ProgressFormUnit.pas' {ProgressForm},
  GotoLineFormUnit in '..\source\forms\GotoLineFormUnit.pas' {GoToLineForm},
  GotoAddressFormUnit in '..\source\forms\GotoAddressFormUnit.pas' {GoToAddressForm},
  MessageFormUnit in '..\source\forms\MessageFormUnit.pas' {MessageForm},

  // Frames' units
  TabFrameTemplateUnit in '..\source\frames\TabFrameTemplateUnit.pas',
  FileTabFrameUnit in '..\source\frames\FileTabFrameUnit.pas',
  CodeTabFrameUnit in '..\source\frames\CodeTabFrameUnit.pas',
  ImportTabFrameUnit in '..\source\frames\ImportTabFrameUnit.pas',
  ExportTabFrameUnit in '..\source\frames\ExportTabFrameUnit.pas',
  ResourceTabFrameUnit in '..\source\frames\ResourceTabFrameUnit.pas',
*)

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


{$R *.res}

{$IFDEF FPC}
type
  TFpcTestRunner = class(TTestRunner)
  end;
var
  Application: TFpcTestRunner;

{$ENDIF}


begin
  {$IFDEF MSWINDOWS}
    {$IFDEF GUI_B}
      Application.Initialize;
      TGUITestRunner.RunTest(RegisteredTests);
    {$ENDIF}
    {$IFDEF CONSOLE}
      TextTestRunner.RunRegisteredTests;
    {$ENDIF}
  {$ENDIF}  
  {$IFDEF FPC}
    Application := TFpcTestRunner.Create(nil);
    Application.Initialize;
      //Application.CreateForm(TGuiTestRunner, TestRunner);
    Application.Run;
    Application.Free;
  {$ENDIF}
end.
