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

  procmatTests in 'procmatTests.pas',
  StringUtilitiesTests in 'StringUtilitiesTests.pas',
  VersionTests in 'VersionTests.pas',
  CodeSectionTests in 'CodeSectionTests.pas',
  ExportersTests in 'ExportersTests.pas',

  {$IFDEF MSWINDOWS}
    {$IFDEF GUI_B}
      Windows,
      Forms,
      SynEdit,
    {$ENDIF}
  {$ENDIF}

  TatraDAS_SynEditStringList in '..\source\misc\TatraDAS_SynEditStringList.pas',

  procmat in '..\source\procmat.pas',
  ExecFileManagerUnit in '..\source\ExecFileManagerUnit.pas',
  ExecFileUnit in '..\source\ExecFileUnit.pas',
  CliUnit in '..\source\CliUnit.pas',
  SectionUnit in '..\source\SectionUnit.pas',
  RegionsUnit in '..\source\RegionsUnit.pas',
  ProgressThreads in '..\source\ProgressThreads.pas',
  SortingUnit in '..\source\SortingUnit.pas',

  VersionUnit in '..\source\VersionUnit.pas',
  StringRes in '..\source\res\StringRes.pas',
  FilesUnit in '..\source\misc\FilesUnit.pas',
  LoggerUnit in '..\source\misc\LoggerUnit.pas',
  ListsUnit in '..\source\misc\ListsUnit.pas',
  StringUtilities in '..\source\StringUtilities.pas',
  ProgressManagerUnit in '..\source\ProgressManagerUnit.pas',

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

  Exporters in '..\source\Exporters.pas',

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


//{$R *.res}

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
end.
