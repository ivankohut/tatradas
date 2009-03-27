program TestTatraDAS;

{$INCLUDE 'delver.inc'}

uses

  TestFrameWork,
  GUITestRunner,

  {$IFDEF MSWINDOWS}
    {$IFDEF GUI_B}
      Windows,
      Forms,
      SynEdit,
    {$ENDIF}
  {$ENDIF}

  CliUnit,

  VersionUnit in 'VersionUnit.pas',
  StringRes in 'res\StringRes.pas',
  FilesUnit in 'misc\FilesUnit.pas',
  LoggerUnit in 'misc\LoggerUnit.pas',
  MyLists in 'misc\MyLists.pas',

  TatraDASHighlighter in 'res\TatraDASHighlighter.pas',
  TranslatorUnit in 'TranslatorUnit.pas',
  ButtonsX in 'misc\ButtonsX.pas',
  myedits in 'misc\myedits.pas',
  MainFormUnit in 'MainFormUnit.pas' {MainForm},
  HexEditFormUnit in 'forms\HexEditFormUnit.pas' {HexEditForm},
  CalculatorUnit in 'forms\CalculatorUnit.pas' {Calculator},
  OptionsFormUnit in 'forms\OptionsFormUnit.pas' {OptionsForm},
  AdvancedChangingToDataFormUnit in 'forms\AdvancedChangingToDataFormUnit.pas' {AdvancedChangingToDataForm},
  AdvancedDisassembleFormUnit in 'forms\AdvancedDisassembleFormUnit.pas' {AdvancedDisassembleForm},
  InsertCommentFormUnit in 'forms\InsertCommentFormUnit.pas' {InsertCommentForm},
  AboutBoxUnit in 'forms\AboutBoxUnit.pas' {AboutBox},
  UnknownFileFormUnit in 'forms\UnknownFileFormUnit.pas' {UnknownFileFormatForm},
  SaveOptionsFormUnit in 'forms\SaveOptionsFormUnit.pas' {SaveOptionsForm},
  ProgressFormUnit in 'forms\ProgressFormUnit.pas' {ProgressForm},
  GotoLineFormUnit in 'forms\GotoLineFormUnit.pas' {GoToLineForm},
  GotoAddressFormUnit in 'forms\GotoAddressFormUnit.pas' {GoToAddressForm},
  MessageFormUnit in 'forms\MessageFormUnit.pas' {MessageForm},

  // Frames' units
  TabFrameTemplateUnit in 'frames\TabFrameTemplateUnit.pas',
  FileTabFrameUnit in 'frames\FileTabFrameUnit.pas',
  CodeTabFrameUnit in 'frames\CodeTabFrameUnit.pas',
  ImportTabFrameUnit in 'frames\ImportTabFrameUnit.pas',
  ExportTabFrameUnit in 'frames\ExportTabFrameUnit.pas',
  ResourceTabFrameUnit in 'frames\ResourceTabFrameUnit.pas',

  // Disassembler units
  DisassemblerUnit in 'disasm\DisassemblerUnit.pas',
  DisassemblerUtils in 'disasm\DisassemblerUtils.pas',
  DisassemblerTypes in 'disasm\DisassemblerTypes.pas',
  x86DisassemblerTypes in 'disasm\x86DisassemblerTypes.pas',
  x86Instructions in 'disasm\x86Instructions.pas',
  x86Disassembler in 'disasm\x86Disassembler.pas',
  DisassembledBlocksUnit in 'disasm\DisassembledBlocksUnit.pas',
  CallsAndJumpsTableUnit in 'disasm\CallsAndJumpsTableUnit.pas',
  
// Executable formats' units
  CustomFileUnit in 'exefiles\CustomFileUnit.pas',
  MZFileUnit in 'exefiles\MZFileUnit.pas',
  COMFileUnit in 'exefiles\COMFileUnit.pas',
  LEFileUnit in 'exefiles\LEFileUnit.pas',
  LXFileUnit in 'exefiles\LXFileUnit.pas',
  NEFileUnit in 'exefiles\NEFileUnit.pas',
  PEFileUnit in 'exefiles\PEFileUnit.pas',
  ELFFileUnit in 'exefiles\ELFFileUnit.pas',

// Sections' units
  CodeSectionUnit in 'sections\CodeSectionUnit.pas',
  ExportSectionUnit in 'sections\ExportSectionUnit.pas',
  ImportSectionUnit in 'sections\ImportSectionUnit.pas',
  ResourceSectionUnit in 'sections\ResourceSectionUnit.pas';


//{$R *.res}

begin
  Application.Initialize;
  TGUITestRunner.RunTest(RegisteredTests);
end.
