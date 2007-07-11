program TestTatraDAS;

{$INCLUDE 'delver.inc'}

uses
  Forms,

  TestFrameWork in 'dunit\TestFrameWork.pas',
  GUITestRunner in 'dunit\GUITestRunner.pas',
  StringRes in 'misc\StringRes.pas',

  TatraDASHighlighter in 'misc\TatraDASHighlighter.pas',
  Languages in 'misc\Languages.pas',
  ButtonsX in 'misc\ButtonsX.pas',
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
  GotoAddressFormUnit in 'forms\GotoAddressFormUnit.pas' {GoToAddressForm},

  // Frames' units
  TabFrameTemplateUnit in 'frames\TabFrameTemplateUnit.pas',
  FileTabFrameUnit in 'frames\FileTabFrameUnit.pas',
  CodeTabFrameUnit in 'frames\CodeTabFrameUnit.pas',
  ImportTabFrameUnit in 'frames\ImportTabFrameUnit.pas',
  ExportTabFrameUnit in 'frames\ExportTabFrameUnit.pas',
  ResourceTabFrameUnit in 'frames\ResourceTabFrameUnit.pas',

// Executable formats' units
  UnknownFileUnit in 'exefiles\UnknownFileUnit.pas',
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
