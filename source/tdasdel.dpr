program TatraDAS;

{$INCLUDE 'delver.inc'}

{%File 'misc\tatradas.rc'}
{%File 'MISC\TatraDASHighlighter.msg'}
{$R 'misc\tatradas.res' 'misc\tatradas.rc'}

uses
  Windows,
  Forms,
  SynEdit,
  SysUtils,
  procmat in 'procmat.pas',
  disassembler in 'disassembler.pas',
  ExecFileUnit in 'ExecFileUnit.pas',
  SectionUnit in 'SectionUnit.pas',
  StringRes in 'misc\StringRes.pas',
  TatraDASHighlighter in 'misc\TatraDASHighlighter.pas',
  ButtonsX in 'misc\ButtonsX.pas',
  Languages in 'misc\Languages.pas',
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
  GotoAddressFormUnit in 'forms\GotoAddressFormUnit.pas' {GoToAddressForm},
  UnknownFileUnit in 'exefiles\UnknownFileUnit.pas',
  MZFileUnit in 'exefiles\MZFileUnit.pas',
  COMFileUnit in 'exefiles\COMFileUnit.pas',
  LEfileUnit in 'exefiles\LEfileUnit.pas',
  LXFileUnit in 'exefiles\LXFileUnit.pas',
  NEFileUnit in 'exefiles\NEFileUnit.pas',
  PEFileUnit in 'exefiles\PEFileUnit.pas',
  ELFFileUnit in 'exefiles\ELFFileUnit.pas',
  CodeSectionUnit in 'sections\CodeSectionUnit.pas',
  ExportSectionUnit in 'sections\ExportSectionUnit.pas',
  ImportSectionUnit in 'sections\ImportSectionUnit.pas',
  ResourceSectionUnit in 'sections\ResourceSectionUnit.pas',
  myedits in 'myedits.pas',
  TabFrameTemplateUnit in 'frames\TabFrameTemplateUnit.pas' {TabFrameTemplate: TFrame},
  CodeTabFrameUnit in 'frames\CodeTabFrameUnit.pas' {CodeTabFrame: TFrame},
  FileTabFrameUnit in 'frames\FileTabFrameUnit.pas' {FileTabFrame: TFrame},
  ProgressFormUnit in 'forms\ProgressFormUnit.pas' {ProgressForm},
  UProgressThread in 'UProgressThread.pas',
  ExecFileManagerUnit in 'ExecFileManagerUnit.pas',
  FileSectionUnit in 'FileSectionUnit.pas',
  ImportTabFrameUnit in 'frames\ImportTabFrameUnit.pas' {ImportTabFrame: TFrame},
  ResourceTabFrameUnit in 'frames\ResourceTabFrameUnit.pas' {ResourceTabFrame: TFrame},
  DummySectionUnit in 'sections\DummySectionUnit.pas',
  ExportTabFrameUnit in 'frames\ExportTabFrameUnit.pas' {ExportTabFrame: TFrame};

begin
  Application.Initialize;
  Application.Title := 'TatraDAS';
  Application.Icon.Handle:=LoadIcon(hinstance,'mainicon');
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TUnknownFileFormatForm, UnknownFileFormatForm);
  Application.CreateForm(TSaveOptionsForm, SaveOptionsForm);
  Application.CreateForm(TAdvancedChangingToDataForm, AdvancedChangingToDataForm);
  Application.CreateForm(TAdvancedDisassembleForm, AdvancedDisassembleForm);
  Application.CreateForm(TInsertCommentForm, InsertCommentForm);
  Application.CreateForm(TCalculator, Calculator);
  Application.CreateForm(TOptionsForm, OptionsForm);
  Application.CreateForm(THexEditForm, HexEditForm);
  Application.CreateForm(TGoToAddressForm, GoToAddressForm);
  Application.CreateForm(TProgressForm, ProgressForm);
  Application.Run;
end.
