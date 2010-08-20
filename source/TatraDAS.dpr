program TatraDAS;

{$INCLUDE 'delver.inc'}

{%File 'TatraDASHighlighter.msg'}
{%File 'res\tatradas.rc'}

{$IFDEF LCL}
  {$IFDEF MSWINDOWS}
    {$R 'res\tatradas.res' 'res\tatradas.rc'}
  {$ENDIF}
{$ENDIF}

{$IFDEF LCL}
  {$DEFINE GUI}
{$ENDIF}

uses
  {$IFDEF FPC}
    {$IFDEF LINUX}
      cthreads,
    {$ENDIF}
  {$ENDIF}

  SysUtils,
  Classes,
  StrUtils,

  {$IFDEF GUI}
    Interfaces, // this includes the LCL widgetset
    LResources,
    
    Forms,
    Dialogs,
  {$ENDIF}
  
  // Base units
  AbstractProgressManager in 'base\AbstractProgressManager.pas',
  ExceptionsUnit in 'base\ExceptionsUnit.pas',
  ExecFileManagerUnit in 'base\ExecFileManagerUnit.pas',
  ExecFileUnit in 'base\ExecFileUnit.pas',
  Exporters in 'base\Exporters.pas',
  GlobalsUnit in 'base\GlobalsUnit.pas',
  procmat in 'base\procmat.pas',
  ProgressManagerUnit in 'base\ProgressManagerUnit.pas',
  ProgressThreads in 'base\ProgressThreads.pas',
  RegionsUnit in 'base\RegionsUnit.pas',
  SectionUnit in 'base\SectionUnit.pas',

  // Utilities units
  StringUtilities in 'utils\StringUtilities.pas',
  SortingUnit in 'utils\SortingUnit.pas',
  FilesUnit in 'utils\FilesUnit.pas',
  ListsUnit in 'utils\ListsUnit.pas',
  LoggerUnit in 'utils\LoggerUnit.pas',

  // Misc. units
  StringRes in 'res\StringRes.pas',

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
  MZFileUnit in 'exefiles\MZFileUnit.pas',
  COMFileUnit in 'exefiles\COMFileUnit.pas',
  LEFileUnit in 'exefiles\LEFileUnit.pas',
  LXFileUnit in 'exefiles\LXFileUnit.pas',
  NEFileUnit in 'exefiles\NEFileUnit.pas',
  PEFileUnit in 'exefiles\PEFileUnit.pas',
  ELFFileUnit in 'exefiles\ELFFileUnit.pas',
  CustomFileUnit in 'exefiles\CustomFileUnit.pas',

  // Sections' units
  CodeSectionUnit in 'sections\CodeSectionUnit.pas',
  ExportSectionUnit in 'sections\ExportSectionUnit.pas',
  ImportSectionUnit in 'sections\ImportSectionUnit.pas',

  {$IFDEF GUI}
    TatraDASHighlighter in 'res\TatraDASHighlighter.pas',
  //  ButtonsX in 'misc\ButtonsX.pas',
    myedits in 'misc\myedits.pas',
    TranslatorUnit in 'misc\TranslatorUnit.pas',
    VersionUnit in 'utils\VersionUnit.pas',

    // Forms' units
    MainFormUnit in 'forms\MainFormUnit.pas' {MainForm},
  //  HexEditFormUnit in 'forms\HexEditFormUnit.pas' {HexEditForm},
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
    ExportTabFrameUnit in 'frames\ExportTabFrameUnit.pas';

  {$ELSE}
    CliUnit in 'misc\CliUnit.pas',
    TatraDAS_SynEditStringList in 'misc\TatraDAS_SynEditStringList.pas';
  {$ENDIF}


{$IFDEF GUI}

//{$IFDEF WINDOWS}{$R TatraDAS.rc}{$ENDIF}

begin
//  {$I TatraDAS.lrs}
  Application.Initialize;
//  Application.Icon.Handle:=LoadIcon(hinstance,'mainicon');
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.CreateForm(TUnknownFileFormatForm, UnknownFileFormatForm);
  Application.CreateForm(TSaveOptionsForm, SaveOptionsForm);
  Application.CreateForm(TAdvancedChangingToDataForm, AdvancedChangingToDataForm);
  Application.CreateForm(TAdvancedDisassembleForm, AdvancedDisassembleForm);
  Application.CreateForm(TInsertCommentForm, InsertCommentForm);
  Application.CreateForm(TCalculator, Calculator);
  Application.CreateForm(TOptionsForm, OptionsForm);
//  Application.CreateForm(THexEditForm, HexEditForm);
  Application.CreateForm(TProgressForm, ProgressForm);
  Application.CreateForm(TGotoLineForm, GotoLineForm);
  Application.CreateForm(TGoToAddressForm, GoToAddressForm);

  if not Translator.ChangeLanguage(MainForm.sINI.ReadString('Settings', 'Language', 'en')) then begin
     DisplayMessage(NoLanguageFilesStr, mtError, [mbOk]);
     Exit;
  end;
  OptionsForm.LoadSettings(MainForm.sINI);

  Application.OnException := MainForm.GuiProcessException;

  Application.Run;
end.

{$ELSE}

procedure WriteLnMessage(const AMessage: string);
begin
  WriteLn(AMessage);
end;

begin
  Logger.Info('----- START -----');

  ProcessText.Disassembling:= 'Disassembling CS_';
  ProcessText.PreparingOutput:='Preparing output CS_';
  ProcessText.LoadingDAS:= 'Loading DAS file - Code Section #';
  ProcessText.LoadingDHF:= 'Loading DHF file - Code Section #';
  ProcessText.SavingDAS:= 'Saving DAS file CS_';// - Code Section #';
  ProcessText.SavingDHF:= 'Saving DHF file CS_';// - Code Section #';

  try
    RunTatraDAS;
  except
    on E: Exception do
      ProcessException(E, WriteLnMessage);
  end;


  Logger.Info('----- DONE ------');
  Logger.Info('');
end.

{$ENDIF}
