program disasm_asm;

uses
  {$IFDEF MSWINDOWS}
    {$IFDEF GUI_B}
      Windows,
      Forms,
      SynEdit,
    {$ENDIF}
  {$ENDIF}

  {$IFDEF LINUX}
    {$IFDEF GUI_B}
      QForms,
      SynEdit,
    {$ENDIF}
  {$ENDIF}

  {$IFDEF FPC}
    {$IFDEF LINUX}
      Crt,
      cthreads,
    {$ENDIF}
  {$ENDIF}

  {$IFDEF MSWINDOWS}
    {$IFDEF DELPHI}
      {$IFDEF CONSOLE}
        //Crt in 'misc\win32\crt.pp',
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  SysUtils,
  Classes,
  StrUtils,



  procmat in 'procmat.pas',
  StringUtilities in 'StringUtilities.pas',
  SortingUnit in 'SortingUnit.pas',
  ExecFileManagerUnit in 'ExecFileManagerUnit.pas',
  ExecFileUnit in 'ExecFileUnit.pas',
  SectionUnit in 'SectionUnit.pas',
  RegionsUnit in 'RegionsUnit.pas',
  ProgressThreads in 'ProgressThreads.pas',

{$IFDEF MSWINDOWS}

  // Misc. units
  StringRes in 'res\StringRes.pas',
  LoggerUnit in 'misc\LoggerUnit.pas',

{$IFDEF GUI_B}
  TatraDASHighlighter in 'res\TatraDASHighlighter.pas',
  ButtonsX in 'misc\ButtonsX.pas',
  myedits in 'misc\myedits.pas',
  TranslatorUnit in 'TranslatorUnit.pas',
  VersionUnit in 'VersionUnit.pas',

  // Forms' units
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

{$ELSE}
  CliUnit in 'CliUnit.pas',
  TatraDAS_SynEditStringList in 'misc\TatraDAS_SynEditStringList.pas',
{$ENDIF}

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
  ResourceSectionUnit in 'sections\ResourceSectionUnit.pas';

{$ENDIF}

{$IFDEF LINUX}
  // Misc. units
  StringRes in 'res/StringRes.pas',
  LoggerUnit in 'misc/LoggerUnit.pas',
  FilesUnit in 'misc/FilesUnit.pas',


{$IFDEF GUI_B}
  ButtonsX in 'misc/ButtonsX.pas',
  myedits in 'misc/myedits.pas',
  TranslatorUnit in 'TranslatorUnit.pas',
  TatraDASHighlighter in 'res/TatraDASHighlighter.pas',
  VersionUnit in 'VersionUnit.pas',

  // Forms' units
  MainFormUnit in 'MainFormUnit.pas' {MainForm},
  HexEditFormUnit in 'forms/HexEditFormUnit.pas' {HexEditForm},
  CalculatorUnit in 'forms/CalculatorUnit.pas' {Calculator},
  OptionsFormUnit in 'forms/OptionsFormUnit.pas' {OptionsForm},
  AdvancedChangingToDataFormUnit in 'forms/AdvancedChangingToDataFormUnit.pas' {AdvancedChangingToDataForm},
  AdvancedDisassembleFormUnit in 'forms/AdvancedDisassembleFormUnit.pas' {AdvancedDisassembleForm},
  InsertCommentFormUnit in 'forms/InsertCommentFormUnit.pas' {InsertCommentForm},
  AboutBoxUnit in 'forms/AboutBoxUnit.pas' {AboutBox},
  UnknownFileFormUnit in 'forms/UnknownFileFormUnit.pas' {UnknownFileFormatForm},
  SaveOptionsFormUnit in 'forms/SaveOptionsFormUnit.pas' {SaveOptionsForm},
  ProgressFormUnit in 'forms/ProgressFormUnit.pas' {ProgressForm},
  GotoAddressFormUnit in 'forms/GotoAddressFormUnit.pas' {GoToAddressForm},

{$ELSE}
  CliUnit in 'CliUnit.pas',
  TatraDAS_SynEditStringList in 'misc/TatraDAS_SynEditStringList.pas',
{$ENDIF}
  // Disassembler units
  DisassemblerUnit in 'disasm/DisassemblerUnit.pas',
  DisassemblerUtils in 'disasm/DisassemblerUtils.pas',
  DisassemblerTypes in 'disasm/DisassemblerTypes.pas',
  x86DisassemblerTypes in 'disasm/x86DisassemblerTypes.pas',
  x86Instructions in 'disasm/x86Instructions.pas',
  x86Disassembler in 'disasm/x86Disassembler.pas',
  DisassembledBlocksUnit in 'disasm/DisassembledBlocksUnit.pas',
  CallsAndJumpsTableUnit in 'disasm/CallsAndJumpsTableUnit.pas',

  // Sections' units
  CodeSectionUnit in 'sections/CodeSectionUnit.pas',
  ExportSectionUnit in 'sections/ExportSectionUnit.pas',
  ImportSectionUnit in 'sections/ImportSectionUnit.pas',
  ResourceSectionUnit in 'sections/ResourceSectionUnit.pas',

  // Executable formats' units
  MZFileUnit in 'exefiles/MZFileUnit.pas',
  COMFileUnit in 'exefiles/COMFileUnit.pas',
  LEFileUnit in 'exefiles/LEFileUnit.pas',
  LXFileUnit in 'exefiles/LXFileUnit.pas',
  NEFileUnit in 'exefiles/NEFileUnit.pas',
  PEFileUnit in 'exefiles/PEFileUnit.pas',
  ELFFileUnit in 'exefiles/ELFFileUnit.pas',
  CustomFileUnit in 'exefiles/CustomFileUnit.pas',

  Unix
  ;

{$ENDIF}


var
  dir: TDirNode;
  RootDirName: string;
  FileIndex: integer;
  FileName: string;
begin
  RootDirName := ParamStr(1);
  dir := TDirNode.Create(RootDirName, '', '', '.exe', TFileNode);

  for FileIndex := 0 to dir.FileCount - 1 do begin
    FileName := AddPathDelimiter(RootDirName, true) + dir.Files[FileIndex].Name;
    if GetFileSize(FileName) > 1000000 then Continue;
    writeln(FileName);
    RunDisassembler(FileName, ChangeFileExt(FileName, '.asm'));
    fpSystem('yasm ' + ChangeFileExt(FileName, '.asm'));
  end;
end.