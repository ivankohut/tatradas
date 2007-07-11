program TatraDAS;

{$INCLUDE 'delver.inc'}

{%File 'TatraDASHighlighter.msg'}
{%File 'misc\tatradas.rc'}

{$IFDEF GUI_B}
  {$IFDEF MSWINDOWS}
    {$R 'misc\tatradas.res' 'misc\tatradas.rc'}
  {$ENDIF}
  {$IFDEF LINUX}
    {$R 'misc/tatradas.res' 'misc/tatradas.rc'}
  {$ENDIF}
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
  Windows,
  {$IFDEF GUI_B}
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
{$IFNDEF DELPHI}
  Crt,
{$ENDIF}
  SysUtils,
  procmat in 'procmat.pas',
  VersionUnit in 'VersionUnit.pas',
  CallsAndJumpsTableUnit in 'CallsAndJumpsTableUnit.pas',
  disassembler in 'disassembler.pas',
  ExecFileManagerUnit in 'ExecFileManagerUnit.pas',
  RegionsUnit in 'RegionsUnit.pas',
  ExecFileUnit in 'ExecFileUnit.pas',
  SectionUnit in 'SectionUnit.pas',

//  UProgressThread in 'UProgressThread.pas',

{$IFDEF MSWINDOWS}

// Misc. units
  StringRes in 'misc\StringRes.pas',

{$IFDEF GUI_B}
  TatraDASHighlighter in 'misc\TatraDASHighlighter.pas',
  ButtonsX in 'misc\ButtonsX.pas',
  Languages in 'misc\Languages.pas',

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
  TatraDAS_SynEditStringList in 'misc\TatraDAS_SynEditStringList.pas',
{$ENDIF}

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

{$ENDIF}

{$IFDEF LINUX}
// Misc. units
  StringRes in 'misc/StringRes.pas',

{$IFDEF GUI_B}
  ButtonsX in 'misc/ButtonsX.pas',
  Languages in 'misc/Languages.pas',
  TatraDASHighlighter in 'misc/TatraDASHighlighter.pas',

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
  TatraDAS_SynEditStringList in 'misc/TatraDAS_SynEditStringList.pas',
{$ENDIF}

// Sections' units
  CodeSectionUnit in 'sections/CodeSectionUnit.pas',
  ExportSectionUnit in 'sections/ExportSectionUnit.pas',
  ImportSectionUnit in 'sections/ImportSectionUnit.pas',
  ResourceSectionUnit in 'sections/ResourceSectionUnit.pas',

// Executable formats' units
  CustomFileUnit in 'exefiles/CustomFileUnit.pas',
  MZFileUnit in 'exefiles/MZFileUnit.pas',
  COMFileUnit in 'exefiles/COMFileUnit.pas',
  LEFileUnit in 'exefiles/LEFileUnit.pas',
  LXFileUnit in 'exefiles/LXFileUnit.pas',
  NEFileUnit in 'exefiles/NEFileUnit.pas',
  PEFileUnit in 'exefiles/PEFileUnit.pas',
  ELFFileUnit in 'exefiles/ELFFileUnit.pas';

{$ENDIF}

{$IFDEF GUI_B} // Hlavny program v GUI

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
  Application.CreateForm(TProgressForm, ProgressForm);
  Application.CreateForm(TGoToAddressForm, GoToAddressForm);
  Application.Run;
end.

{$ENDIF}

{$IFDEF CONSOLE}

const
  ProjectName = 'TatraDAS';
var
  NotSupported: string='This version of '+projectname+' does not support this file format!';
  NotFound: string=' not found!';
  DisassemblyingText: string='Disassemblying...';         // ProgressLabel texty
  IdentifyingJumpsText: string='Identifying jumps and calls...';
  PreparingOutputText: string='Preparing output...';      //

  ExecFileManager: TExecFileManager;
  ExecFile: TExecutableFile;
//  CurrentLanguage: string;
  ProgressPosition: cardinal;



procedure CloseProgram;
begin
  if ExecFile <> nil then
    ExecFile.Destroy;
end;



procedure ShowUsage;
begin
  writeln('usage:');
  Writeln;
  writeln('tdascon input_file_name output_file_name');
  writeln;
  writeln('  input_file_name       name of file you want to disassemble');
  writeln('  output_file_name      name of file you want to send disassembled output to');
//  writeln('language = language which TatraDAS will use to comunicate with you');
  writeln;
//  writeln('Note: If you type invalid language, this parameter will be ignored');
end;



function ProgressFunction(a, b: cardinal; c: string): boolean;
begin
  if a = 0 then begin
    if c<>'' then begin
      Writeln;
      Write(c,':');
    end;
    ProgressPosition:=0;
    result:=true;
    exit;
  end;
{$IFNDEF DELPHI} // Delphi nepozna unit Crt
  GotoXY(75,WhereY);
//  Write((100*a) div b,'%');
  Write(Round(100*a/b),'%');

  GotoXY(22+(50*a) div b,WhereY);
{$ENDIF}
  while (50*a) div b > ProgressPosition do begin
    Inc(ProgressPosition);
    Write('.');
  end;
  result:=true;
end;



procedure RunDisassembler(InputFileName, OutputFileName:string);
var
  SaveOptions: TSaveOptions;
  ProgressFunctionVar: TProgressFunction;
begin
  ProgressFunctionVar:=ProgressFunction;

  ExecFileManager:= TExecFileManager.Create;

  ExecFile:=ExecFileManager.CreateNewExecFile(ExpandFilename(InputFileName));
  if ExecFileManager.Error <> errNone then begin
    case ExecFileManager.Error of
      errOpen: Writeln('Unable to open file '''+ExpandFilename(InputFileName)+'''.');
      else
        Writeln('Unknown error !');
    end;
    ExecFile.Free;
    Exit;
  end;


  WriteLn('Format: ', Integer(ExecFile.ExeFormat));
  if ExecFile.ExeFormat <> ffPE then begin
    writeln('Skipping non-PE file.');
    ExecFile.Free;
    ExecFileManager.Free;
    halt;
  end;

  Writeln;
  Writeln('CSn = Code Section No.');
  Writeln;
  ExecFile.Disassemble;
  SaveOptions:=[soDisassembly];
  ExecFileManager.SaveExecFileToFile(ExecFile, OutputFileName, SaveOptions);
  RenameFile(ChangeFileExt(OutputFileName,'.das'), OutputFileName);
  Writeln;
  Writeln;
  Writeln('Disassemblying finished, output saved to ''',OutputFileName,'''.');
  ExecFile.Free;
  ExecFileManager.Free;
end;



procedure TranslateConsole(language:string);
var f:integer;
begin
  f:=fileopen('lan'+language+'.ini', fmsharedenynone);
  if f<>0 then fileclose(f);
end;



var ProgramIdentification:string;
    i:integer;

begin
  ProcessText.Disassemblying:='Disassemblying CS';
  ProcessText.Indentifying:='Identifying jumps and calls...';
  ProcessText.PreparingOutput:='Preparing output CS';
  ProcessText.LoadingDAS:='Loading DAS file - Code Section #';
  ProcessText.LoadingDHF:='Loading DHF file - Code Section #';
  ProcessText.SavingDAS:='Saving DAS file CS';// - Code Section #';
  ProcessText.SavingDHF:='Saving DHF file CS';// - Code Section #';

  ProgramIdentification:=TatraDASFullNameVersion + ' - console version,   Ivan Kohut  2007';
  Writeln(ProgramIdentification);
  for i:=0 to length(ProgramIdentification)-1 do Write('-');
  Writeln;
  case ParamCount of
    2: begin
//        TranslateConsole(ParamStr(2));
        RunDisassembler(ParamStr(1),ParamStr(2));
       end;
    else begin
      ShowUsage;
    end;
  end;
end.

{$ENDIF}
