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
  {$IFDEF FPC}
  Crt, cthreads,
  {$ENDIF}
  SysUtils,
  Classes,
  StrUtils,
  procmat in 'procmat.pas',
  VersionUnit in 'VersionUnit.pas',
  CallsAndJumpsTableUnit in 'CallsAndJumpsTableUnit.pas',
  disassembler in 'disassembler.pas',
  ExecFileManagerUnit in 'ExecFileManagerUnit.pas',
  ExecFileUnit in 'ExecFileUnit.pas',
  SectionUnit in 'SectionUnit.pas',
  ProgressThreads in 'ProgressThreads.pas',

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
  MZFileUnit in 'exefiles/MZFileUnit.pas',
  COMFileUnit in 'exefiles/COMFileUnit.pas',
  LEFileUnit in 'exefiles/LEFileUnit.pas',
  LXFileUnit in 'exefiles/LXFileUnit.pas',
  NEFileUnit in 'exefiles/NEFileUnit.pas',
  PEFileUnit in 'exefiles/PEFileUnit.pas',
  ELFFileUnit in 'exefiles/ELFFileUnit.pas',
  CustomFileUnit in 'exefiles/CustomFileUnit.pas';

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

procedure ShowUsage;
begin
  WriteLn('usage:');
  WriteLn;
  WriteLn('tdascon input_file_name output_file_name');
  WriteLn;
  WriteLn('  input_file_name       name of file you want to disassemble');
  WriteLn('  output_file_name      name of file you want to send disassembled output to');
  WriteLn;
end;



procedure ExecuteProgress(AThread: TThread);
var
  ProgressCharsCount: integer;
  CurrentProgress: string;
  SavedXPosition: integer;
begin
  ProgressData.Finished:= false;
  ProgressData.ErrorStatus:= errNone;
  ProgressData.Maximum:= 0;
  ProgressData.Position:= 0;
  ProgressData.Name:= '';

  ProgressCharsCount:= 0;
  CurrentProgress:= '';
  AThread.Resume;
  while not ProgressData.Finished do begin
    if ProgressData.Maximum <> 0 then begin
      // Display progress name and reset progress shower after progress change
      if CurrentProgress <> ProgressData.Name then begin
        CurrentProgress:= ProgressData.Name;
        ProgressCharsCount:= 0;
        WriteLn;
        Write(StringRightPad(ProgressData.Name + ':', MaxProgressNameLength + 1));
      end;
      // Show progress
      while ProgressCharsCount < Round(20 * ProgressData.Position / ProgressData.Maximum) do begin
        Write('.');
        Inc(ProgressCharsCount);
      end;
       {$IFDEF FPC}
       SavedXPosition:= WhereX;
       GotoXY(MaxProgressNameLength + 2 + 20 + 1, WhereY);
       Write(Round(100 * ProgressData.Position / ProgressData.Maximum), '%');
       GotoXY(SavedXPosition, WhereY);
      {$ENDIF}
    end;
    Sleep(100);
  end;
  AThread.WaitFor;
  WriteLn;
end;



procedure RunDisassembler(InputFileName, OutputFileName:string);
var
  ExecFileManager: TExecFileManager;
  ExecFile: TExecutableFile;
  SaveOptions: TSaveOptions;

begin
  ExecFileManager:= TExecFileManager.Create;

  // Create ExecFile
  ExecFile:= ExecFileManager.CreateNewExecFile(ExpandFilename(InputFileName));
  if ProgressData.ErrorStatus <> errNone then begin
    case ProgressData.ErrorStatus of
      errOpen: Writeln('Unable to open file ''' + ExpandFilename(InputFileName) + '''.');
      else
        Writeln('Unknown error !');
    end;
    ExecFile.Free;
    Exit;
  end;

  // Disassemble it
  Writeln;
  Writeln('CS_n <=> Code Section No.');
  Writeln;
  ExecuteProgress(TDisassembleThread.Create(ExecFile));

  // Save result to file
  SaveOptions:= [soDisassembly];
  ExecuteProgress(TSaveThread.Create(ExecFileManager, ExecFile, OutputFileName, SaveOptions));
  RenameFile(ChangeFileExt(OutputFileName, '.das'), OutputFileName);
  WriteLn;
  WriteLn;
  WriteLn('Disassemblying finished, result saved to ''', OutputFileName, '''.');

  ExecFile.Free;
  ExecFileManager.Free;
end;


var
  ProgramIdentification: string;

begin
  ProcessText.Disassemblying:= 'Disassemblying CS_';
//  ProcessText.Indentifying:= 'Identifying jumps and calls...';
  ProcessText.PreparingOutput:='Preparing output CS_';
  ProcessText.LoadingDAS:= 'Loading DAS file - Code Section #';
  ProcessText.LoadingDHF:= 'Loading DHF file - Code Section #';
  ProcessText.SavingDAS:= 'Saving DAS file CS_';// - Code Section #';
  ProcessText.SavingDHF:= 'Saving DHF file CS_';// - Code Section #';


  ProgramIdentification:= TatraDASFullNameVersion + ' - console version, Ivan Kohut (c) 2007';
  WriteLn(ProgramIdentification);
  WriteLn(DupeString('-', Length(ProgramIdentification)));
  Writeln;

  case ParamCount of
    2: begin
         RunDisassembler(ParamStr(1), ParamStr(2));
       end;
    else begin
      ShowUsage;
    end;
  end;

end.

{$ENDIF}
