unit CliUnit;

{$INCLUDE 'delver.inc'}

interface

uses
  {$IFDEF FPC}
    {$IFDEF LINUX}
  Crt,
    {$ENDIF}
  {$ENDIF}

  {$IFDEF MSWINDOWS}
    {$IFDEF DELPHI}
      {$IFDEF CONSOLE}
        //Crt in 'misc\win32\crt.pp',
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  Classes,
  SysUtils,
  // project units
  ExceptionsUnit,
  GlobalsUnit,
  StringUtilities,
  FilesUnit,
  procmat,
  ExecFileManagerUnit,
  ExecFileUnit,
  ProgressThreads;

procedure RunTatraDAS;


implementation

uses
  ProgressManagerUnit,
  StrUtils,
  LoggerUnit,
  CustomFileUnit;

type
  TRunOptions = record
    BadParameters: Boolean;
    ErrorMessage: string;
    InputFileName: string;
    OutputFileName: string;
    IsCustomFile: Boolean;
    CustomFileParameters: TCustomFileParameters;
    IsOutputProject: Boolean;
    ExportOption: TExportOption;
  end;

const
  UsageStr =
    'tdascon ([-i auto] | [-i custom  [-t <CodeSectionFileOffset>] [-s <CodeSectionSize>] [-e <EntryPointOffset>]])' + sLineBreak +
    '        [-o (proj|das|nasm)] input_file_name output_file_name' + sLineBreak + sLineBreak +
    'Options description:' + sLineBreak + sLineBreak +
    '  -i <input file format> Specify the format of input file, possible options:' + sLineBreak +
    '                           auto - automatic detection of input file format (default)' + sLineBreak +
    '                           custom - custom input file format, accepts these options: ' + sLineBreak +
    '                             -t  code section file offset' + sLineBreak +
    '                             -s  code section size' + sLineBreak +
    '                             -e  entry point file offset' + sLineBreak +
    '  -o <output format>     Specify the output format, possible options are:' + sLineBreak +
    '                           proj - TatraDAS project (*.das and *.dhf file), can be used by GUI version of TatraDAS' + sLineBreak +
    '                           das - just a DAS file, contains disassembly listing (default)' + sLineBreak +
    '                           nasm - NASM and YASM compilable (usually) file' + sLineBreak;



function ProcessParameters: TRunOptions;
type
  TState = (stMain, stCustom);
var
  ParamKey, ParamValue: string;
  ParamIndex: Integer;
  state: TState;
  InputFileSize: Int64;
begin
  // Defaults
  Result.IsCustomFile := False;
  Result.IsOutputProject := False;
  Result.ExportOption := eoDAS;
  Result.CustomFileParameters.EntrypointOffset := 0;
  Result.CustomFileParameters.FileOffset := 0;
  Result.CustomFileParameters.Size := $FFFFFFFF;
  Result.CustomFileParameters.Bit32 := True;

  state := stMain;
  ParamIndex := 0;
  while True do begin
    if ParamCount < ParamIndex + 2 then begin
      Result.BadParameters := True;
      Result.ErrorMessage := 'Not enough parameters';
      Exit;
    end;
    Inc(ParamIndex);
    ParamKey := ParamStr(ParamIndex);
    Inc(ParamIndex);
    ParamValue := ParamStr(ParamIndex);
    case state of
      stMain:
        // Input file format
        if ParamKey = '-i' then begin
          if ParamValue = 'auto' then
            Continue
          else if ParamValue = 'custom' then begin
            Result.IsCustomFile := True;
            state := stCustom;
          end
          else begin
            Result.ErrorMessage := 'Bad input file format parameter';
            Result.BadParameters := True;
            Exit;
          end;
        end

        // Output format
        else if ParamKey = '-o' then begin
          if ParamValue = 'proj' then
            Result.IsOutputProject := True
          else if ParamValue = 'das' then
            Result.ExportOption := eoDAS
          else if ParamValue = 'nasm' then
            Result.ExportOption := eoNASM
          else begin
            Result.BadParameters := True;
            Result.ErrorMessage := 'Bad output format';
            Exit;
          end;
        end

        // Input and output file names
        else begin
          Result.InputFileName := ExpandFilename(ParamKey);
          Result.OutputFileName := ExpandFilename(ParamValue);
          if not FileExists(Result.InputFileName) then begin
            Result.BadParameters := True;
            Result.ErrorMessage := 'Input file "' + Result.InputFileName + '" does not exist';
            Exit;
          end;

          // Check sizes and offsets if custom file
          if Result.IsCustomFile then begin
            InputFileSize := GetFileSize(Result.InputFileName);
            if Result.CustomFileParameters.FileOffset >= InputFileSize then begin
              Result.BadParameters := True;
              Result.ErrorMessage := 'Code section offset out of file';
              Exit;
            end;
            if Result.CustomFileParameters.Size = $FFFFFFFF then
              Result.CustomFileParameters.Size := InputFileSize - Result.CustomFileParameters.FileOffset
            else if (Result.CustomFileParameters.FileOffset + Result.CustomFileParameters.Size) > InputFileSize then begin
              Result.BadParameters := True;
              Result.ErrorMessage := 'Code section too big';
              Exit;
            end;

            if (Result.CustomFileParameters.EntryPointOffset < Result.CustomFileParameters.FileOffset) or
              (Result.CustomFileParameters.FileOffset >= (Result.CustomFileParameters.FileOffset + Result.CustomFileParameters.Size)) then begin
              Result.BadParameters := True;
              Result.ErrorMessage := 'Entry point out of code section';
              Exit;
            end;
          end;

          Result.BadParameters := False;
          Break;
        end;

      stCustom: begin
        // 16/32 bitness
        if ParamKey = '-b' then begin
          if ParamValue = '16' then
            Result.CustomFileParameters.Bit32 := False
          else if ParamValue = '32' then
            Result.CustomFileParameters.Bit32 := True
          else begin
            Result.BadParameters := True;
            Result.ErrorMessage := 'Bad 16/32 bitness';
            Exit;
          end;
        end
        // Code section size
        else if ParamKey = '-s' then begin
          if StrToIntDef(ParamValue, -1) <> -1 then
            Result.CustomFileParameters.Size := StrToInt(ParamValue)
          else begin
            Result.BadParameters := True;
            Result.ErrorMessage := 'Incorrect code section size';
            Exit;
          end;
        end
        // Entry point file offset
        else if ParamKey = '-e' then begin
          if StrToIntDef(ParamValue, -1) <> -1 then
            Result.CustomFileParameters.EntryPointOffset := StrToInt(ParamValue)
          else begin
            Result.BadParameters := True;
            Result.ErrorMessage := 'Incorrect entry point offset';
            Exit;
          end;
        end
        // Code section file offset
        else if ParamKey = '-t' then begin
          if StrToIntDef(ParamValue, -1) <> -1 then
            Result.CustomFileParameters.FileOffset := StrToInt(ParamValue)
          else begin
            Result.BadParameters := True;
            Result.ErrorMessage := 'Incorrect file offset';
            Exit;
          end;
        end
        else begin
          Dec(ParamIndex, 2);
          state := stMain;
        end;
      end;

    end;
  end;
end;



function GetRunOptionsText(const ARunOptions: TRunOptions): string;
begin
  with ARunOptions do begin
    if BadParameters then
      Result := 'Bad parameters: ' + ErrorMessage
    else begin
      Result := 'Input file: ' + InputFileName + sLineBreak + 'Output file: ' + OutputFileName;
      if IsCustomFile then begin
        Result := Result +
          'Custom file.' + sLineBreak +
          'EntryPointOffset: ' + CarToHex(CustomFileParameters.EntryPointOffset, 8) + sLineBreak +
          'FileOffset: ' + CarToHex(CustomFileParameters.FileOffset, 8) + sLineBreak +
          'Size: ' + CarToHex(CustomFileParameters.Size, 8) + sLineBreak +
          'Bit32: ' + BoolToStr(CustomFileParameters.Bit32, True) + sLineBreak;
      end;
      Result := Result + 'OutputFormat: ';
      if IsOutputProject then
        Result := Result + 'project' + sLineBreak
      else if eoNASM = ExportOption then
        Result := Result + 'nasm' + sLineBreak
      else if eoDAS = ExportOption then
        Result := Result + 'das' + sLineBreak
      else
        raise ETatraDASException.Create('Internal error');
    end;
  end;
end;



procedure DisplayProgramUsage(Reason: string);
begin
  if Reason <> '' then begin
    WriteLn;
    WriteLn('Parameter error: ' + Reason);
    WriteLn;
    WriteLn;
  end;
  WriteLn(UsageStr);
end;



function GetProgramIdentification: string;
begin
  Result := TatraDASFullNameVersion + ' - console version, ' + CopyrightStr;
  Result := DupeString('-', Length(Result)) + sLineBreak + Result + sLineBreak + DupeString('-', Length(Result)) + sLineBreak;
end;



var
  CurrentPhaseName: string;



procedure ConsoleShowProgress(APhase: string; AProgress: Double);
begin
  {$IFDEF LINUX}
    {$IFDEF FPC}
  if CurrentPhaseName <> APhase then begin
    WriteLn;
    CurrentPhaseName := APhase;
  end;

  GotoXY(3, WhereY);
  Write(
    StringRightPad(CurrentPhaseName + ':', MaxProgressNameLength + 1) +
    StringRightPad(DupeString('.', Round(20 * AProgress)), 20 + 1) +
    IntToStr(Round(100 * AProgress)) + '%'
  );
    {$ENDIF}
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  if CurrentPhaseName <> APhase then begin
    CurrentPhaseName := APhase;
    WriteLn(CurrentPhaseName + ':');
  end;
  {$ENDIF}
end;



procedure ExecuteProgress(AThread: TThread);
begin
  CurrentPhaseName := '';
  ProgressData.AbortExecution := False;
  ProgressManager := TProgressManager.Create(ConsoleShowProgress);
  try
    ProgressManager.StartProgress(AThread);
  finally
    FreeAndNil(ProgressManager);
  end;
  WriteLn;
end;



procedure RunDisassembler(ExecFile: TExecutableFile; OutputFileName: string; IsOutputProject: Boolean; ExportOption: TExportOption);
var
  DisassemblingThread, SavingThread: TThread;
begin
  // Disassemble it
  Writeln;
  Writeln('CS_n  =  Code Section number ''n''.');
  Writeln;

  // Non thread way
//  ExecFile.Disassemble;

  // Thread way
  DisassemblingThread := TDisassembleThread.Create(ExecFile);
  try
    ExecuteProgress(DisassemblingThread);
  finally
    FreeAndNil(DisassemblingThread);
  end;

  // Save it
  if IsOutputProject then
    SavingThread := TSaveThread.Create(ExecFileManager, ExecFile, OutputFileName)
  else
    SavingThread := TExportThread.Create(ExecFileManager, ExecFile, OutputFileName, ExportOption, []);
  try
    ExecuteProgress(SavingThread);
  finally
    FreeAndNil(SavingThread);
  end;

  // Rename it
  RenameFile(ChangeFileExt(OutputFileName, DASFileExtension), OutputFileName);
  WriteLn;
  WriteLn;
  WriteLn('Disassembling finished, result saved to file ''', OutputFileName, '''.');
end;



procedure RunTatraDAS;
var
  RunOptions: TRunOptions;
  ExecFile: TExecutableFile;
begin
  WriteLn(GetProgramIdentification);
  RunOptions := ProcessParameters;
  Logger.Debug(GetRunOptionsText(RunOptions));
  if RunOptions.BadParameters then begin
    DisplayProgramUsage(RunOptions.ErrorMessage);
    Exit;
  end;

  if RunOptions.IsCustomFile then
    ExecFile := ExecFileManager.CreateNewCustomExecFile(RunOptions.InputFileName, RunOptions.CustomFileParameters)
  else
    ExecFile := ExecFileManager.CreateNewExecFile(RunOptions.InputFileName);
  try
    RunDisassembler(ExecFile, RunOptions.OutputFileName, RunOptions.IsOutputProject, RunOptions.ExportOption);
  finally
    ExecFile.Free;
  end;
end;



{
// Procedure is for testing purposes only
procedure SaveCodeSections(AExecFile: TExecutableFile; AFileName: string);
var
  SectionIndex: integer;
  CodeSection: TCodeSection;
  fs: TStream;
begin
  fs := TFileStream.Create(AFileName, fmCreate);
  for SectionIndex := 0 to AExecFile.Sections.Count - 1 do begin
    if (AExecFile.Sections[SectionIndex] is TCodeSection) then begin
      CodeSection := (AExecFile.Sections[SectionIndex] as TCodeSection);
      CodeSection.CodeStream.Position := 0;
      CodeSection.CodeStream.SaveToStream(fs);
      //fs.Write(CodeSection.CodeStream);
    end;
  end;
  fs.Free;
end;
}



end.
