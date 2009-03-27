unit CliUnit;

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

  StringUtilities,
  FilesUnit,
  procmat,
  ExecFileManagerUnit,
  ExecFileUnit,
  ProgressThreads
  ;


procedure RunTatraDAS;


implementation

uses
// Temporary
//  CodeSectionUnit,
  StrUtils,
  LoggerUnit,
  CustomFileUnit;


type
  TRunOptions = record
    BadParameters: boolean;
    ErrorMessage: string;
    InputFileName: string;
    OutputFileName: string;
    IsCustomFile: boolean;
    CustomFileParameters: TCustomFileParameters;
    SaveOptions: TSaveOptions;
  end;

const
  CRLF = #13#10;
  UsageStr =
    'tdascon ([-i auto] | [-i custom  [-t <CodeSectionFileOffset>] [-s <CodeSectionSize>] [-e <EntryPointOffset>]])' + CRLF +
    '        [-o (proj|das|nasm)] input_file_name output_file_name' + CRLF + CRLF +
    'Options description:' + CRLF + CRLF +
    '  -i <input file format> Specify the format of input file, possible options:' + CRLF +
    '                           auto - automatic detection of input file format (default)' + CRLF +
    '                           custom - custom input file format, accepts these options: ' + CRLF +
    '                             -t  code section file offset' + CRLF +
    '                             -s  code section size' + CRLF +
    '                             -e  entry point file offset' + CRLF +
    '  -o <output format>     Specify the output format, possible options are:' + CRLF +
    '                           proj - TatraDAS project (*.das and *.dhf file), can be used by GUI version of TatraDAS' + CRLF +
    '                           das - just a DAS file, contains disassembly listing (default)' + CRLF +
    '                           nasm - NASM and YASM compilable (usually) file' + CRLF;


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
  result.IsCustomFile := false;
  result.SaveOptions := [soDisassembly];
  result.CustomFileParameters.EntrypointOffset := 0;
  result.CustomFileParameters.FileOffset := 0;
  result.CustomFileParameters.Size := $FFFFFFFF;
  result.CustomFileParameters.Bit32 := true;

  state := stMain;
  ParamIndex := 0;
  while true do begin
    if ParamCount < ParamIndex + 2 then begin
      result.BadParameters := true;
      result.ErrorMessage := 'Not enough parameters';
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
            result.IsCustomFile := true;
            state := stCustom;
          end
          else begin
            result.ErrorMessage := 'Bad input file format parameter';
            result.BadParameters := true;
            Exit;
          end;
        end

        // Output format
        else if ParamKey = '-o' then begin
          if ParamValue = 'proj' then
            result.SaveOptions := [soProject]
          else if ParamValue = 'das' then
            result.SaveOptions := [soDisassembly]
          else if ParamValue = 'nasm' then
            result.SaveOptions := [soNASM]
          else begin
            result.BadParameters := true;
            result.ErrorMessage := 'Bad output format';
            Exit;
          end;
        end

        // Input and output file names
        else begin
          result.InputFileName := ExpandFilename(ParamKey);
          result.OutputFileName := ExpandFilename(ParamValue);
          if not FileExists(result.InputFileName) then begin
            result.BadParameters := true;
            result.ErrorMessage := 'Input file "' + result.InputFileName + '" does not exist';
            Exit;
          end;

          // Check sizes and offsets if custom file
          if result.IsCustomFile then begin
            InputFileSize := GetFileSize(result.InputFileName);
            if result.CustomFileParameters.FileOffset >= InputFileSize then begin
              result.BadParameters := true;
              result.ErrorMessage := 'Code section offset out of file';
              Exit;
            end;
            if result.CustomFileParameters.Size = $FFFFFFFF then
              result.CustomFileParameters.Size := InputFileSize - result.CustomFileParameters.FileOffset
            else
              if (result.CustomFileParameters.FileOffset + result.CustomFileParameters.Size) > InputFileSize then begin
                result.BadParameters := true;
                result.ErrorMessage := 'Code section too big';
                Exit;
              end;

            if (result.CustomFileParameters.EntryPointOffset < result.CustomFileParameters.FileOffset) or (result.CustomFileParameters.FileOffset >= (result.CustomFileParameters.FileOffset + result.CustomFileParameters.Size)) then begin
              result.BadParameters := true;
              result.ErrorMessage := 'Entry point out of code section';
              Exit;
            end;
          end;

          result.BadParameters := false;
          Break;
        end;

      stCustom: begin
        // 16/32 bitness
        if ParamKey = '-b' then begin
          if ParamValue = '16' then
            result.CustomFileParameters.Bit32 := false
          else if ParamValue = '32' then
            result.CustomFileParameters.Bit32 := true
          else begin
            result.BadParameters := true;
            result.ErrorMessage := 'Bad 16/32 bitness';
            Exit;
          end;
        end
        // Code section size
        else if ParamKey = '-s' then begin
          if StrToIntDef(ParamValue, -1) <> - 1 then
            result.CustomFileParameters.Size := StrToInt(ParamValue)
          else begin
            result.BadParameters := true;
            result.ErrorMessage := 'Incorrect code section size';
            Exit;
          end;
        end
        // Entry point file offset
        else if ParamKey = '-e' then begin
          if StrToIntDef(ParamValue, -1) <> - 1 then
            result.CustomFileParameters.EntryPointOffset := StrToInt(ParamValue)
          else begin
            result.BadParameters := true;
            result.ErrorMessage := 'Incorrect entry point offset';
            Exit;
          end;
        end
        // Code section file offset
        else if ParamKey = '-t' then begin
          if StrToIntDef(ParamValue, -1) <> - 1 then
            result.CustomFileParameters.FileOffset := StrToInt(ParamValue)
          else begin
            result.BadParameters := true;
            result.ErrorMessage := 'Incorrect file offset';
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
      result := 'Bad parameters: ' + ErrorMessage
    else begin
      result := 'Input file: ' + InputFileName + CRLF + 'Output file: ' + OutputFileName;
      if IsCustomFile then begin
        result := result +
          'Custom file.' + CRLF +
          'EntryPointOffset: ' + CarToHex(CustomFileParameters.EntryPointOffset, 8) + CRLF +
          'FileOffset: ' + CarToHex(CustomFileParameters.FileOffset, 8) + CRLF +
          'Size: ' + CarToHex(CustomFileParameters.Size, 8) + CRLF +
          'Bit32: ' + BoolToStr(CustomFileParameters.Bit32, true) + CRLF;
      end;
      result := result + 'OutputFormat: ';
      if soProject in SaveOptions then
        result := result + 'project' + CRLF
      else if soNasm in SaveOptions then
        result := result + 'nasm' + CRLF
      else if soDisassembly in SaveOptions then
        result := result + 'das' + CRLF
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
  result := TatraDASFullNameVersion + ' - console version, Ivan Kohut (c) 2008';
  result := DupeString('-', Length(result)) + CRLF + result + CRLF + DupeString('-', Length(result)) + CRLF;
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
       {$IFDEF LINUX}
        SavedXPosition:= WhereX;
        GotoXY(MaxProgressNameLength + 2 + 20 + 1, WhereY);
        Write(Round(100 * ProgressData.Position / ProgressData.Maximum), '%');
        GotoXY(SavedXPosition, WhereY);
       {$ENDIF}
      {$ENDIF}
    end;
    Sleep(100);
  end;
  AThread.WaitFor;
  WriteLn;
end;



procedure RunDisassembler(ExecFile: TExecutableFile; OutputFileName: string; SaveOptions: TSaveOptions);
begin
  if ProgressData.ErrorStatus <> errNone then
    raise ETatraDASException.Create('');

  // Disassemble it
  Writeln;
  Writeln('CS_n  =  Code Section number ''n''.');
  Writeln;
  ExecuteProgress(TDisassembleThread.Create(ExecFile));
  if ProgressData.ErrorStatus <> errNone then
    raise ETatraDASException.Create('');

  // Save it
  ExecuteProgress(TSaveThread.Create(ExecFileManager, ExecFile, OutputFileName, SaveOptions));

  // Rename it
  RenameFile(ChangeFileExt(OutputFileName, '.das'), OutputFileName);
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
    RunDisassembler(ExecFile, RunOptions.OutputFileName, RunOptions.SaveOptions);
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
