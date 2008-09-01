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

  procmat,
  ExecFileManagerUnit,
  ExecFileUnit,
  ProgressThreads
  ;

procedure ShowUsage;
procedure RunDisassembler(InputFileName, OutputFileName: string);


implementation

// Temporary
uses
  CodeSectionUnit;


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


procedure RunDisassembler(InputFileName, OutputFileName: string);
var
  ExecFileManager: TExecFileManager;
  ExecFile: TExecutableFile;
  SaveOptions: TSaveOptions;
begin
  ExecFileManager:= TExecFileManager.Create;
  ExecFile:= nil;
  try
    // Create ExecFile
    ExecFile := ExecFileManager.CreateNewExecFile(ExpandFilename(InputFileName));
    if ProgressData.ErrorStatus <> errNone then
      raise ETatraDASException.Create('');

    // Disassemble it
    Writeln;
    Writeln('CS_n  =  Code Section number ''n''.');
    Writeln;
    ExecuteProgress(TDisassembleThread.Create(ExecFile));
    if ProgressData.ErrorStatus <> errNone then
      raise ETatraDASException.Create('');

    // Save result to file

    // TODO
    SaveOptions:= [soDisassembly];
    //SaveOptions:= [soNASM];

    // Ulozenie kodu kodovych sekcii
    //SaveCodeSections(ExecFile, ChangeFileExt(OutputFileName, '.bin'));

    ExecuteProgress(TSaveThread.Create(ExecFileManager, ExecFile, OutputFileName, SaveOptions));
    RenameFile(ChangeFileExt(OutputFileName, '.das'), OutputFileName);
    WriteLn;
    WriteLn;
    WriteLn('Disassembling finished, result saved to file ''', OutputFileName, '''.');

  finally
    ExecFile.Free;
    ExecFileManager.Free;
  end;
end;



end.