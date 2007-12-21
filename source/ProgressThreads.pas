unit ProgressThreads;

interface

uses
  Classes,
  SysUtils,

  procmat,
  DisassemblerUnit,
  DisassemblerTypes,
  CodeSectionUnit,
  ExecFileManagerUnit,
  ExecFileUnit;

type


  TDisassembleThread = class(TThread)
  private
    fExecFile: TExecutableFile;
  protected
    procedure Execute; override;
  public
    constructor Create(AExecFile: TExecutableFile);
  end;


  TDisassemblePartThread = class(TThread)
  private
    fCodeSection: TCodeSection;
    fOptions: TDisassembleOptions;
  protected
    procedure Execute; override;
  public
    constructor Create(ACodeSection: TCodeSection; AOptions: TDisassembleOptions);
  end;


  TSaveThread = class(TThread)
  private
    fManager: TExecFileManager;
    fExecFile: TExecutableFile;
    fFileName: string;
    fSaveOptions: TSaveOptions;
  protected
    procedure Execute; override;
  public
    constructor Create(Manager: TExecFileManager; AExecFile: TExecutableFile; AFileName: string; ASaveOptions: TSaveOptions);
  end;


  TLoadThread = class(TThread)
  private
    fManager: TExecFileManager;
    fFileName: string;
  protected
    procedure Execute; override;
  public
    constructor Create(Manager: TExecFileManager; AFileName: string);
  end;


Implementation


{ TDisassembleThread }


constructor TDisassembleThread.Create(AExecFile: TExecutableFile);
begin
  inherited Create(true);
  fExecFile:= AExecFile;
end;



procedure TDisassembleThread.Execute;
begin
  try
    fExecFile.Disassemble;
  except
    on EUserTerminatedProcess do
      ProgressData.ErrorStatus:= errUserTerminated;
    on Exception do
      ProgressData.ErrorStatus:= errUnspecified;
  end;
  ProgressData.Finished:= true;
end;


{ TDisassemblePartThread }


constructor TDisassemblePartThread.Create(ACodeSection: TCodeSection; AOptions: TDisassembleOptions);
begin
  inherited Create(true);
  fCodeSection:= ACodeSection;
  fOptions:= AOptions;
end;



procedure TDisassemblePartThread.Execute;
begin
  try
    fCodeSection.DisassemblePart(fOptions);
  except
    on EUserTerminatedProcess do
      ProgressData.ErrorStatus:= errUserTerminated;
    on Exception do
      ProgressData.ErrorStatus:= errUnspecified;
  end;
  ProgressData.Finished:= true;
end;


{ TSaveThread }


constructor TSaveThread.Create(Manager: TExecFileManager; AExecFile: TExecutableFile; AFileName: string; ASaveOptions: TSaveOptions);
begin
  inherited Create(true);
  fManager:= Manager;
  fExecFile:= AExecFile;
  fFileName:= AFileName;
  fSaveOptions:= ASaveOptions;
end;



procedure TSaveThread.Execute;
begin
  try
    fManager.SaveExecFileToFile(fExecFile, fFileName, fSaveOptions);
  except
    on EUserTerminatedProcess do
      ProgressData.ErrorStatus:= errUserTerminated;
    on Exception do
      ProgressData.ErrorStatus:= errUnspecified;
  end;
  ProgressData.Finished:= true;
end;


{ TLoadThread }


constructor TLoadThread.Create(Manager: TExecFileManager; AFileName: string);
begin
  inherited Create(true);
  fManager:= Manager;
  fFileName:= AFileName;
end;



procedure TLoadThread.Execute;
begin
  try
    ProgressData.Result:= Pointer(fManager.LoadExecFileFromFile(fFileName));
  except
    on EUserTerminatedProcess do
      ProgressData.ErrorStatus:= errUserTerminated;
    on Exception do
      ProgressData.ErrorStatus:= errUnspecified;
  end;
  ProgressData.Finished:= true;
end;



end.
