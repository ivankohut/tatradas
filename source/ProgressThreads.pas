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
  protected
    procedure Execute; override;
  public
    constructor Create(Manager: TExecFileManager; AExecFile: TExecutableFile; AFileName: string);
  end;


  TExportThread = class(TThread)
  private
    fManager: TExecFileManager;
    fExecFile: TExecutableFile;
    fFileName: string;
    fExportOption: TExportOption;
    fExportCustomDASOptions: TExportCustomDASOptions;
  protected
    procedure Execute; override;
  public
    constructor Create(Manager: TExecFileManager; AExecFile: TExecutableFile; AFileName: string; AExportOption: TExportOption; AExportCustomDASOptions: TExportCustomDASOptions);
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

uses Exporters;


{ TDisassembleThread }


constructor TDisassembleThread.Create(AExecFile: TExecutableFile);
begin
  inherited Create(True);
  fExecFile := AExecFile;
end;



procedure TDisassembleThread.Execute;
begin
  try
    fExecFile.Disassemble;
  except
    on EUserTerminatedProcess do
      ProgressData.ErrorStatus := errUserTerminated;
    on Exception do
      ProgressData.ErrorStatus := errUnspecified;
  end;
  ProgressData.Finished := True;
end;


{ TDisassemblePartThread }


constructor TDisassemblePartThread.Create(ACodeSection: TCodeSection; AOptions: TDisassembleOptions);
begin
  inherited Create(True);
  fCodeSection := ACodeSection;
  fOptions := AOptions;
end;



procedure TDisassemblePartThread.Execute;
begin
  try
    fCodeSection.DisassemblePart(fOptions);
  except
    on EUserTerminatedProcess do
      ProgressData.ErrorStatus := errUserTerminated;
    on Exception do
      ProgressData.ErrorStatus := errUnspecified;
  end;
  ProgressData.Finished := True;
end;


{ TSaveThread }


constructor TSaveThread.Create(Manager: TExecFileManager; AExecFile: TExecutableFile; AFileName: string);
begin
  inherited Create(True);
  fManager := Manager;
  fExecFile := AExecFile;
  fFileName := AFileName;
end;



procedure TSaveThread.Execute;
begin
  try
    fManager.SaveExecFileToFile(fExecFile, fFileName);
  except
    on EUserTerminatedProcess do
      ProgressData.ErrorStatus := errUserTerminated;
    on Exception do
      ProgressData.ErrorStatus := errUnspecified;
  end;
  ProgressData.Finished := True;
end;


{ TExportThread }


constructor TExportThread.Create(Manager: TExecFileManager; AExecFile: TExecutableFile; AFileName: string; AExportOption: TExportOption; AExportCustomDASOptions: TExportCustomDASOptions);
begin
  inherited Create(True);
  fManager := Manager;
  fExecFile := AExecFile;
  fFileName := AFileName;
  fExportOption := AExportOption;
  fExportCustomDASOptions := AExportCustomDASOptions;
end;



procedure TExportThread.Execute;
begin
  inherited;
  try
    ExportToFile(fExportOption, fExportCustomDASOptions, fExecFile, fFileName);
  except
    on EUserTerminatedProcess do
      ProgressData.ErrorStatus := errUserTerminated;
    on Exception do
      ProgressData.ErrorStatus := errUnspecified;
  end;
  ProgressData.Finished := True;
end;


{ TLoadThread }


constructor TLoadThread.Create(Manager: TExecFileManager; AFileName: string);
begin
  inherited Create(True);
  fManager := Manager;
  fFileName := AFileName;
end;



procedure TLoadThread.Execute;
begin
  try
    ProgressData.Result := Pointer(fManager.LoadExecFileFromFile(fFileName));
  except
    on EUserTerminatedProcess do
      ProgressData.ErrorStatus := errUserTerminated;
    on Exception do
      ProgressData.ErrorStatus := errUnspecified;
  end;
  ProgressData.Finished := True;
end;



end.
