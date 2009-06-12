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
  TProgressThread = class(TThread)
  protected
    fSuccess: Boolean;
  public
    constructor Create;
  end;


  TDisassembleThread = class(TProgressThread)
  private
    fExecFile: TExecutableFile;
  protected
    procedure Execute; override;
  public
    constructor Create(AExecFile: TExecutableFile);
  end;


  TDisassemblePartThread = class(TProgressThread)
  private
    fCodeSection: TCodeSection;
    fOptions: TDisassembleOptions;
  protected
    procedure Execute; override;
  public
    constructor Create(ACodeSection: TCodeSection; AOptions: TDisassembleOptions);
  end;


  TSaveThread = class(TProgressThread)
  private
    fManager: TExecFileManager;
    fExecFile: TExecutableFile;
    fFileName: string;
  protected
    procedure Execute; override;
  public
    constructor Create(Manager: TExecFileManager; AExecFile: TExecutableFile; AFileName: string);
  end;


  TExportThread = class(TProgressThread)
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


  TLoadThread = class(TProgressThread)
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


{ TProgressThread }


constructor TProgressThread.Create;
begin
  inherited Create(True);
  fSuccess := False;
end;


{ TDisassembleThread }


constructor TDisassembleThread.Create(AExecFile: TExecutableFile);
begin
  inherited Create;
  fExecFile := AExecFile;
end;



procedure TDisassembleThread.Execute;
begin
  try
    fExecFile.Disassemble;
    fSuccess := True;
  except
    on EUserTerminatedProcess do
      ProgressData.ErrorStatus := errUserTerminated;
    on Exception do
      ProgressData.ErrorStatus := errUnspecified;
  end;
  ProgressManager.Finish(fSuccess);
end;


{ TDisassemblePartThread }


constructor TDisassemblePartThread.Create(ACodeSection: TCodeSection; AOptions: TDisassembleOptions);
begin
  inherited Create;
  fCodeSection := ACodeSection;
  fOptions := AOptions;
end;



procedure TDisassemblePartThread.Execute;
begin
  try
    fCodeSection.DisassemblePart(fOptions);
    fSuccess := True;
  except
    on EUserTerminatedProcess do
      ProgressData.ErrorStatus := errUserTerminated;
    on Exception do
      ProgressData.ErrorStatus := errUnspecified;
  end;
  ProgressManager.Finish(fSuccess);
end;


{ TSaveThread }


constructor TSaveThread.Create(Manager: TExecFileManager; AExecFile: TExecutableFile; AFileName: string);
begin
  inherited Create;
  fManager := Manager;
  fExecFile := AExecFile;
  fFileName := AFileName;
end;



procedure TSaveThread.Execute;
begin
  try
    fManager.SaveExecFileToFile(fExecFile, fFileName);
    fSuccess := True;
  except
    on EUserTerminatedProcess do
      ProgressData.ErrorStatus := errUserTerminated;
    on Exception do
      ProgressData.ErrorStatus := errUnspecified;
  end;
  ProgressManager.Finish(fSuccess);
end;


{ TExportThread }


constructor TExportThread.Create(Manager: TExecFileManager; AExecFile: TExecutableFile; AFileName: string; AExportOption: TExportOption; AExportCustomDASOptions: TExportCustomDASOptions);
begin
  inherited Create;
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
    TExporter.ExportToFile(fExportOption, fExportCustomDASOptions, fExecFile, fFileName);
    fSuccess := True;
  except
    on EUserTerminatedProcess do
      ProgressData.ErrorStatus := errUserTerminated;
    on Exception do
      ProgressData.ErrorStatus := errUnspecified;
  end;
  ProgressManager.Finish(fSuccess);
end;


{ TLoadThread }


constructor TLoadThread.Create(Manager: TExecFileManager; AFileName: string);
begin
  inherited Create;
  fManager := Manager;
  fFileName := AFileName;
end;



procedure TLoadThread.Execute;
begin
  try
    ProgressData.Result := Pointer(fManager.LoadExecFileFromFile(fFileName));
    fSuccess := True;
  except
    on EUserTerminatedProcess do
      ProgressData.ErrorStatus := errUserTerminated;
    on Exception do
      ProgressData.ErrorStatus := errUnspecified;
  end;
  ProgressManager.Finish(fSuccess);
end;



end.
