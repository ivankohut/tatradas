unit ProgressThreads;

interface

uses
  Classes,

  procmat,
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


implementation


{ TDisassembleThread }


constructor TDisassembleThread.Create(AExecFile: TExecutableFile);
begin
  inherited Create(true);
  fExecFile:= AExecFile;
end;



procedure TDisassembleThread.Execute;
begin
  fExecFile.Disassemble;
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
  fManager.SaveExecFileToFile(fExecFile, fFileName, fSaveOptions);
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
  ProgressData.Result:= Pointer(fManager.LoadExecFileFromFile(fFileName));
  ProgressData.Finished:= true;
end;



end.
