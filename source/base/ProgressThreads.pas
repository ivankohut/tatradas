unit ProgressThreads;

interface

uses
  Classes,
  SysUtils,
  // project units
  procmat,
  DisassemblerTypes,
  CodeSectionUnit,
  ExecFileManagerUnit,
  ExecFileUnit;

type
  TProgressThread = class(TThread)
  private
    fExceptionClass: ExceptClass;
    fExceptionMessage: string;
  protected
    procedure DoExecute; virtual; abstract;
    procedure Execute; override;
  public
    constructor Create;
    function WasException: Boolean;
    property ExceptionClass: ExceptClass read fExceptionClass;
    property ExceptionMessage: string read fExceptionMessage;
  end;


  TDisassembleThread = class(TProgressThread)
  private
    fExecFile: TExecutableFile;
  protected
    procedure DoExecute; override;
  public
    constructor Create(AExecFile: TExecutableFile);
  end;


  TDisassemblePartThread = class(TProgressThread)
  private
    fCodeSection: TCodeSection;
    fOptions: TDisassembleOptions;
  protected
    procedure DoExecute; override;
  public
    constructor Create(ACodeSection: TCodeSection; AOptions: TDisassembleOptions);
  end;


  TSaveThread = class(TProgressThread)
  private
    fManager: TExecFileManager;
    fExecFile: TExecutableFile;
    fFileName: string;
  protected
    procedure DoExecute; override;
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
    procedure DoExecute; override;
  public
    constructor Create(Manager: TExecFileManager; AExecFile: TExecutableFile; AFileName: string; AExportOption: TExportOption; AExportCustomDASOptions: TExportCustomDASOptions);
  end;


  TLoadThread = class(TProgressThread)
  private
    fManager: TExecFileManager;
    fFileName: string;
  protected
    procedure DoExecute; override;
  public
    constructor Create(Manager: TExecFileManager; AFileName: string);
  end;


implementation

uses
  GlobalsUnit,
  Exporters;


{ TProgressThread }


constructor TProgressThread.Create;
begin
  inherited Create(True);
end;



procedure TProgressThread.Execute;
var
  Success: Boolean;
begin
  Success := False;
  try
    DoExecute;
    Success := True;
  except
    on E: Exception do begin
      fExceptionClass := ExceptClass(E.ClassType);
      fExceptionMessage := E.Message;
    end;
  end;
  ProgressManager.Finish(Success);
end;



function TProgressThread.WasException: Boolean;
begin
  Result := fExceptionClass <> nil;
end;


{ TDisassembleThread }


constructor TDisassembleThread.Create(AExecFile: TExecutableFile);
begin
  inherited Create;
  fExecFile := AExecFile;
end;



procedure TDisassembleThread.DoExecute;
begin
  fExecFile.Disassemble;
end;


{ TDisassemblePartThread }


constructor TDisassemblePartThread.Create(ACodeSection: TCodeSection; AOptions: TDisassembleOptions);
begin
  inherited Create;
  fCodeSection := ACodeSection;
  fOptions := AOptions;
end;



procedure TDisassemblePartThread.DoExecute;
begin
  fCodeSection.DisassemblePart(fOptions);
end;


{ TSaveThread }


constructor TSaveThread.Create(Manager: TExecFileManager; AExecFile: TExecutableFile; AFileName: string);
begin
  inherited Create;
  fManager := Manager;
  fExecFile := AExecFile;
  fFileName := AFileName;
end;



procedure TSaveThread.DoExecute;
begin
  fManager.SaveExecFileToFile(fExecFile, fFileName);
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



procedure TExportThread.DoExecute;
begin
  TExporter.ExportToFile(fExportOption, fExportCustomDASOptions, fExecFile, fFileName);
end;


{ TLoadThread }


constructor TLoadThread.Create(Manager: TExecFileManager; AFileName: string);
begin
  inherited Create;
  fManager := Manager;
  fFileName := AFileName;
end;



procedure TLoadThread.DoExecute;
begin
  ProgressData.Result := Pointer(fManager.LoadExecFileFromFile(fFileName));
end;



end.
