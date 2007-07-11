unit UProgressThread;

{$INCLUDE 'delver.inc'}

interface

uses
  SysUtils,
  Classes,

  SectionUnit,
  procmat,
  ExecFileUnit;

type

  TMyMethod = function (): boolean of object;


  TProgressThread = class(TThread)
    ProgressPosition: cardinal;
    Finished: Boolean;
    constructor Create(ProgressText: string; ProgressMax: cardinal);
   private
    fProgressText: string;
    fProgressMax: cardinal;
    procedure SetUpUI;
   end;


  TLoadFromFileThread = class(TProgressThread)
   protected
    procedure Execute; override;
   public
    DASFile: ^TextFile;
    Disassembled: TStrings;
//    constructor Create(ProgressText: string; ProgressMax: cardinal);
  end;

  TMyThread = class(TThread)
   private
     fMethod: TMyMethod;
   protected
    procedure Execute; override;
   public
    constructor Create(aMethod: TMyMethod);
//    constructor Create(ProgressText: string; ProgressMax: cardinal);
  end;


  TExecFileManager_LoadExecFileFromFile = class(TThread)
   private
     fParams_FileName: TFileName;
     fResult: TExecutableFile;
   protected
    procedure Execute; override;
   public
    constructor Create(aFileName: TFileName);
    property TheResult: TExecutableFile read fResult;
  end;


  TSection_LoadFromFile = class(TThread)
   private
     fSection: TSection;
     fParams_DASStream: TTextFileStream;
     fParams_DHFStream: TFileStream;
     fResult: boolean;
   protected
    procedure Execute; override;
   public
    constructor Create(DHF: TFileStream; DAS: TTextFileStream);
    property TheResult: boolean read fResult;
  end;


implementation

uses
{$IFDEF GUI_B}
  ProgressFormUnit,
  MainFormUnit,
{$ENDIF}
  ExecFileManagerUnit;


procedure TProgressThread.SetUpUI;
begin
{$IFDEF GUI_B}
  ProgressForm.ProgressBar1.Position:=0;
  ProgressForm.ProgressBar1.Max:=fProgressMax;
  ProgressForm.ProgressLabel.Caption:='testik_a';
  ProgressForm.Show;
{$ENDIF}
end;


constructor TProgressThread.Create(ProgressText: string; ProgressMax: cardinal);
begin
  inherited Create(true);
  fProgressText:=ProgressText;
  fProgressMax:=ProgressMax;
  ProgressPosition:=0;
  Finished:=false;
{$IFDEF GUI_B}
  Synchronize(SetUpUI);
{$ELSE}

{$ENDIF}


end;
{
constructor TLoadFromFileThread.Create(ProgressText: string; ProgressMax: cardinal);
begin
  inherited Create(true);
end;
}
procedure TLoadFromFileThread.Execute;
var line: string;
begin
  while (not Terminated) and (not EOF(DASFile^)) and (not (ProgressPosition = fProgressMax)) do begin
    Inc(ProgressPosition);
    ReadLn(DASFile^,line);
    Disassembled.Add(line);
  end;
  Finished:=True;
end;

{ TMyThread }

constructor TMyThread.Create(aMethod: TMyMethod);
begin
  fMethod:=aMethod;
end;

procedure TMyThread.Execute;
begin
  fMethod;
end;

{ TExecFileManager_LoadExecFileFromFile }

constructor TExecFileManager_LoadExecFileFromFile.Create(aFileName: TFileName);
begin
  inherited Create(false);
  fParams_FileName:= aFileName;
end;

procedure TExecFileManager_LoadExecFileFromFile.Execute;
begin
// upravit
//  fResult:=ExecFileManager.LoadExecFileFromFile(fParams_FileName);
  ProgressFinished:=true;
end;

{ TSection_LoadFromFile }

constructor TSection_LoadFromFile.Create(DHF: TFileStream; DAS: TTextFileStream);
begin
  inherited Create(false);
  fParams_DHFStream:=DHF;
  fParams_DASStream:=DAS;
end;

procedure TSection_LoadFromFile.Execute;
begin
//  fResult:= fSection.LoadFromFile(fParams_DHFStream, fParams_DASStream);
  ProgressFinished:=true;
end;

end.

