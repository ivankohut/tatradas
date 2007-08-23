unit ProgressFormUnit;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls,

  procmat,
  ProgressThreads;

type

  TProgressForm = class(TForm)
    CancelButton: TButton;
    ProgressBar1: TProgressBar;
    ProgressLabel: TLabel;
    PauseButton: TButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure PauseButtonClick(Sender: TObject);
  private
    fThread: TThread;
  public
//    function Execute(AThread: TProgressThread): boolean;
    procedure Execute(AThread: TThread);
  end;

var
  ProgressForm: TProgressForm;


implementation


uses
 MainFormUnit;

{$R *.dfm}

procedure TProgressForm.Execute(AThread: TThread);
begin
  fThread:= AThread;
  MainForm.Enabled:= false;
  Show;

  ProgressData.Finished:= false;
  ProgressData.ErrorStatus:= errNone;
  ProgressData.Maximum:= 0;
  ProgressData.Position:= 0;
  ProgressData.Name:= '';
  fThread.Resume;
  while not ProgressData.Finished do begin
    Application.ProcessMessages;
    ProgressBar1.Max:= ProgressData.Maximum;
    ProgressBar1.Position:= ProgressData.Position;
    ProgressLabel.Caption:= ProgressData.Name;
    Sleep(100);
  end;
  Close;
  MainForm.Enabled:= true;
end;



procedure TProgressForm.PauseButtonClick(Sender: TObject);
begin
  if fThread.Suspended then begin
    PauseButton.Caption:= 'Pause';
    fThread.Resume;
  end
  else begin
    PauseButton.Caption:= 'Resume';
    fThread.Suspend;
  end;
end;



procedure TProgressForm.CancelButtonClick(Sender: TObject);
begin
  ProgressData.ErrorStatus:= errUserTerminated;
  ProgressData.Finished:= true;
end;


end.
