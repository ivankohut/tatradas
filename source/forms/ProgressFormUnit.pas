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
  TranslatorUnit,
  ProgressThreads;

type

  TProgressForm = class(TForm, ITranslatable)
    CancelButton: TButton;
    ProgressBar1: TProgressBar;
    ProgressLabel: TLabel;
    PauseButton: TButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure PauseButtonClick(Sender: TObject);
  private
    fThread: TThread;
  public
    procedure Execute(AThread: TThread);
    procedure Translate;
  end;

var
  ProgressForm: TProgressForm;


implementation


uses
 MainFormUnit;

{$R *.dfm}

var
  PauseStr: string;
  ResumeStr: string;


procedure TProgressForm.Execute(AThread: TThread);
begin
  fThread:= AThread;
  MainForm.Enabled:= false;

  // Reset ProgressForm components
  ProgressLabel.Caption:= '';
  ProgressBar1.Position:= 0;
  Show;
  Application.ProcessMessages;

  // Reset ProgressData
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
  MainForm.SetFocus;
end;



procedure TProgressForm.PauseButtonClick(Sender: TObject);
begin
  if fThread.Suspended then begin
    PauseButton.Caption:= PauseStr;
    fThread.Resume;
  end
  else begin
    PauseButton.Caption:= ResumeStr;
    fThread.Suspend;
  end;
end;



procedure TProgressForm.CancelButtonClick(Sender: TObject);
begin
  ProgressData.ErrorStatus:= errUserTerminated;
  ProgressData.Finished:= true;
  if fThread.Suspended then
    PauseButtonClick(self);
end;



procedure TProgressForm.Translate;
begin
  Caption:= Translator.TranslateControl('ProgressForm', 'Caption');
  PauseStr:= Translator.TranslateControl('ProgressForm', 'Pause');
  ResumeStr:= Translator.TranslateControl('ProgressForm', 'Resume');
  CancelButton.Caption:= Translator.TranslateControl('Common', 'CancelButton');
  // Set PauseButton caption (PauseButton is in pause state during translating)
  PauseButton.Caption:= PauseStr;
end;


end.
