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
    PauseStr: string;
    ResumeStr: string;
    fCurrentPhaseName: string;
  public
    procedure Execute(AThread: TProgressThread);
    procedure Translate;
  end;

var
  ProgressForm: TProgressForm;


implementation


uses
  MainFormUnit, ProgressManagerUnit;

{$R *.dfm}


procedure GuiShowProgress(APhase: string; AProgress: Double);
begin
  Application.ProcessMessages;
  ProgressForm.ProgressLabel.Caption := APhase;
  ProgressForm.ProgressBar1.Position := Round(1000 * AProgress);
  Application.ProcessMessages;
end;



procedure TProgressForm.Execute(AThread: TProgressThread);
begin
  fThread := AThread;
  MainForm.Enabled := False;

  fCurrentPhaseName := '';

  // Reset ProgressForm components
  ProgressLabel.Caption := '';
  ProgressBar1.Position := 0;
  Show;
  Application.ProcessMessages;

  // Reset ProgressData
  ProgressData.ErrorStatus := errNone;

  ProgressManager := TProgressManager.Create(GuiShowProgress);
  try
    ProgressManager.StartProgress(fThread);
  finally
    FreeAndNil(ProgressManager);
  end;

  Close;
  MainForm.Enabled := True;
  MainForm.SetFocus;
end;



procedure TProgressForm.PauseButtonClick(Sender: TObject);
begin
  if fThread.Suspended then begin
    PauseButton.Caption := PauseStr;
    fThread.Resume;
  end
  else begin
    PauseButton.Caption := ResumeStr;
    fThread.Suspend;
  end;
end;



procedure TProgressForm.CancelButtonClick(Sender: TObject);
begin
  ProgressData.ErrorStatus := errUserTerminated;
  ProgressManager.Finish(False);
  if fThread.Suspended then
    PauseButtonClick(self);
end;



procedure TProgressForm.Translate;
begin
  Caption := Translator.TranslateControl('ProgressForm', 'Caption');
  PauseStr := Translator.TranslateControl('ProgressForm', 'Pause');
  ResumeStr := Translator.TranslateControl('ProgressForm', 'Resume');
  CancelButton.Caption := Translator.TranslateControl('Common', 'CancelButton');
  // Set PauseButton caption (PauseButton is in pause state during translating)
  PauseButton.Caption := PauseStr;
end;



end.
