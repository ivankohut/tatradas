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
  // project units
  procmat,
  Translatables,
  ProgressThreads;

type

  { TProgressForm }

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
    function Translatable: TTranslatable;
  end;

var
  ProgressForm: TProgressForm;


implementation


uses
  MainFormUnit, ProgressManagerUnit, GlobalsUnit;

{$R *.lfm}


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
  ProgressData.AbortExecution := False;

  ProgressManager := TProgressManager.Create(GuiShowProgress);
  try
    ProgressManager.StartProgress(fThread);
  finally
    FreeAndNil(ProgressManager);
    Close;
    MainForm.Enabled := True;
    MainForm.SetFocus;
  end;
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
  ProgressData.AbortExecution := True;
  ProgressManager.Finish(False);
  if fThread.Suspended then
    PauseButtonClick(self);
end;



function TProgressForm.Translatable: TTranslatable;
begin
  Result := TTranslatableSimpleForm.Create(
    self,
    'ProgressForm',
    [
      TTranslatableString.Create(PauseStr, 'Pause'),
      TTranslatableString.Create(ResumeStr, 'Resume'),
      // PauseButton is in pause state during translating
      TTranslatableCaption.Create(PauseButton, 'Pause')
    ],
    [CancelButton]
  );
end;



end.
