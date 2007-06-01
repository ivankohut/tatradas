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

  procmat
  ;//, UProgressThread;

type
  TProgressForm = class(TForm)
    CancelButton: TButton;
    ProgressBar1: TProgressBar;
    ProgressLabel: TLabel;
    PauseButton: TButton;
    procedure CancelButtonClick(Sender: TObject);
//    procedure Show; override;
  private
    { Private declarations }
  public
//    vlakno: TProgressThread;
    { Public declarations }
    procedure Execute;
  end;

var
  ProgressForm: TProgressForm;

implementation

//procedure Show; override;


{$R *.dfm}

procedure TProgressForm.CancelButtonClick(Sender: TObject);
begin
//  vlakno.Terminate;
end;

procedure TProgressForm.Execute;
begin
  ProgressBar1.Position;
  Show;
  while not ProgressFinished do begin
    Application.ProcessMessages;
    ProgressBar1.Position:=ProgressPosition;
    ProgressLabel.Caption:=ProgressName;
    Sleep(10);
  end;
  Close;
end;

end.
