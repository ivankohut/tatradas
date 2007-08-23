unit InsertCommentFormUnit;

interface

uses
{$IFDEF MSWINDOWS}
  Controls, Forms, Dialogs, StdCtrls,
{$ENDIF}
{$IFDEF LINUX}
  QControls, QForms, QStdCtrls,
{$ENDIF}
  SysUtils,
  Classes,
  INIFiles,

  procmat,
  TatraDASFormUnit;

type
  TInsertCommentForm = class(TTatraDASForm)
    InsertCommentEdit: TEdit;
    InsertCommentLabel: TLabel;
    OKButton: TButton;
    CancelButton: TButton;
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Translate(ini: TMemINIFile); override;
  end;

var
  InsertCommentForm: TInsertCommentForm;

implementation

{$R *.dfm}

procedure TInsertCommentForm.OKButtonClick(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

procedure TInsertCommentForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TInsertCommentForm.FormActivate(Sender: TObject);
begin
  InsertCommentEdit.SetFocus;
end;

procedure TInsertCommentForm.Translate(ini: TMemINIFile);
begin
  Caption:=ini.ReadString('InsertCommentForm','Caption',TranslateErrorStr);
  InsertCommentLabel.Caption:=ini.ReadString('InsertCommentForm','InsertCommentLabel',TranslateErrorStr);
  CancelButton.Caption:=ini.ReadString('Common','CancelButton',TranslateErrorStr);
end;

end.
