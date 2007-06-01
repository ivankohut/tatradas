unit InsertCommentFormUnit;

interface

uses
{$IFDEF MSWINDOWS}
  Controls, Forms, Dialogs, 
{$ENDIF}
{$IFDEF LINUX}
  QControls, QForms, QStdCtrls,
{$ENDIF}
  SysUtils, Classes, INIFiles, StdCtrls;

type
  TInsertCommentForm = class(TForm)
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
    procedure Translate(ini: TMemINIFile; error: string);
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

procedure TInsertCommentForm.Translate(ini: TMemINIFile; error: string);
begin
  Caption:=ini.ReadString('InsertCommentForm','Caption',error);
  InsertCommentLabel.Caption:=ini.ReadString('InsertCommentForm','InsertCommentLabel',error);
  CancelButton.Caption:=ini.ReadString('Common','CancelButton',error);
end;

end.
