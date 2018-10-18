unit InsertCommentFormUnit;

interface

uses
  Controls, Forms, Dialogs, StdCtrls,
  SysUtils,
  Classes,
  IniFiles,
  // project units
  procmat,
  Translatables;

type
  TInsertCommentForm = class(TForm, ITranslatable)
    InsertCommentEdit: TEdit;
    InsertCommentLabel: TLabel;
    OKButton: TButton;
    CancelButton: TButton;
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  public
    function Translatable: TTranslatable;
  end;

var
  InsertCommentForm: TInsertCommentForm;


implementation


{$R *.lfm}


procedure TInsertCommentForm.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;



procedure TInsertCommentForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;



procedure TInsertCommentForm.FormActivate(Sender: TObject);
begin
  InsertCommentEdit.SetFocus;
end;



function TInsertCommentForm.Translatable: TTranslatable;
begin
  Result := TTranslatableSimpleForm.Create(self, 'InsertCommentForm', [TTranslatableCaption.Create(InsertCommentLabel)], [CancelButton]);
end;



end.
