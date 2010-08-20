unit InsertCommentFormUnit;

interface

uses
  Controls, Forms, Dialogs, StdCtrls,
  SysUtils,
  Classes,
  INIFiles,

  LResources,
  procmat,
  TranslatorUnit;

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
    procedure Translate;
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



procedure TInsertCommentForm.Translate;
begin
  Caption := Translator.TranslateControl('InsertCommentForm', 'Caption');
  InsertCommentLabel.Caption := Translator.TranslateControl('InsertCommentForm', 'InsertCommentLabel');
  CancelButton.Caption := Translator.TranslateControl('Common', 'CancelButton');
end;



end.
