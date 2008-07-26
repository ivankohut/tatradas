unit GotoLineFormUnit;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  procmat,
  StringRes,
  myedits;


type
  TGotoLineForm = class(TForm, ITranslatable)
    GotoLineLabel: TLabel;
    CancelButton: TButton;
    OKButton: TButton;
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetMaxAddress(MaxAddress: cardinal);
    function GetAddress: cardinal;
  public
    GotoLineEdit: TMyNumEdit;
    procedure Translate;
    property MaxAddress: cardinal write SetMaxAddress;
    property Address: cardinal read GetAddress;
  end;

var
  GotoLineForm: TGotoLineForm;

implementation

uses
  TranslatorUnit; 

{$R *.dfm}


procedure TGotoLineForm.SetMaxAddress(MaxAddress: cardinal);
begin
  GotoLineEdit.MaxValue := MaxAddress;
end;



function TGotoLineForm.GetAddress: cardinal;
begin
  result := GotoLineEdit.AsCardinal;
end;



procedure TGotoLineForm.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;



procedure TGotoLineForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;



procedure TGotoLineForm.FormActivate(Sender: TObject);
begin
  GotoLineEdit.SetFocus;
end;



procedure TGotoLineForm.FormDestroy(Sender: TObject);
begin
  GotoLineEdit.Free;
end;



procedure TGotoLineForm.Translate;
begin
  Caption := Translator.TranslateControl('GotoLineForm', 'Caption');
  GotoLineLabel.Caption := Translator.TranslateControl('GotoLineForm', 'GotoLineLabel');
  CancelButton.Caption := Translator.TranslateControl('Common', 'CancelButton');
end;



procedure TGotoLineForm.FormCreate(Sender: TObject);
begin
  GotoLineEdit := TMyNumEdit.Create(self);
  GotoLineEdit.Parent := self;
  GotoLineEdit.Left := 110;
  GotoLineEdit.Top := 16;
  GotoLineEdit.Width := 121;
  GotoLineEdit.Height := 21;
end;



end.
