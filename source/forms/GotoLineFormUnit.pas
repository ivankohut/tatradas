unit GotoLineFormUnit;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  // project units
  procmat,
  StringRes,
  Translatables,
  myedits;

type

  { TGotoLineForm }

  TGotoLineForm = class(TForm, ITranslatable)
    GotoLineLabel: TLabel;
    CancelButton: TButton;
    OKButton: TButton;
    PanelForEdit: TPanel;
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    GotoLineEdit: TDecimalPositiveEdit;
    procedure SetMaxLineIndex(MaxLineIndex: Cardinal);
    function GetLineIndex: Cardinal;
  public
    function Translatable: TTranslatable;
    property MaxLineIndex: Cardinal write SetMaxLineIndex;
    property LineIndex: Cardinal read GetLineIndex;
  end;

var
  GotoLineForm: TGotoLineForm;

implementation

{$R *.lfm}

procedure TGotoLineForm.SetMaxLineIndex(MaxLineIndex: Cardinal);
begin
  GotoLineEdit.MaxValue := MaxLineIndex;
end;



function TGotoLineForm.GetLineIndex: Cardinal;
begin
  Result := GotoLineEdit.AsCardinal;
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
  GotoLineEdit.SelectAll;
end;



procedure TGotoLineForm.FormDestroy(Sender: TObject);
begin
  GotoLineEdit.Free;
end;



function TGotoLineForm.Translatable: TTranslatable;
begin
  Result := TTranslatableSimpleForm.Create(self, 'GotoLineForm', [TTranslatableCaption.Create(GotoLineLabel)], [CancelButton]);
end;



procedure TGotoLineForm.FormCreate(Sender: TObject);
begin
  GotoLineEdit := TDecimalPositiveEdit.Create(self);
  GotoLineEdit.Parent := PanelForEdit;
  GotoLineEdit.Align := alClient;
end;



end.
