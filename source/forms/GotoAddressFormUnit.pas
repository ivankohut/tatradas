unit GotoAddressFormUnit;

interface

uses
{$IFDEF MSWINDOWS}
    Forms, StdCtrls, Controls, ExtCtrls, Dialogs,
{$ENDIF}
{$IFDEF LINUX}
    QForms, QStdCtrls, QControls, QExtCtrls, QDialogs,
{$ENDIF}
    SysUtils,
    Classes,
    INIFiles,

    {PBBinHexEdit, }

    procmat,
    StringRes,
    myedits;

type
  TGoToAddressForm = class(TForm, ITranslatable)
    OKButton: TButton;
    CancelButton: TButton;
    GotoAddressLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure SetMaxAddress(MaxAddress: cardinal);
    function GetAddress: cardinal;
  public
    GotoAddressEdit: THexPositiveEdit;
    procedure Translate;
    property MaxAddress: cardinal write SetMaxAddress;
    property Address: cardinal read GetAddress;
  end;

var
  GoToAddressForm: TGoToAddressForm;

implementation

uses MainFormUnit, TranslatorUnit, OptionsFormUnit;

{$R *.dfm}

procedure TGoToAddressForm.SetMaxAddress(MaxAddress: cardinal);
begin
  GotoAddressEdit.MaxValue := MaxAddress;
end;



function TGoToAddressForm.GetAddress: cardinal;
begin
  result := GotoAddressEdit.AsCardinal;
end;



procedure TGoToAddressForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;



procedure TGoToAddressForm.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;



procedure TGoToAddressForm.FormCreate(Sender: TObject);
begin
  GotoAddressEdit := THexPositiveEdit.Create(self);
  GotoAddressEdit.Parent := self;
  GotoAddressEdit.Left := 110;
  GotoAddressEdit.Top := 16;
  GotoAddressEdit.Width := 121;
  GotoAddressEdit.Height := 21;
end;



procedure TGoToAddressForm.FormActivate(Sender: TObject);
begin
  GotoAddressEdit.SetFocus;
end;



procedure TGoToAddressForm.Translate;
begin
  Caption := Translator.TranslateControl('GotoAddressForm', 'Caption');
  GotoAddressLabel.Caption := Translator.TranslateControl('GotoAddressForm', 'GotoAddressLabel');
  CancelButton.Caption := Translator.TranslateControl('Common', 'CancelButton');
end;



procedure TGoToAddressForm.FormDestroy(Sender: TObject);
begin
  GotoAddressEdit.Free;
end;



end.
