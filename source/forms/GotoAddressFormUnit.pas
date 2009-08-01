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

    procmat,
    StringRes,
    myedits;

type
  TGoToAddressForm = class(TForm, ITranslatable)
    OKButton: TButton;
    CancelButton: TButton;
    GotoAddressLabel: TLabel;
    PlaceForEditPanel: TPanel;
    procedure CancelButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fMinAddress: Cardinal;
    GotoAddressEdit: THexPositiveEdit;
    procedure SetMaxAddress(MaxAddress: cardinal);
    function GetAddress: cardinal;
    procedure RefreshOKButtonState;
    procedure GotoAddressEditChange(Sender: TObject);
  public
    procedure Translate;
    property MaxAddress: Cardinal write SetMaxAddress;
    property MinAddress: Cardinal write fMinAddress;
    property Address: Cardinal read GetAddress;
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
  Result := GotoAddressEdit.AsCardinal;
end;



procedure TGoToAddressForm.RefreshOKButtonState;
begin
  OKButton.Enabled := (GotoAddressEdit.AsCardinal >= fMinAddress);
end;



procedure TGoToAddressForm.GotoAddressEditChange(Sender: TObject);
begin
  RefreshOKButtonState;
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
  GotoAddressEdit.Parent := PlaceForEditPanel;
  GotoAddressEdit.Align := alClient;
  GotoAddressEdit.OnChange := GotoAddressEditChange;
end;



procedure TGoToAddressForm.FormActivate(Sender: TObject);
begin
  GotoAddressEdit.SetFocus;
  RefreshOKButtonState;
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
