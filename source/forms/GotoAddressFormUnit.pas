unit GotoAddressFormUnit;

interface

uses
  Forms, StdCtrls, Controls, ExtCtrls, Dialogs,
  SysUtils,
  Classes,
  INIFiles,
  // project units
  procmat,
  myedits,
  Translatables;

type

  { TGoToAddressForm }

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
    procedure SetMaxAddress(MaxAddress: Cardinal);
    function GetAddress: Cardinal;
    procedure RefreshOKButtonState;
    procedure GotoAddressEditChange(Sender: TObject);
  public
    function Translatable: TTranslatable;
    property MaxAddress: Cardinal write SetMaxAddress;
    property MinAddress: Cardinal write fMinAddress;
    property Address: Cardinal read GetAddress;
  end;

var
  GoToAddressForm: TGoToAddressForm;

implementation

{$R *.lfm}

procedure TGoToAddressForm.SetMaxAddress(MaxAddress: Cardinal);
begin
  GotoAddressEdit.MaxValue := MaxAddress;
end;



function TGoToAddressForm.GetAddress: Cardinal;
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



function TGoToAddressForm.Translatable: TTranslatable;
begin
  Result := TTranslatableSimpleForm.Create(self, 'GotoAddressForm', [TTranslatableCaption.Create(GotoAddressLabel)], [CancelButton])
end;



procedure TGoToAddressForm.FormDestroy(Sender: TObject);
begin
  GotoAddressEdit.Free;
end;



end.
