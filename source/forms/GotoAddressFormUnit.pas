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
    TatraDASFormUnit,
    StringRes,
    myedits;

type
  TGoToAddressForm = class(TTatraDASForm)
    OKButton: TButton;
    CancelButton: TButton;
    GotoAddressLabel: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure SetMaxAddress(address: cardinal);
    function GetAddress: cardinal;
  public
    GotoAddressEdit: TMyHexEdit;
    procedure Translate(ini: TMemINIFile); override;
    property MaxAddress: cardinal write SetMaxAddress;
    property Address: cardinal read GetAddress;
  end;

var
  GoToAddressForm: TGoToAddressForm;

implementation

uses MainFormUnit, Languages, OptionsFormUnit;

{$R *.dfm}

procedure TGoToAddressForm.SetMaxAddress(address: cardinal);
begin
  GotoAddressEdit.MaxValue:=address;
end;

function TGoToAddressForm.GetAddress: cardinal;
begin
  result:=GotoAddressEdit.AsCardinal;
end;

procedure TGoToAddressForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TGoToAddressForm.OKButtonClick(Sender: TObject);
begin
  ModalResult:=mrOK;
end;

procedure TGoToAddressForm.FormCreate(Sender: TObject);
begin
  GotoAddressEdit:=TMyHexEdit.Create(self);
  GotoAddressEdit.Parent:=self;
  GotoAddressEdit.Left:=110;
  GotoAddressEdit.Top:=16;
  GotoAddressEdit.Width:=121;
  GotoAddressEdit.Height:=21;

// Tu sa spusta prelozenie prostredia na zaciatku behu programu
// GotoAddressForm musi vznikat ako posledny z formularov
  if Langs.fCount > 0 then Langs.Translate;

  OptionsForm.LoadSettings(MainForm.sINI);
end;

procedure TGoToAddressForm.FormActivate(Sender: TObject);
begin
  GotoAddressEdit.SetFocus;
end;

procedure TGoToAddressForm.Translate(ini: TMemINIFile);
begin
  Caption:=ini.ReadString('GotoAddressForm','Caption',TranslateErrorStr);
  GotoAddressLabel.Caption:=ini.ReadString('GotoAddressForm','GotoAddressLabel',TranslateErrorStr);
  CancelButton.Caption:=ini.ReadString('Common','CancelButton',TranslateErrorStr);
end;

procedure TGoToAddressForm.FormDestroy(Sender: TObject);
begin
  GotoAddressEdit.Free;
end;

end.
